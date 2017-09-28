namespace CardWirthEngine.GameMasters

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.Data.Type
open CardWirthEngine.Util
open CardWirthEngine.Utils
open CardWirthEngine.Cards
open CardWirthEngine.GameMasters.Cards

module Party =
  type Goods
    = Skill of Skill.t
    | Item of Item.t
    | Beast of Beast.t

  let inline good_equals left right =
    match left, right with
      Skill l, Skill r -> Skill.equals l r
    | Item l, Item r -> Item.equals l r
    | Beast l, Beast r -> Beast.equals l r
    | _ -> false

  type t =
    { adventurers : Adventurers.t
    ; money : Money
    ; bag : Goods list
    ; name : string
    }

  let inline update_adventurers f =
    function
      { adventurers = adventurers } as party ->
        { party with adventurers = f adventurers }

  let inline party_count party =
    Adventurers.length party.adventurers

  let inline average_level party =
    let sum = 
      party.adventurers
      |> Adventurers.fold
           (Adventurers.fc <| fun a c -> a + c.property.level)
           0 in
    sum / party_count party

  let inline update_adventurer pos f =
    update_adventurers <| Adventurers.update pos f

  let inline set_adventurer pos cast =
    update_adventurer pos (const' cast)

  exception InvalidPartyIndexException of int * int

  let inline at index party =
    match Adventurers.get_by_index index party.adventurers with
      Adventurers.Exist cast -> cast
    | Adventurers.Flipped cast -> cast
    | _ -> raise <| InvalidPartyIndexException (index, party_count party)

  let add_goods count good party =
    let goods = List.multi_cons count good party.bag in
    { party with bag = goods }

  let remove_goods remove_count good party =
    let f =
      match remove_count with
        RemoveCount.All
          -> fun f -> List.filter (f >> not)
      | RemoveCount.Count count
          -> List.filter_not_limited count in
    let goods =
      f (fun g -> good_equals g good) party.bag in
    { party with bag = goods }

  (* Card Ops *)
  let inline add count card party =
    let rec bag cards =
      function
        0 -> cards
      | count -> bag (card :: cards) (count - 1) in
    { party with bag = bag party.bag count }

  let inline remove count card party =
    List.filter_not_limited
      count
      begin fun c ->
        match c, card with
          Skill left, Skill right -> Skill.equals left right
        | Item left, Item right -> Item.equals left right
        | Beast left, Beast right -> Beast.equals left right
        | _ -> false end
      party.bag

  let inline count_card card party =
    List.count_by
      begin fun c ->
        match c, card with
          Skill left, Skill right -> Skill.equals left right
        | Item left, Item right -> Item.equals left right
        | Beast left, Beast right -> Beast.equals left right
        | _ -> false end
      party.bag

  (* Coupon Ops *)
  let inline add_coupon pos coupon =
    update_adventurer pos <| Cast.add_coupon coupon
  
  (* 全員にクーポンを与える場合、裏返ったキャストは除外 *)
  let inline add_coupon_all coupon =
    update_adventurers
    <| Adventurers.map (Adventurers.cc' <| Cast.add_coupon coupon)

  let inline remove_coupon pos name =
    update_adventurer pos <| Cast.remove_coupon name

  (* 全員からクーポンを除去する場合、裏返ったキャストも含める *)
  let inline remove_coupon_all name =
    update_adventurers
    <| Adventurers.map (Adventurers.cc <| Cast.remove_coupon name)

  let inline find_coupon_holder name party =
    Adventurers.try_find_with_position
      begin function
        Adventurers.Exist cast -> Cast.has_coupon name cast
      | Adventurers.Flipped cast -> Cast.has_coupon name cast
      end
      party.adventurers

  (* Money Ops *)
  let inline has_money amount =
    function { money = money } -> amount <= money

  let inline add_money amount =
    function
      { money = money } as party ->
        let balance' = money + amount in
        let balance = if balance' < 0 then 0 else balance' in
        { party with money = balance }