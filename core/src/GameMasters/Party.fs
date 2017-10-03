namespace CardWirthEngine.GameMasters

open Aether
open Aether.Operators

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
    with
      static member adventurers_ =
        (fun p -> p.adventurers), (fun advs p -> { p with adventurers = advs })
      static member money_ =
        (fun p -> p.money), (fun m p -> { p with money = m })
      static member bag_ =
        (fun p -> p.bag), (fun b p -> { p with bag = b })
      static member name_ =
        (fun p -> p.name), (fun n p -> ({ p with name = n } : t))
  
  let get_adventurers = Optic.get t.adventurers_
  let map_adventurers = Optic.map t.adventurers_

  let party_count =
    get_adventurers >> Adventurers.length

  let inline average_level party =
    let sum = 
      party.adventurers
      |> Adventurers.fold
           (Adventurers.fc <| fun a c -> a + c.property.level)
           0 in
    sum / party_count party

  let inline map_adventurer pos f =
    map_adventurers <| Adventurers.update pos f

  let inline set_adventurer pos cast =
    map_adventurer pos (const' cast)

  exception InvalidPartyIndexException of int * int

  let at index party =
    match Adventurers.get_by_index index party.adventurers with
      Adventurers.Exist cast -> cast
    | Adventurers.Flipped cast -> cast
    | _ -> raise <| InvalidPartyIndexException (index, party_count party)

  let map_bag = Optic.map t.bag_

  let add_goods count good =
    map_bag <| List.multi_cons count good

  let remove_goods remove_count good =
    let f =
      match remove_count with
        RemoveCount.All
          -> fun f -> List.filter (f >> not)
      | RemoveCount.Count count
          -> List.filter_not_limited count in
    map_bag <| f (fun g -> good_equals g good)

  (* Card Ops *)
  let add count card =
    let rec bag cards =
      function
        0 -> cards
      | count -> bag (card :: cards) (count - 1) in
    map_bag (fun b -> bag b count)

  let remove count card =
    map_bag <|
      List.filter_not_limited
        count
        begin fun c ->
          match c, card with
            Skill left, Skill right -> Skill.equals left right
          | Item left, Item right -> Item.equals left right
          | Beast left, Beast right -> Beast.equals left right
          | _ -> false
          end in

  let count_card card party =
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
    map_adventurer pos <| Cast.add_coupon coupon
  
  (* 全員にクーポンを与える場合、裏返ったキャストは除外 *)
  let inline add_coupon_all coupon =
    map_adventurers
    <| Adventurers.map (Adventurers.cc' <| Cast.add_coupon coupon)

  let inline remove_coupon pos name =
    map_adventurer pos <| Cast.remove_coupon name

  (* 全員からクーポンを除去する場合、裏返ったキャストも含める *)
  let inline remove_coupon_all name =
    map_adventurers
    <| Adventurers.map (Adventurers.cc <| Cast.remove_coupon name)

  let find_coupon_holder name party =
    Adventurers.try_find_with_position
      begin function
        Adventurers.Exist cast -> Cast.has_coupon name cast
      | Adventurers.Flipped cast -> Cast.has_coupon name cast
      end
      party.adventurers

  (* Money Ops *)
  let map_money = Optic.map t.money_

  let inline has_money amount =
    function { money = money } -> amount <= money

  let add_money amount =
    map_money <|
      fun money ->
        let balance = money + amount in
        if balance < 0 then 0 else balance