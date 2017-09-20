namespace CardWirthEngine.GameMasters

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.Data.Type
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

  let inline party_count party =
    Adventurers.length party.adventurers

  let inline average_level party =
    let sum = 
      party.adventurers
      |> Adventurers.to_list
      |> List.sumBy
        (fun (cast : Cast.t) -> cast.property.level) in
    sum / party_count party

  let inline set_adventurer pos cast party =
    let adventurers = Adventurers.updated pos cast party.adventurers in
    { party with adventurers = adventurers }

  exception InvalidPartyIndexException of int * int

  let inline at index party =
    if index >= party_count party
    then
      raise <| InvalidPartyIndexException (index, party_count party)
    else
      Adventurers.get_by_index index party.adventurers

  let add_goods count good party =
    let goods = ListUtil.multi_cons count good party.bag in
    { party with bag = goods }

  let remove_goods remove_count good party =
    let f =
      match remove_count with
        RemoveCount.All
          -> List.filter
      | RemoveCount.Count count
          -> ListUtil.filter_limit count in
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
    ListUtil.filter_limit
      count
      (fun c ->
        match c, card with
          Skill left, Skill right -> Skill.equals left right
        | Item left, Item right -> Item.equals left right
        | Beast left, Beast right -> Beast.equals left right
        | _ -> false)
      party.bag

  let inline count_card card party =
    ListUtil.count_by
      (fun c ->
        match c, card with
          Skill left, Skill right -> Skill.equals left right
        | Item left, Item right -> Item.equals left right
        | Beast left, Beast right -> Beast.equals left right
        | _ -> false)
      party.bag

  let inline has_money amount =
    function
      { money = money } ->
        amount <= money