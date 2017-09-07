namespace CardWirthEngine.GameMasters

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

  exception InvalidPartyIndexException of int * int

  let inline at index party =
    if index >= party_count party
    then
      raise <| InvalidPartyIndexException (index, party_count party)
    else
      Adventurers.get_by_index index party.adventurers

  let add_goods count good party =
    let goods = ListUtil.multi_cons count good party.bag
    { party with bag = goods }

  let remove_goods count good party =
    let goods =
      ListUtil.filter_limit
        count
        (fun g -> good_equals g good)
        party.bag in
    { party with bag = goods }