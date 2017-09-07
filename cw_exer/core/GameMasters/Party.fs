namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data.Type
open CardWirthEngine.Utils
open CardWirthEngine.Cards

module Party =

  type Adventurers = Cast.t array

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
    { adventurers : Adventurers
    ; money : Money
    ; bag : Goods list
    }

  let inline party_count party =
    Array.length party.adventurers

  let inline average_level party =
    let sum = 
      party.adventurers
      |> Array.sumBy
        (fun (cast : Cast.t) -> cast.property.level) in
    sum / party_count party

  exception InvalidPartyIndexException of int * int

  let inline at index party =
    if index >= party_count party
    then
      raise <| InvalidPartyIndexException (index, party_count party)
    else
      party.adventurers.[index]

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