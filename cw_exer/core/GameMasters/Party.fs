namespace CardWirthEngine.GameMasters

open CardWirthEngine.Cards

module Party =

  type Adventurers = Cast.t array

  type Goods
    = Skills of Skill.t list
    | Items of Item.t list
    | Beasts of Beast.t list

  type t =
    { adventurers : Adventurers
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