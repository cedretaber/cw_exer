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