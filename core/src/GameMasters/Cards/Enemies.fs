namespace CardWirthEngine.GameMasters.Cards

open CardWirthEngine.Data.Type
open CardWirthEngine.Cards

module Enemies =
  type t = (EnemyId, Cast.t) Map

  let get = Map.tryFind
  let updated = Map.add
  let removed = Map.remove
  let to_list = Map.toList