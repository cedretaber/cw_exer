namespace CardWirthEngine.GameMasters.Cards

open CardWirthEngine.Data.Type
open CardWirthEngine.Cards

module Enemies =
  type t = (EnemyId, Cast.t) Map

  let get = Map.tryFind
  let add = Map.add
  let remove = Map.remove