namespace CardWirthEngine.Scenario

open CardWirthEngine.Data.Types

module Status =
  type Area
    = Area of AreaId
    | Battle of BattleId
  type t =