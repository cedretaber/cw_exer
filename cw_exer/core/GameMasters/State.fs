namespace CardWirthEngine.GameMasters

open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Data
open CardWirthEngine.Data.Types

module State =
  type Area
    = Area of AreaId
    | Battle of BattleId
  type State
    = OnEvent of (Event.t * Content.t) list
    | OnField
    | OnBattle

  type t =
    { current_area : Area
    ; flags : (Flag.Name, Flag.State) Map
    ; steps : (Step.Name, Step.State) Map
    ; state : State
    }