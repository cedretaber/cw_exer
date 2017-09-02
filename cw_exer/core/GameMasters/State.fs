namespace CardWirthEngine.GameMasters

open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Data
open CardWirthEngine.Data.Types

module State =
  type Area
    = Area of AreaId
    | Battle of BattleId

  type Event
    = Content of Event.t * Content.t
    | Action

  type State
    = OnEvent of Event list
    | OnField
    | OnBattle

  type t =
    { summary : Info.Summary.t
    ; current_area : Area
    ; flags : (Flag.Name, Flag.State) Map
    ; steps : (Step.Name, Step.State) Map
    ; state : State
    ; selected_adventurer : int
    ; bgm : Bgm
    ; random : System.Random
    }