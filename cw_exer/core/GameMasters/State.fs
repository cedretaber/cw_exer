namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data
open CardWirthEngine.Data.Types
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Cards

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

  type AreaState
    = Area of id : AreaId
    | Battle of id : BattleId

  type Flags = (Flag.Name, Flag.State) Map
  type Steps = (Step.Name, Step.State) Map

  type GlobalState =
    { flags : Flags
    ; steps : Steps
    ; random : System.Random
    }

  type Casts = (CastId, Cast.t) Map
  type Skills = (SkillId, Skill.t) Map
  type Items = (ItemId, Item.t) Map
  type Beasts = (BeastId, Beast.t) Map

  type Cards =
    { casts : Casts
    ; skills : Skills
    ; items : Items
    ; beasts : Beasts
    }

  type t =
    { summary : Info.Summary.t
    ; current_area : Area
    ; global_state : GlobalState
    ; cards : Cards
    ; state : State
    ; selected_adventurer : int
    ; bgm : Bgm
    }
    with
      member this.random max = this.global_state.random.Next max
      member this.random (min, max) = this.global_state.random.Next (min, max)

  (* flag ops *)
  let inline get_flag name state =
    state.global_state.flags |> Map.find name

  let inline set_flag name value state =
    let flags = MapUtil.updated name value state.global_state.flags in
    let global_state = { state.global_state with flags =  flags } in
    { state with global_state = global_state }

  (* step ops *)
  let inline get_step name state =
    state.global_state.steps |> Map.find name

  let inline get_step_length name state =
    let steps = state.summary.steps |> Map.find name in
    steps.steps |> Array.length

  exception InvalidStepIndex

  let inline set_step name value state =
    let length = get_step_length name state in
    if value >= 0 && value < length
    then
      let steps = MapUtil.updated name value state.global_state.steps in
      let global_state = { state.global_state with steps = steps } in
      { state with global_state = global_state }
    else
      raise InvalidStepIndex

  let inline casts state = state.cards.casts
  let inline skills state = state.cards.skills
  let inline items state = state.cards.items
  let inline beasts state = state.cards.beasts