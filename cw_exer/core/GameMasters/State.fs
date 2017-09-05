namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Cards

module State =

  type Area
    = Area of AreaId
    | Battle of BattleId * Round * Cast.t array

  type Event
    = Content of Event.t * Content.t
    | Action
    
  type Flags = (Flag.Name, Flag.State) Map
  type Steps = (Step.Name, Step.State) Map

  type GlobalState =
    { flags : Flags
    ; steps : Steps
    }

  type Casts = (CastId, Cast.t) Map
  type Skills = (SkillId, Skill.t) Map
  type Items = (ItemId, Item.t) Map
  type Beasts = (BeastId, Beast.t) Map
  type Infos = (InfoId, Info.t) Map

  type Cards =
    { casts : Casts
    ; skills : Skills
    ; items : Items
    ; beasts : Beasts
    ; infos : Infos
    }

  type Scenario =
    { summary : Info.Summary.t
    ; cards : Cards
    (* 以下、可変情報 *)
    ; current_area : Area
    ; global_state : GlobalState
    ; eventStack : Event list
    ; selected_pc : int
    ; companions : Cast.t array
    ; bgm : Bgm
    }

  exception OutOfScenarioException

  type t
    = Scenario of Scenario * Party.t * System.Random
    with
      (* Anywhere *)
      member this.random max =
        match this with
          Scenario (_, _, random) -> random.Next max
      member this.random (min, max) =
        match this with
          Scenario (_, _, random) -> random.Next (min, max)
      member this.party =
        match this with
          Scenario (_, party, _) -> party
      member this.adventurers =
        this.party.adventurers

      (* Scenario *)
      member private this.scenario =
        match this with
          Scenario (scenario, _, _) ->
            scenario
        | _ -> raise OutOfScenarioException
      member this.summary =
        this.scenario.summary
      member this.area =
        this.scenario.current_area
      member this.enemies =
        match this.scenario.current_area with
          Battle (_, _, enemies) -> Some enemies
        | _ -> Option.None
      member this.round =
        match this.scenario.current_area with
          Battle (_, round, _) -> Some round
        | _ -> Option.None
      member this.cards =
        this.scenario.cards
      member this.global_state =
        this.scenario.global_state
      member this.selected_pc =
        this.scenario.selected_pc
      member this.companions =
        this.scenario.companions

  (* global state ops *)
  let inline set_global_state global_state (state : t) =
    match state with
      Scenario (scenario, party, random) ->
        let new_scenario = { scenario with global_state = global_state } in
        Scenario (new_scenario, party, random)
    | _ -> raise OutOfScenarioException

  exception InvalidStateException

  (* flag ops *)
  let inline get_flag name (state: t) =
    state.global_state.flags |> Map.find name

  let inline set_flag name value (state: t) =
    let flags = MapUtil.updated name value state.global_state.flags in
    let global_state = { state.global_state with flags =  flags } in
    set_global_state global_state state

  (* step ops *)
  let inline get_step name (state: t) =
    state.global_state.steps |> Map.find name

  let inline get_step_length name (state: t) =
    let steps =
      state.summary.steps
      |> Map.find name in
    steps.steps
    |> Array.length

  exception InvalidStepIndexException

  let inline set_step name value (state: t) =
    let length = get_step_length name state in
    if value >= 0 && value < length
    then
      let steps = MapUtil.updated name value state.global_state.steps in
      let global_state = { state.global_state with steps = steps } in
      set_global_state global_state state
    else
      raise InvalidStepIndexException

  (* card info ops *)
  let inline casts (state: t) = state.cards.casts
  let inline skills (state: t) = state.cards.skills
  let inline items (state: t) = state.cards.items
  let inline beasts (state: t) = state.cards.beasts
  let inline infos (state: t) = state.cards.infos

  (* party ops *)

  exception InvalidSelectedAdventurerException

  let inline get_selected_pc (state: t) =
    match state with
      Scenario({ selected_pc = idx }, party, _) ->
        let advs = party.adventurers in
        if idx < Array.length advs
        then
          idx, advs.[idx]
        else
          raise InvalidSelectedAdventurerException

  let inline set_selected_pc idx (state: t) =
    match state with
      Scenario (scenario, party, random) ->
        Scenario ({ scenario with selected_pc = idx }, party, random)

  let inline get_random_pc (state: t) =
    match state with
      Scenario (_, party, _) ->
        let idx = state.random <| Party.party_count party in
        idx, Party.at idx party
        

  (* Enemy Ops *)
  let inline enemy_at index (state : t) =
    Option.bind
      (fun enemies ->
        if index >= Array.length enemies
        then
          Option.None
        else
          Some enemies.[index])
      state.enemies

  (* BGM *)
  let inline change_bgm bgm state =
    match state with
      Scenario (scenario, party, random) ->
        Scenario ({ scenario with bgm = bgm }, party, random)