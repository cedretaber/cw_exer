﻿namespace CardWirthEngine.GameMasters

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

  type Scenario =
    { summary : Info.Summary.t
    ; current_area : Area
    ; global_state : GlobalState
    ; cards : Cards
    ; state : State
    ; selected_adventurer : int
    ; bgm : Bgm
    }

  type t =
    Scenario of Scenario * Party.t * System.Random
    with
      member this.random max =
        match this with
          Scenario (_, _, random) -> random.Next max
      member this.random (min, max) =
        match this with
          Scenario (_, _, random) -> random.Next (min, max)
      member this.get_global_state =
        match this with
          Scenario (scenario, _, _) ->
            scenario.global_state
      member this.set_global_state global_state =
        match this with
          Scenario (scenario, party, random) ->
            let new_scenario = { scenario with global_state = global_state } in
            Scenario (new_scenario, party, random)
      member this.get_summary =
        match this with
          Scenario (scenario, _, _) ->
            scenario.summary
      member this.get_cards =
          match this with
            Scenario (scenario, _, _) ->
              scenario.cards

  exception InvalidState

  (* flag ops *)
  let inline get_flag name (state: t) =
    state.get_global_state.flags |> Map.find name

  let inline set_flag name value (state: t) =
    let flags = MapUtil.updated name value state.get_global_state.flags in
    let global_state = { state.get_global_state with flags =  flags } in
    state.set_global_state global_state

  (* step ops *)
  let inline get_step name (state: t) =
    state.get_global_state.steps |> Map.find name

  let inline get_step_length name (state: t) =
    let steps =
      state.get_summary.steps
      |> Map.find name in
    steps.steps
    |> Array.length

  exception InvalidStepIndex

  let inline set_step name value (state: t) =
    let length = get_step_length name state in
    if value >= 0 && value < length
    then
      let steps = MapUtil.updated name value state.get_global_state.steps in
      let global_state = { state.get_global_state with steps = steps } in
      state.set_global_state global_state
    else
      raise InvalidStepIndex

  (* card info ops *)
  let inline casts (state: t) = state.get_cards.casts
  let inline skills (state: t) = state.get_cards.skills
  let inline items (state: t) = state.get_cards.items
  let inline beasts (state: t) = state.get_cards.beasts