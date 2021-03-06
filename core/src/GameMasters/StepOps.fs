﻿namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data
open CardWirthEngine.Scenario.Events
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards

module StepOps =

  let get name state =
    Scenario.get_step name <| State.get_scenario_unsafe state

  let set name value =
    State.map_scenario <| Scenario.set_step name value

  let inline private step_length name state = Scenario.get_step_length name <| State.get_scenario_unsafe state

  let private crement f name state =
    Maybe.c {
      let step = get name state in
      let idx = f step in
      let! length = step_length name state
      if idx >= 0 && idx < length then
        return set name idx state
    } |> Option.defaultValue state

  let increment : Step.Name -> State.t -> State.t = crement <| fun i -> i + 1
  let decrement : Step.Name -> State.t -> State.t = crement <| fun i -> i - 1

  let substitute : Content.SourceStep -> Step.Name -> State.t -> State.t =
    fun source target state ->
      let maybe_target_length = step_length target state in
      match source, maybe_target_length with
        Content.SourceStep.Random, Some target_length ->
          Some <| State.random target_length state
      | Content.SourceStep.SelectedPc, _ ->
          match Scenario.get_selected <| State.get_scenario_unsafe state with
            Scenario.PC pos ->
              Some <| Adventurers.pos_to_int pos
          | _ ->
              Option.None
      | Content.SourceStep.From name, _ ->
          Some <| get name state
      | _ -> Option.None
      |> Option.fold
        (fun _ step -> set target step state)
        state