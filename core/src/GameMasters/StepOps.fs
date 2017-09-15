namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data
open CardWirthEngine.Scenario.Events
open CardWirthEngine.GameMasters

module StepOps =

  let get name state =
    Scenario.get_step name <| State.get_scenario_unsafe state

  let set name value =
    State.update_scenarion <| Scenario.set_step name value

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

  let substitute : Content.SourceStep.t -> Step.Name -> State.t -> State.t =
    fun source target state ->
      let maybe_target_length = step_length target state in
      match source, maybe_target_length with
        Content.SourceStep.Random, Some target_length ->
          Some (state.random target_length)
      | Content.SourceStep.SelectedPc, _ ->
          Scenario.selected_pos <| State.get_scenario_unsafe state
      | Content.SourceStep.From name, _ ->
          Some (get name state)
      | _ -> Option.None
      |> Option.fold
        (fun _ step -> set target step state)
        state