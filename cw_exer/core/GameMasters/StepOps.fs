namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data
open CardWirthEngine.Scenario.Events
open CardWirthEngine.GameMasters

module StepOps =
  open State

  let get : Step.Name -> State.t -> Step.State =
    State.get_step

  let set : Step.Name -> Step.State -> State.t -> State.t =
    State.set_step

  let private crement f name state =
    let step = get name state in
    let idx = f step in
    if idx >= 0 && idx < State.get_step_length name state
    then
      set name idx state
    else
      state

  let increment : Step.Name -> State.t -> State.t = crement <| fun i -> i + 1
  let decrement : Step.Name -> State.t -> State.t = crement <| fun i -> i - 1

  let substitute : Content.SourceStep.t -> Step.Name -> State.t -> State.t =
    fun source target state ->
      let target_length = State.get_step_length target state in
      let step =
        match source with
          Content.SourceStep.Random ->
            state.random target_length
        | Content.SourceStep.SelectedAdventurer ->
            state.selected_adventurer
        | Content.SourceStep.From name ->
            get name state in
      if step <= target_length
      then
        set target step state
      else
        state