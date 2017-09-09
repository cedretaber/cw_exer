namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data
open CardWirthEngine.Scenario.Events
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards

module StepOps =

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
        | Content.SourceStep.SelectedPc ->
            match state.selected_pos with
              State.PC pos -> Adventurers.pos_to_int pos
            | _ -> -1
        | Content.SourceStep.From name ->
            get name state in
      if step >= 0 && step <= target_length
      then
        set target step state
      else
        state