namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data
open CardWirthEngine.Scenario.Events
open CardWirthEngine.GameMasters

module StepOps =
  open State

  let get : Step.Name -> State.t -> Step.State =
    fun name ->
      function
      { steps = steps } ->
        Map.find name steps

  exception InvalidStepIndexException

  let private read_step name =
    function
      { summary = summary; steps = steps } ->
        let step_info = Map.find name summary.steps in
        step_info, steps

  let set : Step.Name -> Step.State -> State.t -> State.t =
    fun name idx state ->
      let step_info, steps = read_step name state in
      if idx >= 0 && idx < Array.length step_info.steps
      then
        { state with steps = MapUtil.updated name idx steps }
      else
        raise InvalidStepIndexException

  let private crement f name state =
    let step_info, steps = read_step name state in
    let step = Map.find name steps in
    let idx = f step in
    if idx >= 0 && idx < Array.length step_info.steps
    then
      { state with steps = MapUtil.updated name idx steps }
    else
      state

  let increment : Step.Name -> State.t -> State.t = crement <| fun i -> i + 1
  let decrement : Step.Name -> State.t -> State.t = crement <| fun i -> i - 1

  let substitute : Content.SourceStep.t -> Step.Name -> State.t -> State.t =
    fun source target state ->
      let target_info, steps = read_step target state in
      let target_length = Array.length target_info.steps in
      let step =
        match source with
          Content.SourceStep.Random ->
            state.random.Next target_length
        | Content.SourceStep.SelectedAdventurer ->
            state.selected_adventurer
        | Content.SourceStep.From name ->
            Map.find name steps in
      if step <= target_length
      then
        set target step state
      else
        state