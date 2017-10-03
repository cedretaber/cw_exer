namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data
open CardWirthEngine.Scenario.Events
open CardWirthEngine.GameMasters

module FlagOps =
  let get name =
    State.get_scenario_unsafe >> Scenario.get_flag name

  let set name value =
    State.map_scenario <| Scenario.set_flag name value

  let flip : Flag.Name -> State.t -> State.t * bool =
    fun name state ->
      let bool = not <| get name state in
      set name bool state, bool

  let substitute : Content.SourceFlag -> Flag.Name -> State.t -> State.t * bool =
    fun source name state ->
      let flag =
        match source with
          Content.SourceFlag.Random -> State.random 2 state = 0
        | Content.SourceFlag.From name -> get name state in
      set name flag state, flag

  let compare : Flag.Name -> Flag.Name -> State.t -> bool =
    fun left right state ->
      (get left state) = (get right state)