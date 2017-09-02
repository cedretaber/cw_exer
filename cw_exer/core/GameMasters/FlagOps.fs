namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data
open CardWirthEngine.Scenario.Events
open CardWirthEngine.GameMasters

module FlagOps =
  let get : Flag.Name -> State.t -> Flag.State =
    fun name ->
      function
      { flags = flags } ->
        Map.find name flags

  let set : Flag.Name -> Flag.State -> State.t -> State.t =
    fun name bool state ->
      { state with flags = MapUtil.updated name bool state.flags }

  let flip : Flag.Name -> State.t -> State.t =
    fun name state ->
      let bool = not <| get name state in
      set name bool state

  let substitute : Content.SourceFlag.t -> Flag.Name -> State.t -> State.t =
    fun source name state ->
      let flag =
        match source with
          Content.SourceFlag.Random -> state.random.Next 2 = 0
        | Content.SourceFlag.From name -> get name state in
      set name flag state

  let compare : Flag.Name -> Flag.Name -> State.t -> bool =
    fun left right state ->
      (get left state) = (get right state)