namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Data.Types
open CardWirthEngine.GameMasters

module AreaOrBattle =
  let inline get_area state =
    let scenario = State.get_scenario_unsafe state in
    scenario.current_area

  let inline is_area_in id (state : State.t) =
    match get_area state with
      Scenario.Area id' when id' = id -> true
    | _ -> false

  let inline is_battle_in id (state : State.t) =
    match get_area state with
      Scenario.Battle (id', _, _) when id' = id -> true
    | _ -> false

  let inline is_battle (state : State.t) =
    match get_area state with
      Scenario.Battle _ -> true
    | _ -> false

  let inline round cmp value (state : State.t) =
    match get_area state with
      Scenario.Battle (_, round, _) ->
        Comparison.compare cmp round value
    | _ -> false