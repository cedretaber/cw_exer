namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Data.Types
open CardWirthEngine.GameMasters

module AreaOrBattle =
  let inline is_area_in id (state : State.t) =
    match state.area with
      State.Area id' when id' = id -> true
    | _ -> false

  let inline is_battle_in id (state : State.t) =
    match state.area with
      State.Battle (id', _, _) when id' = id -> true
    | _ -> false

  let inline is_battle (state : State.t) =
    match state.area with
      State.Battle _ -> true
    | _ -> false

  let inline round cmp value (state : State.t) =
    state.round
    |> Option.fold
      (fun _ round -> Comparison.compare cmp round value)
      false