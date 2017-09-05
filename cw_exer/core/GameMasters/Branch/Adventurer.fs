namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
open CardWirthEngine.GameMasters

module Adventurer =
  let inline judge ability state = true

  exception InvalidTargetException of Target

  let inline level target level (state : State.t) =
    match target with
      Target.Party ->
        Party.average_level state.party >= level
    | Target.Selected ->
        (State.get_selected_pc state).property.level >= level
    | _ -> raise <| InvalidTargetException target

  let inline private judge_status status (cast : Cast.t) =
    match status, cast with
      Active, Cast.Active -> true
    | Inactive, Cast.Inactive -> true
    | Alive, Cast.Alive -> true
    | Dead, Cast.Dead -> true
    | Fine, Cast.Fine -> true
    | Injured, Cast.Injured -> true
    | HeavyInjured, Cast.HeavyInjured -> true
    | Unconscious, Cast.Unconscious -> true
    | Poison, Cast.Poison _ -> true
    | Sleep, Cast.Sleep _ -> true
    | Bind, Cast.Bind _ -> true
    | Paralyze, Cast.Paralyze _ -> true
    | Confuse, Cast.Confuse _ -> true
    | Overheat, Cast.Overheat _ -> true
    | Brave, Cast.Brave _ -> true
    | Panic, Cast.Panic _ -> true
    | Silence, Cast.Silence _ -> true
    | FaceUp, Cast.FaceUp _ -> true
    | AntiMagic, Cast.AntiMagic _ -> true
    | UpAction, Cast.UpAction _ -> true
    | UpAvoid, Cast.UpAvoid _ -> true
    | UpResist, Cast.UpResist _ -> true
    | UpDefense, Cast.UpDefense _ -> true
    | DownAction, Cast.DownAction _ -> true
    | DownAvoid, Cast.DownAvoid _ -> true
    | DownResist, Cast.DownResist _ -> true
    | DownDefense, Cast.DownDefense _ -> true
    | Status.None, _ -> true
    | _ -> false

  let inline status target status (state : State.t) =
    match target with
      Target.Selected ->
        State.get_selected_pc state
        |> judge_status status
    | Target.Random ->
        State.get_random_pc state
        |> judge_status status
    | Target.Party ->
        Array.forall
          (judge_status status)
          state.adventurers
    | _ ->
        raise <| InvalidTargetException target
      