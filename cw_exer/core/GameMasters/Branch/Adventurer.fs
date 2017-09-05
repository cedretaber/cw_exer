namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Utils
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
        (State.get_selected_pc state |> Pair.second).property.level >= level
    | _ -> raise <| InvalidTargetException target

  let inline private judge_status status (cast : Cast.t) =
    match status, cast with
      Active, Cast.Active
    | Inactive, Cast.Inactive
    | Alive, Cast.Alive
    | Dead, Cast.Dead
    | Fine, Cast.Fine
    | Injured, Cast.Injured
    | HeavyInjured, Cast.HeavyInjured
    | Unconscious, Cast.Unconscious
    | Poison, Cast.Poison _
    | Sleep, Cast.Sleep _
    | Bind, Cast.Bind _
    | Paralyze, Cast.Paralyze _
    | Confuse, Cast.Confuse _
    | Overheat, Cast.Overheat _
    | Brave, Cast.Brave _
    | Panic, Cast.Panic _
    | Silence, Cast.Silence _
    | FaceUp, Cast.FaceUp _
    | AntiMagic, Cast.AntiMagic _
    | UpAction, Cast.UpAction _
    | UpAvoid, Cast.UpAvoid _
    | UpResist, Cast.UpResist _
    | UpDefense, Cast.UpDefense _
    | DownAction, Cast.DownAction _
    | DownAvoid, Cast.DownAvoid _
    | DownResist, Cast.DownResist _
    | DownDefense, Cast.DownDefense _
    | Status.None, _ -> true
    | _ -> false

  let inline status target status (state : State.t) =
    match target with
      Target.Selected ->
        State.get_selected_pc state
        |> Pair.second
        |> judge_status status
    | Target.Random ->
        State.get_random_pc state
        |> Pair.second
        |> judge_status status
    | Target.Party ->
        Array.forall
          (judge_status status)
          state.adventurers
    | _ ->
        raise <| InvalidTargetException target
      