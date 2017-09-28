namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine
open CardWirthEngine.Utils
open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Casts
open CardWirthEngine.Cards
open CardWirthEngine.Scenario.Events.Contents.BranchAbility
open CardWirthEngine.Scenario.Events.Contents.BranchRandomSelect
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards

module Adventurer =

  // TODO: 実装する
  let inline private judge_ability level sleep physical mental cast =
    match sleep, cast with
      true, Cast.Bind _
    | true, Cast.Sleep _ -> false
    | _ -> true
      
  let inline judge ability (state : State.t) =
    let { level = level
        ; target = { target = target; sleep = sleep }
        ; physical = physical
        ; mental = mental
        } = ability in
    match target with
      Target.Selected ->
        let new_state, cast = State.get_selected_or_random state in
        new_state, judge_ability level sleep physical mental cast
    | Target.Random ->
        let idx, cast = State.get_random_pc state in
        Pair.t
          (State.set_selected (Scenario.PC idx) state)
          (judge_ability level sleep physical mental cast)
    | Target.Party ->
        state,
        Adventurers.forall
          (fun card -> judge_ability level sleep physical mental card.cast)
          state.adventurers
    | _ ->
      state, false

  exception InvalidTargetException of Target

  let inline level target level (state : State.t) =
    match target with
      Target.Party ->
        Party.average_level state.party >= level
    | Target.Selected ->
        (State.get_selected_or_random state |> Pair.second).property.level >= level
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
        let new_state, cast =
          State.get_selected_or_random state in
        new_state, judge_status status cast
    | Target.Random ->
        let idx, cast =
          State.get_random_pc state in
        State.set_selected (Scenario.PC idx) state,
        judge_status status cast
    | Target.Party ->
        state,
        Adventurers.forall
          (fun card -> judge_status status card.cast)
          state.adventurers
    | _ ->
        raise <| InvalidTargetException target
      
  let inline party_count value (state : State.t) =
    Party.party_count state.party > value

  (* random_select *)
  let inline private generate_casts party enemy npc (state : State.t) =
    seq {
      if party then
        for idx, card
          in Adventurers.indexed state.party.adventurers ->
            State.set_selected (Scenario.PC (Adventurers.int_to_pos idx)) state, card.cast
      let scenario = State.get_scenario_unsafe state in
      if enemy then
        let enemies =
          Option.fold
            (fun _ es -> Enemies.to_list es)
            List.Empty
            (Scenario.enemies scenario)
        for cast in enemies ->
          state, cast
      if npc then 
        for card in scenario.companions ->
          state, card.cast
    }

  let inline private level_filter level (casts : ('a * Cast.t) seq) =
    match level with
      Option.None ->
        casts
    | Some { max = max; min = min } ->
        Seq.filter
          (function
            _, { property = { level = level } } ->
              min <= level && level <= max)
          casts

  let inline private status_filter status casts =
    match status with
      Option.None ->
        casts
    | Some status ->
        Seq.filter
          (function _, cast -> judge_status status cast)
          casts

  let random_select condition (state : State.t) =
    let
      { range = { party = party
                ; enemy = enemy
                ; npc = npc
                }
      ; level = level
      ; status = status
      } = condition in
    generate_casts party enemy npc state
    |> level_filter level
    |> status_filter status
    |> Seq.tryHead
    |> Option.fold
      (Util.const' <| function state, _ -> state, true)
      (state, false)

  let inline has_coupon range matching_type values (state : State.t) =
    let f f =
      match matching_type with
        And -> List.forall f values
      | Or -> List.exists f values
    let g (cast : Cast.t) =
      let had_coupons =
        CouponSet.to_set cast.property.coupons in
      f <| fun coupon -> Set.contains coupon had_coupons
    let g' (card : Adventurers.CardState) = g card.cast
    match range with
      Selected ->
        state.selected_cast
        |> Option.fold
          (fun _ cast -> state, g cast)
          (state, false)
    | Random ->
        Adventurers.try_find_with_position g' state.adventurers
        |> function
             Some (pos, _) ->
               (State.set_selected (Scenario.PC pos) state), true
           | _ ->
               state, false
    | Range.Party ->
        state, Adventurers.forall g' state.adventurers
    | _ ->
      state, false






      