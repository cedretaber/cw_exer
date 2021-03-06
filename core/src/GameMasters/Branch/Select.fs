﻿namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Data.Casts
open CardWirthEngine.Utils
open CardWirthEngine.Scenario.Events.Contents.BranchSelect
open CardWirthEngine.Cards
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards

module Select =

  let inline set_selected_pc index state =
    State.set_selected (Scenario.PC index) state

  let inline private filter target adventurers =
    let advs =
      Adventurers.to_seq_with_pos adventurers in
    match target with
      Active ->
        Seq.choose
          (function
            pos, (card : Adventurers.CardState) when Cast.is_active card.cast ->
              Some (pos, card)
          | _ ->
              Option.None)
          advs
    | Party ->
        advs

  let inline private appraise initial coupons (advs : (Adventurers.Position * Adventurers.CardState) seq) =
    let go =
      fun pos adv -> async {
        return pos, List.fold
          (fun acm (coupon : Coupon.t) ->
              acm +
                if Cast.has_coupon coupon.name adv
                then coupon.value
                else 0)
          initial
          coupons
      } in
    [ for pos, adv in advs -> go pos adv.cast ]
    |> Async.Parallel 
    |> Async.RunSynchronously

  let select selection (state: State.t) =
    let { target = target; method = method } =
      selection in
    match method with
      Manual ->
        let filtered =
          filter target (State.get_adventurers state)
          |> Seq.map Pair.first in
        ( state
        , Output.SelectPlayerCharactor
            (Seq.toList filtered)
        )

    | Random ->
        let pos, _ = State.get_random_pc state in
        State.set_selected (Scenario.PC pos) state, Output.None

    | Valued (initial, coupons) ->
        let pos, _ =
          Adventurers.to_seq_with_pos (State.get_adventurers state)
          |> appraise initial coupons
          |> Array.maxBy Pair.second in
        State.set_selected (Scenario.PC pos) state, Output.None