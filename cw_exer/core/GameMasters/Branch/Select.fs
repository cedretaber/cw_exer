namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Utils
open CardWirthEngine.Scenario.Events.Contents.BranchSelect
open CardWirthEngine.Cards
open CardWirthEngine.GameMasters

module Select =

  let inline set_selected_pc index state =
    State.set_selected_pc index state

  let inline private filter target adventurers =
    let advs =
      adventurers
      |> Array.indexed
      |> Array.toList in
    match target with
      Active ->
        List.filter
          (function _, adv -> Cast.is_active adv)
          advs
    | Party ->
        advs

  let inline private appraise initial coupons advs =
    let go =
      fun idx adv -> async {
        return idx, List.fold
          (fun acm coupon ->
              acm +
                if Cast.has_coupon adv coupon
                then coupon.value
                else 0)
          initial
          coupons
      } in
    [ for idx, adv in advs -> go idx adv ]
    |> Async.Parallel 
    |> Async.RunSynchronously

  let select selection (state: State.t) =
    match selection with
      { target = target; method = method } ->
        let advs_with_index = filter target state.party.adventurers in
        match method with
          Manual ->
            state,
            Output.SelectPlayerCharactor
              (List.map Pair.first advs_with_index)

        | Random ->
            let index, _ =
              advs_with_index
              |> List.length
              |> state.random
              |> (Array.get <| List.toArray advs_with_index)
            State.set_selected_pc index state, Output.None

        | Valued (initial, coupons) ->
            let index, _ =
              advs_with_index
              |> appraise initial coupons
              |> Array.maxBy Pair.second in
            State.set_selected_pc index state, Output.None