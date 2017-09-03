namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Util
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
    let go = fun adv ->
      List.fold
        (fun acm coupon ->
            acm +
              if Cast.has_coupon adv coupon
              then coupon.value
              else 0)
        initial
        coupons in
    List.map
      (function idx, adv -> idx, go adv)
      advs

  let select selection (state: State.t) =
    match selection with
      { target = target; method = method } ->
        let advs_with_index = filter target state.party.adventurers in
        match method with
          Manual ->
            state,
            Output.SelectPlayerCharactor
              (List.map first advs_with_index)

        | Random ->
            let length = List.length advs_with_index in
            let i = state.random length in
            let (index, _) = (List.toArray advs_with_index).[i] in
            State.set_selected_pc index state, Output.None

        | Valued (initial, coupons) ->
            advs_with_index
            |> appraise initial coupons
            |> List.maxBy second
            |> function index, _ -> State.set_selected_pc index state, Output.None