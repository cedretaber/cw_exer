namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Casts
open CardWirthEngine.Cards

module CouponOps =
  
  let add_coupon target name value state =

    let coupon : Coupon.t = { name = name; value = value } in

    begin match target with
      Target.Selected ->
        begin function
          Scenario.PC pos ->
            State.map_party
              (Party.add_coupon pos coupon)
        | Scenario.Enemy id ->
            State.map_scenario
              (Scenario.map_enemy (Cast.add_coupon coupon) id)
        | Scenario.Companion pos ->
            State.map_scenario
              (Scenario.update_companion (Cast.add_coupon coupon) pos)
        | Scenario.None ->
            id
        end (State.get_scenario_unsafe state).selected
    | Target.Random ->
        let pos, _ = State.get_random_pc state in
        State.map_party <| Party.add_coupon pos coupon
    | Target.Party ->
        State.map_party <| Party.add_coupon_all coupon
    | _ ->
        id
    end state
  
  let rec remove_coupon target name state =
    begin match target with
      Target.Selected ->
        (State.get_scenario_unsafe state).selected
        |> function
             Scenario.PC pos ->
               State.map_party
                 (Party.remove_coupon pos name)
           | Scenario.Enemy id ->
               State.map_scenario
                 (Scenario.map_enemy (Cast.remove_coupon name) id)
           | Scenario.Companion pos ->
               State.map_scenario
                 (Scenario.update_companion (Cast.remove_coupon name) pos)
           | Scenario.None ->
               id
    | Target.Random ->
        Party.find_coupon_holder name (State.get_party state)
        |> function
             Some (pos, _) ->
               State.set_selected (Scenario.PC pos)
               >> remove_coupon Target.Selected name
           | Option.None ->
               id
    | Target.Party ->
        State.map_party <| Party.remove_coupon_all name
    | _ ->
      id
    end state