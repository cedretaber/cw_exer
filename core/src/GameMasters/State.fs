namespace CardWirthEngine.GameMasters

open CardWirthEngine.Util
open CardWirthEngine.Data.Type
open CardWirthEngine.GameMasters.Cards

module State =
  type GlobalData =
    { gossips : GossipName Set
    ; completed_scenarii : ScenarioName Set
    }

  exception OutOfScenarioException

  type t
    = Scenario of Scenario.t * Party.t * GlobalData * System.Random
    with
      (* Anywhere *)
      member this.random max =
        match this with
          Scenario (_, _, _, random) -> random.Next max
      member this.random (min, max) =
        match this with
          Scenario (_, _, _, random) -> random.Next (min, max)
      member this.party =
        match this with
          Scenario (_, party, _, _) -> party
      member this.adventurers =
        this.party.adventurers
      member this.global_data =
        match this with
          Scenario (_, _, global_data, _) -> global_data
      member this.selected_cast =
        match this with
          Scenario ({ selected = Scenario.PC pos }
                   , party, _ , _
                   ) ->
            match Adventurers.get pos party.adventurers with
              Adventurers.Exist cast -> Some cast
            | Adventurers.Flipped cast -> Some cast
        | Scenario ({ selected = Scenario.Enemy idx
                    ; current_area = Scenario.Battle (_, _, enemies) }
                   , _, _, _
                   ) ->
            Enemies.get idx enemies
        | Scenario ({ selected = Scenario.Companion pos
                    ; companions = companions  }
                   , _, _, _
                   ) ->
            match Adventurers.get pos companions with
              Adventurers.Exist cast -> Some cast
            | Adventurers.Flipped cast -> Some cast
        | _ -> Option.None

  (* party ops *)
  let inline set_party party (state : t) =
    match state with
      Scenario (scenario, _, global_data, random) ->
        Scenario (scenario, party, global_data, random)

  let inline set_adventurer_at pos cast (state : t) =
    let party = Party.set_adventurer pos cast state.party in
    set_party party state

  let inline update_party f (state: t) =
    set_party (f state.party) state

  (* Money Ops *)
  let add_money amount (state : t) =
    let state' =
      update_party
        (Party.add_money amount)
        state in
    state'.party.money, state'

  (* global data ops *)
  let inline set_global_data global_data (state : t) =
    match state with
      Scenario (scenario, party, _, random) ->
        Scenario (scenario, party, global_data, random)

  let inline update_gossips f (state : t) =
    let global_data = state.global_data in
    let new_data =
      { global_data with gossips = f global_data.gossips } in
    set_global_data new_data state

  let inline get_gossip gossip (state : t) =
    update_gossips (Set.add gossip) state

  let inline lose_gossip gossip (state : t) =
    update_gossips (Set.remove gossip) state

  let inline has_gossip gossip (state : t) =
    match state with
      Scenario (_, _, global_data, _) ->
        Set.contains gossip global_data.gossips

  let inline update_completed f (state : t) =
    let global_data = state.global_data in
    let new_data =
      { global_data with completed_scenarii = f global_data.completed_scenarii } in
    set_global_data new_data state

  let inline get_completed scenario (state : t) =
    update_completed (Set.add scenario) state

  let inline lose_completed scenario (state : t) =
    update_completed (Set.remove scenario) state

  let inline is_completed scenario (state : t) =
    match state with
      Scenario (_, _, global_data, _) ->
        Set.contains scenario global_data.completed_scenarii

  (* scenario ops *)
  exception InvalidStateException

  let inline update_scenarion f (state : t) =
    match state with
      Scenario (scenario, party, global_data, random) ->
        Scenario (f scenario, party, global_data, random)
    | _ -> raise OutOfScenarioException

  let get_scenario_unsafe =
    function
      Scenario (scenario, _, _, _) -> scenario
    | _ -> raise InvalidStateException
  
  exception InvalidSelectedAdventurerException

  let inline get_random_pc (state: t) =
    match state with
      Scenario (_, party, _, _) ->
        let idx = state.random <| Party.party_count party in
        Adventurers.int_to_pos idx, Party.at idx party

  let inline set_selected selected =
    update_scenarion <|
      fun scenario -> { scenario with selected = selected }

  let inline get_selected state =
    let scenario = get_scenario_unsafe state in
    Scenario.selected_pos scenario

  let inline get_selected_or_random (state: t) =
    match state.selected_cast with
      Some cast ->
        state, cast
    | Option.None ->
        let idx, selected = get_random_pc state in
        set_selected (Scenario.PC idx) state, selected

  (* card ops *)
  let inline add_to_bag count goods =
    update_party (Party.add_goods count goods)

  let inline remove_from_bag count goods =
    update_party (Party.remove_goods count goods)

  (* Background *)
  let inline change_background backgrounds =
    update_scenarion <| Scenario.add_backgrounds backgrounds

  (* BGM *)
  let inline change_bgm bgm =
    update_scenarion <|
      fun scenario -> { scenario with bgm = bgm }