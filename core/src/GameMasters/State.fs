namespace CardWirthEngine.GameMasters

open System

open Aether
open Aether.Operators

open CardWirthEngine.Util
open CardWirthEngine.Data.Type
open CardWirthEngine.GameMasters.Cards

module State =
  type GlobalData =
    { gossips : GossipName Set
    ; completed_scenarii : ScenarioName Set
    }
    with
      static member gossip_ =
        (fun gd -> gd.gossips), (fun gs gd -> { gd with gossips = gs })
      static member completed_scenarii_ =
        (fun gd -> gd.completed_scenarii), (fun cs gd -> { gd with completed_scenarii = cs })

  exception OutOfScenarioException

  type t
    = Scenario of Scenario.t * Party.t * GlobalData * System.Random
    with
      (* Lens and Prism *)
      static member scenario_ =
        (function Scenario (s, _, _, _) -> Some s)
        , (fun s -> function Scenario (_, p, g, r) -> Scenario (s, p, g, r))
      static member party_ =
        (function Scenario (_, p, _, _) -> p)
        , (fun p -> function Scenario (s, _, g, r) -> Scenario (s, p, g, r))
      static member global_data_ =
        (function Scenario (_, _, g, _) -> g)
        , (fun g -> function Scenario (s, p, _, r) -> Scenario (s, p, g, r))
      static member random_ =
        (function Scenario (_, _, _, r) -> r)
        , (fun r -> function Scenario (s, p, g, _) -> Scenario (s, p, g, r)) 

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
  let get_party = Optic.get t.party_
  let set_party = Optic.set t.party_
  let map_party = Optic.map t.party_

  let get_adventurer_at pos =
    get_party >> Party.at (Adventurers.pos_to_int pos)

  let set_adventurer_at pos cast =
    map_party <| Party.set_adventurer pos cast

  let get_momey = t.party_ >-> Party.t.money_ |> Optic.get

  let add_money amount =
    map_party <| Party.add_money amount

  (* global data ops *)
  let set_global_data = Optic.set t.global_data_
  let map_global_data = Optic.map t.global_data_

  let private gossips_ = t.global_data_ >-> GlobalData.gossip_
  let get_gossips = Optic.get gossips_
  let map_gossips = Optic.map gossips_

  let get_gossip gossip =
    map_gossips <| Set.add gossip

  let lose_gossip gossip =
    map_gossips <| Set.remove gossip

  let has_gossip gossip =
    get_gossips >> Set.contains gossip

  let completeds_ = t.global_data_ >-> GlobalData.completed_scenarii_
  let get_completeds = Optic.get completeds_
  let map_completeds = Optic.map completeds_

  let get_completed scenario =
    map_completeds <| Set.add scenario

  let lose_completed scenario =
    map_completeds <| Set.remove scenario

  let is_completed scenario =
    get_completeds >> Set.contains scenario

  (* scenario ops *)
  exception InvalidStateException

  let get_scenario = Optic.get t.scenario_
  let map_scenario = Optic.map t.scenario_

  let map_scenarion_unsafe f state =
    if Option.isNone <| get_scenario state then raise OutOfScenarioException
    map_scenario f state

  let get_scenario_unsafe =
    get_scenario >> Option.get

  let get_random_pc state =
    let party = get_party state in
    let idx = state.random <| Party.party_count party in
    Adventurers.int_to_pos idx, Party.at idx party

  let selected = t.scenario_ >?> Scenario.t.selected_ |> Optic.get

  let set_selected selected =
    map_scenario <| Scenario.set_selected selected

  (* 選択中キャストが存在しない場合、冒険者中から強制的に選択する。 *)
  let force_selected state =
    match selected state with
      Option.None
    | Some (Scenario.None) ->
        let pos, _ = get_random_pc state in
        set_selected (Scenario.PC pos) state
    | _ ->
        state

  let force_selected_and_cast state =
    let state' = force_selected state in
    let error = Exception "Unreachable Status" in
    let cast =
      match selected state' with
        Some (Scenario.PC pos) ->
          get_adventurer_at pos state'
      | Some (Scenario.Enemy id) ->
          match Scenario.enemy_at id <| get_scenario_unsafe state' with
            Some (enemy) -> enemy
          | Option.None -> raise error
      | Some (Scenario.Companion pos) ->
          (get_scenario_unsafe state).companions
          |> Adventurers.get pos
          |> function
                Adventurers.Exist cast -> cast
              | Adventurers.Flipped cast -> cast
              | _ -> raise error
      | _ -> raise error
    cast, state'

  (* card ops *)
  let inline add_to_bag count goods =
    map_party (Party.add_goods count goods)

  let inline remove_from_bag count goods =
    map_party (Party.remove_goods count goods)

  (* Background *)
  let get_backgrounds =
    function
      Scenario (scenario, _, _, _) -> scenario.backgrounds

  let change_background backgrounds =
    map_scenario <| Scenario.add_backgrounds backgrounds

  (* BGM *)
  let change_bgm bgm =
    map_scenario <| Scenario.set_bgm bgm