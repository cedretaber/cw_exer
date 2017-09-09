namespace CardWirthEngine.GameMasters

open CardWirthEngine.Util
open CardWirthEngine.Utils
open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Cards
open CardWirthEngine.GameMasters.Cards

module State =
  type GlobalData =
    { gossips : GossipName Set
    ; completed_scenarii : ScenarioName Set
    }

  type Area
    = Area of AreaId
    | Battle of BattleId * round : Round * enemies : Enemies.t

  type Event
    = Content of Event.t * Content.t
    | Action
    
  type Flags = (Flag.Name, Flag.State) Map
  type Steps = (Step.Name, Step.State) Map

  type GlobalState =
    { flags : Flags
    ; steps : Steps
    }

  type Casts = (CastId, Cast.t) Map
  type Skills = (SkillId, Skill.t) Map
  type Items = (ItemId, Item.t) Map
  type Beasts = (BeastId, Beast.t) Map
  type Infos = (InfoId, Info.t) Map

  type Cards =
    { casts : Casts
    ; skills : Skills
    ; items : Items
    ; beasts : Beasts
    ; infos : Infos
    }
  
  type SelectedCast
    = PC of Adventurers.Position
    | Enemy of EnemyId
    | Companion of Adventurers.Position
    | None

  type Scenario =
    { summary : Info.Summary.t
    ; cards : Cards
    (* 以下、可変情報 *)
    ; current_area : Area
    ; global_state : GlobalState
    ; eventStack : Event list
    ; selected : SelectedCast
    ; companions : Adventurers.t
    ; bgm : Bgm
    }

  exception OutOfScenarioException

  type t
    = Scenario of Scenario * Party.t * GlobalData * System.Random
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

      (* Scenario *)
      member private this.scenario =
        match this with
          Scenario (scenario, _, _, _) ->
            scenario
        | _ -> raise OutOfScenarioException
      member this.summary =
        this.scenario.summary
      member this.cards =
        this.scenario.cards
      member this.area =
        this.scenario.current_area
      member this.enemies =
        match this.scenario.current_area with
          Battle (_, _, enemies) -> Some enemies
        | _ -> Option.None
      member this.round =
        match this.scenario.current_area with
          Battle (_, round, _) -> Some round
        | _ -> Option.None
      member this.global_state =
        this.scenario.global_state
      member this.companions =
        this.scenario.companions
      member this.selected_pos =
        this.scenario.selected
      member this.selected_cast =
        match this.scenario.selected with
          PC pos -> Some (Adventurers.get pos this.adventurers)
        | Enemy idx -> Option.bind (Enemies.get idx) this.enemies
        | Companion pos -> Some (Adventurers.get pos this.companions)
        | None -> Option.None

  (* party ops *)
  let inline set_party party (state : t) =
    match state with
      Scenario (scenario, _, global_data, random) ->
        Scenario (scenario, party, global_data, random)

  let inline set_adventurer_at pos cast (state : t) =
    let party =
      Party.updated_adventurers pos (const' cast) state.party in
    set_party party state

  (* global data ops *)
  let inline set_global_data global_data (state : t) =
    match state with
      Scenario (scenario, party, _, random) ->
        Scenario (scenario, party, global_data, random)

  let inline set_gossip f (state : t) =
    let global_data = state.global_data in
    let new_data =
      { global_data with gossips = f global_data.gossips } in
    set_global_data new_data state
  let inline get_gossip gossip (state : t) =
    set_gossip (Set.add gossip) state
  let inline lose_gossip gossip (state : t) =
    set_gossip (Set.remove gossip) state
  let inline has_gossip gossip (state : t) =
    match state with
      Scenario (_, _, global_data, _) ->
        Set.contains gossip global_data.gossips

  let inline set_completed f (state : t) =
    let global_data = state.global_data in
    let new_data =
      { global_data with completed_scenarii = f global_data.completed_scenarii } in
    set_global_data new_data state
  let inline get_completed scenario (state : t) =
    set_gossip (Set.add scenario) state
  let inline lose_completed scenario (state : t) =
    set_gossip (Set.remove scenario) state
  let inline is_completed scenario (state : t) =
    match state with
      Scenario (_, _, global_data, _) ->
        Set.contains scenario global_data.completed_scenarii

  let inline private get_card id cards =
    Map.tryFind id cards

  let inline casts (state: t) = state.cards.casts
  let inline get_cast id (state : t) =
    get_card id <| casts state

  let inline skills (state: t) = state.cards.skills
  let inline get_skilll id (state : t) =
    get_card id <| skills state

  let inline items (state: t) = state.cards.items
  let inline get_item id (state : t) =
    get_card id <| items state

  let inline beasts (state: t) = state.cards.beasts
  let inline get_beast id (state : t) =
    get_card id <| beasts state

  let inline infos (state: t) = state.cards.infos
  let inline get_info id (state : t) =
    get_card id <| infos state

  (* scenario ops *)
  let inline update_scenarion f (state : t) =
    match state with
      Scenario (scenario, party, global_data, random) ->
        Scenario (f scenario, party, global_data, random)
    | _ -> raise OutOfScenarioException

  let inline set_current_area current_area (state : t) =
    update_scenarion
      (fun scenarion -> { scenarion with current_area = current_area })
      state

  (* global state ops *)
  let inline set_global_state global_state (state : t) =
    update_scenarion
      (fun scenario -> { scenario with global_state = global_state })
      state

  exception InvalidStateException

  (* flag ops *)
  let inline get_flag name (state: t) =
    state.global_state.flags |> Map.find name

  let inline set_flag name value (state: t) =
    let flags = Map.add name value state.global_state.flags in
    let global_state = { state.global_state with flags =  flags } in
    set_global_state global_state state

  (* step ops *)
  let inline get_step name (state: t) =
    state.global_state.steps |> Map.find name

  let inline get_step_length name (state: t) =
    let steps =
      state.summary.steps
      |> Map.find name in
    steps.steps
    |> Array.length

  exception InvalidStepIndexException

  let inline set_step name value (state: t) =
    let length = get_step_length name state in
    if value >= 0 && value < length
    then
      let steps = Map.add name value state.global_state.steps in
      let global_state = { state.global_state with steps = steps } in
      set_global_state global_state state
    else
      raise InvalidStepIndexException

  (* party ops *)
  exception InvalidSelectedAdventurerException

  let inline get_random_pc (state: t) =
    match state with
      Scenario (_, party, _, _) ->
        let idx = state.random <| Party.party_count party in
        Adventurers.int_to_pos idx, Party.at idx party

  let inline set_selected selected (state: t) =
    update_scenarion
      (fun scenario -> { scenario with selected = selected } )
      state

  let inline get_selected_or_random (state: t) =
    match state.selected_cast with
      Some cast ->
        state, cast
    | Option.None ->
        let idx, selected = get_random_pc state in
        set_selected (PC idx) state, selected
        
  (* Enemy Ops *)
  let inline enemy_at id (state : t) =
    Option.bind (Enemies.get id) state.enemies

  let inline set_enemy id enemy (state : t) =
    match state.area with
      Battle (battle_id, round, enemies) ->
        set_current_area
          (Battle (battle_id, round, Enemies.updated id enemy enemies))
          state
    | _ -> state

  let inline update_enemy f enemy_id (state : t) =
    Maybe.c {
      let! enemies = state.enemies
      let! enemy = Enemies.get enemy_id enemies
      return (set_enemy enemy_id (f enemy) state)
    } |> Option.fold (fun _ state -> state) state
   

  (* Companions ops *)
  let inline add_companion companion (state : t) =
    update_scenarion
      (fun scenario ->
        let companions = Adventurers.add companion scenario.companions in
        { scenario with companions = companions })
      state
  let inline remove_companion id (state : t) =
    update_scenarion
      (fun scenario ->
        let companions = Adventurers.remove_by_id id scenario.companions in
        { scenario with companions = companions })
      state
  let inline has_companion id (state : t) =
    Adventurers.contains_by
      (fun { property = { id = id' } } -> id' = id)
      state.companions
  let inline set_companion pos companion (state : t) =
    update_scenarion
      (fun scenario ->
        let companions = Adventurers.updated pos companion scenario.companions in
        { scenario with companions = companions })
      state
  let inline update_companion f pos (state : t) =
    let companions = Adventurers.updated pos f state.companions in
    update_scenarion
      (fun scenario -> { scenario with companions = companions })
      state


  (* card ops *)
  let inline add_to_bag count goods (state : t) =
    let party = Party.add_goods count goods state.party in
    set_party party state

  let inline remove_from_bag all count goods (state : t) =
    let party = Party.remove_goods all count goods state.party in
    set_party party state
         

  (* BGM *)
  let inline change_bgm bgm state =
    update_scenarion
      (fun scenario -> { scenario with bgm = bgm })
      state