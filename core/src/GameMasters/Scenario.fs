namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data.Type
open CardWirthEngine.Data
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Cards
open CardWirthEngine.GameMasters.Cards

module Scenario =

  type Area
    = Area of AreaId
    | Battle of BattleId * round : Round * enemies : Enemies.t
    
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

  type Event
    = Content of Event.t * Content.t
    | Action
  
  type SelectedCast
    = PC of Adventurers.Position
    | Enemy of EnemyId
    | Companion of Adventurers.Position
    | None
  
  type t =
    { summary : Summary.t
    ; cards : Cards
    (* 以下、可変情報 *)
    ; current_area : Area
    ; global_state : GlobalState
    ; eventStack : Event list
    ; selected : SelectedCast
    ; companions : Adventurers.t
    ; bgm : Bgm
    }

  let enemies =
    function
      { current_area = Battle (_, _, enemies) } ->
        Some enemies
    | _ ->
        Option.None

  let rounds =
    function
      { current_area = Battle (_, rounds, _) } ->
        Some rounds
    | _ ->
        Option.None

  let selected_pos =
    function
      { selected = PC pos } -> Some (Adventurers.pos_to_int pos)
    | _ -> Option.None
        
  
  (* Cards Ops *)
  let inline private get_card id cards =
    Map.tryFind id cards

  let inline casts scenario = scenario.cards.casts
  let inline get_cast id scenario =
    get_card id <| casts scenario

  let inline skills scenario = scenario.cards.skills
  let inline get_skilll id scenario =
    get_card id <| skills scenario

  let inline items scenario = scenario.cards.items
  let inline get_item id scenario =
    get_card id <| items scenario

  let inline beasts scenario = scenario.cards.beasts
  let inline get_beast id scenario =
    get_card id <| beasts scenario

  let inline infos scenario = scenario.cards.infos
  let inline get_info id scenario =
    get_card id <| infos scenario
    

  (* flag ops *)
  let inline get_flag name scenario =
    scenario.global_state.flags |> Map.find name

  let inline set_flag name value scenario =
    let flags = Map.add name value scenario.global_state.flags in
    { scenario with global_state = { scenario.global_state with flags =  flags } }


  (* step ops *)
  exception InvalidStepIndexException

  let inline get_step name scenario =
    scenario.global_state.steps |> Map.find name

  let inline get_step_length name scenario =
    scenario.summary.steps
    |> Map.tryFind name
    |> Option.map
      (fun steps -> Array.length steps.steps)

  let inline set_step name value scenario =
    let maybe_new_scenario =
      Maybe.c {
        let! length = get_step_length name scenario
        if value >= 0 && value < length then
          let steps = Map.add name value scenario.global_state.steps in
          return { scenario with global_state = { scenario.global_state with steps = steps } }
      } in
    match maybe_new_scenario with
      Some new_scenario -> new_scenario
    | Option.None -> raise InvalidStepIndexException
        
        
  (* Enemy Ops *)
  let inline enemy_at id scenario =
    Option.bind (Enemies.get id) <| enemies scenario

  let inline set_enemy id enemy =
    function
      { current_area = Battle (battle_id, round, enemies) } as scenario ->
        let battle = Battle (battle_id, round, Enemies.updated id enemy enemies) in
        { scenario with current_area = battle }
    | scenario -> scenario

  let inline update_enemy f enemy_id scenario =
    Maybe.c {
      let! enemies = enemies scenario
      let! enemy = Enemies.get enemy_id enemies
      return set_enemy enemy_id (f enemy) scenario
    } |> Option.fold (fun _ state -> state) scenario
   

  (* Companions ops *)
  let inline add_companion companion scenario =
    let companions = Adventurers.add companion scenario.companions in
    { scenario with companions = companions }

  let inline remove_companion id scenario =
    let companions = Adventurers.remove_by_id id scenario.companions in
    { scenario with companions = companions }

  let inline has_companion id scenario =
    Adventurers.contains_by
      (fun { property = { id = id' } } -> id' = id)
      scenario.companions
 
  let inline set_companion pos companion scenario =
    let companions = Adventurers.updated pos companion scenario.companions in
    { scenario with companions = companions }

  let inline update_companion f pos scenario =
    let companions = Adventurers.updated pos f scenario.companions in
    { scenario with companions = companions }