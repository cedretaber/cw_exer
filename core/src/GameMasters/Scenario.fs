namespace CardWirthEngine.GameMasters

open Aether
open Aether.Operators

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
    ; infos : InfoId Set
    }
    with
      static member flags_ =
        (fun gs -> gs.flags), (fun flags gs -> { gs with flags = flags })
      static member steps_ =
        (fun gs -> gs.steps), (fun steps gs -> { gs with steps = steps })
      static member infos_ =
        (fun gs -> gs.infos), (fun infos gs -> { gs with infos = infos })

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
    ; event_stack : Event list
    ; selected : SelectedCast
    ; companions : Adventurers.t
    ; backgrounds : BackgroundImage.t list
    ; bgm : Bgm
    }
    with
      static member summary_ =
        (fun t -> t.summary), (fun s t -> { t with summary = s })
      static member cards_ =
        (fun t -> t.cards), (fun cs t -> { t with cards = cs })
      static member current_area_ =
        (fun t -> t.current_area), (fun ca t -> { t with current_area = ca })
      static member global_state_ =
        (fun t -> t.global_state), (fun gs t -> { t with global_state = gs })
      static member event_stack_ =
        (fun t -> t.event_stack), (fun es t -> { t with event_stack = es })
      static member selected_ =
        (fun t -> t.selected), (fun s t -> { t with selected = s })
      static member companions_ =
        (fun t -> t.companions), (fun cs t -> { t with companions = cs })
      static member backgrounds_ =
        (fun t -> t.backgrounds), (fun bg t -> { t with backgrounds = bg })
      static member bgm_ =
        (fun t -> t.bgm), (fun bgm t -> { t with bgm = bgm })

  let enemies : t -> Enemies.t option =
    function
      { current_area = Battle (_, _, enemies) } ->
        Some enemies
    | _ ->
        Option.None

  let rounds : t -> Round option =
    function
      { current_area = Battle (_, rounds, _) } ->
        Some rounds
    | _ ->
        Option.None

  let get_selected : t -> SelectedCast =
    Optic.get t.selected_
  let set_selected : SelectedCast -> t -> t =
    Optic.set t.selected_
        
  
  (* Cards Ops *)
  let inline private get_card id cards =
    Map.tryFind id cards

  let inline casts scenario = scenario.cards.casts
  let inline get_cast id scenario =
    get_card id <| casts scenario

  let inline skills scenario = scenario.cards.skills
  let inline get_skill id scenario =
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
  let private flags_ = t.global_state_ >-> GlobalState.flags_
  let get_flags = Optic.get flags_
  let set_flags = Optic.set flags_
  let map_flags = Optic.map flags_

  let get_flag : Flag.Name -> t -> Flag.State =
    fun name ->
      get_flags >> Map.find name
  
  let set_flag : Flag.Name -> Flag.State -> t -> t =
    fun name value ->
      map_flags <| Map.add name value


  (* step ops *)
  exception InvalidStepIndexException

  let private steps_ = t.global_state_ >-> GlobalState.steps_
  let get_steps = Optic.get steps_
  let set_steps = Optic.set steps_
  let map_steps = Optic.map steps_

  let get_step : Step.Name -> t -> Step.State =
    fun name ->
      get_steps >> Map.find name

  let get_step_length : Step.Name -> t -> int option =
    fun name ->
      Optic.get (t.summary_ >-> Summary.t.steps_)
      >> Map.tryFind name
      >> Option.map (fun steps -> Array.length steps.steps)

  let set_step : Step.Name -> Step.State -> t -> t =
    fun name value scenario ->
      Maybe.c {
        let! length = get_step_length name scenario
        if value >= 0 && value < length then
          return map_steps (Map.add name value) scenario
      } |> function
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
    Adventurers.exists
      (fun card -> id = card.cast.property.id)
      scenario.companions
 
  let inline set_companion pos companion scenario =
    let companions = Adventurers.update pos companion scenario.companions in
    { scenario with companions = companions }

  let inline update_companion f pos scenario =
    let companions = Adventurers.update pos f scenario.companions in
    { scenario with companions = companions }

  (* Info ops *)
  let inline has_info id scenario =
    Set.contains id scenario.global_state.infos
  
  let inline add_info id scenario =
    let infos = Set.add id scenario.global_state.infos in
    let global_state = { scenario.global_state with infos = infos } in
    { scenario with global_state = global_state }

  let inline remove_info id scenario =
    let infos = Set.remove id scenario.global_state.infos in
    let global_state = { scenario.global_state with infos = infos } in
    { scenario with global_state = global_state }

 
  (* Background Ops *)
  let set_backgrounds = Optic.set t.backgrounds_
  let map_backgrounds = Optic.map t.backgrounds_

  let add_backgrounds backgrounds =
    map_backgrounds
      begin fun backgrounds' ->
        List.fold_right
          begin fun image acm ->
            if BackgroundImage.is_inherited image
            then image :: acm
            else [image]
          end
          backgrounds'
          backgrounds
      end

  let set_bgm = Optic.set t.bgm_