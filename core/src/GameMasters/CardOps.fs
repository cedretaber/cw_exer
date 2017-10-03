namespace CardWirthEngine.GameMasters

open CardWirthEngine.Utils
open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards
open CardWirthEngine.GameMasters.Cards.Adventurers
open CardWirthEngine.GameMasters.Party

module CardOps =

  let companion_exists : CastId -> State.t -> bool =
    fun id ->
      State.get_scenario_unsafe >> Scenario.has_companion id

  let add_companion : CastId -> StartAction -> State.t -> State.t = 
    fun id _start_action ->
    // TODO: start_action対応
      State.map_scenario <|
        fun scenario ->
          Scenario.get_cast id scenario
          |> Option.fold
            (fun _ companion -> Scenario.add_companion companion scenario)
            scenario
  
  let remove_companion : CastId -> State.t -> State.t =
    fun id ->
      // TODO: 戦闘中に離脱した際、同キャストの行動をキャンセルする。
      State.map_scenario <| Scenario.remove_companion id

  type PcOrEnemy
    = PC of Adventurers.Position * Cast.t
    | Enemy of EnemyId * Cast.t
  
  (*
     選択中:
       選択中のPCがn枚持っている
     ランダム:
       パーティ中の1人がn枚持っている。選択中のPCを変更。
       誰も持っていなかった場合、最後に評価されたPCに変更。
     パーティ全員:
       パーティ全員がそれぞれn枚ずつ持っている。選択中のPCをランダムに変更。
       1.50系のバグ: 全員隠蔽状態の場合は成功
     荷物袋:
       荷物袋の中にn枚入っている。
     全員と荷物袋:
       パーティ全体と荷物袋で、 *合計* n枚以上。
     フィールド全体:
       パーティ全体、荷物袋、敵全体の中の1人がn枚持っている。選択中のPCを変更。
       Note: 味方NPCを含まない
  *)
  let inline private exists count_card card_in_bag count target (state : State.t) =

    let inline check_card (card : Adventurers.CardState) = count_card card.cast >= count
    let inline check_card' card = count_card card >= count

    let count_backpack =
      lazy begin Party.count_card card_in_bag state.party end in

    match target with
      Range.Selected ->
        let bool =
          Option.fold
            (fun _ cast -> check_card' cast)
            false
            state.selected_cast in
        bool, state
    | Range.Random ->
        match
          Adventurers.try_find_with_position
            check_card
            state.adventurers with
          Option.None -> false, state
        | Some (pos, _) ->
            true, State.set_selected (Scenario.PC pos) state
    | Range.Party ->
        let pos, _ = State.get_random_pc state in
        Adventurers.forall check_card state.adventurers,
        State.set_selected (Scenario.PC pos) state
    | Range.Backpack ->
        count_backpack.Force () <= 0, state
    | Range.PartyAndBackpack ->
        let of_advs =
          Adventurers.fold
            (fun count card ->
              if count <= 0
              then count - count_card card.cast
              else 0)
            count
            state.adventurers in
        let rest = count - of_advs in
        rest <= 0 || rest - count_backpack.Force () <= 0,
        state
    | Range.Field ->
        let targets =
          seq {
            for p, c
              in Adventurers.to_seq_with_pos state.adventurers
                -> PC (p, c.cast)
            let scenario = State.get_scenario_unsafe state in
            let maybe_enemies = Scenario.get_enemies scenario in
            if Option.isSome maybe_enemies then
              let enemies = Option.get maybe_enemies in
              for i, c in Enemies.indexed enemies
                -> Enemy (i, c)
          } in
        let maybe_cast =
          Seq.tryFind
            (function
              PC (_, c) -> check_card' c
            | Enemy (_, c) -> check_card' c)
            targets in
        match maybe_cast with
          Some (PC (pos, _)) ->
            true, State.set_selected (Scenario.PC pos) state
        | Some (Enemy (idx, _)) ->
            true, State.set_selected (Scenario.Enemy idx) state
        | Option.None ->
            false, state
    | _ -> false, state

  let inline private maybe_exist get_card count_card card_in_bag count target state =
    state
    |> State.get_scenario_unsafe
    |> get_card
    |> Option.fold
      (fun _ card -> exists (count_card card) (card_in_bag card) count target state)
      (false, state)

  let inline private add add_card goods count target state =

    let inline update_cast pos cast (state : State.t) =
      let rest, cast' = add_card cast count in
      State.set_adventurer_at pos cast' state
      |> State.add_to_bag rest goods

    let inline update_npc cast = add_card cast count |> Pair.second

    let add_all_advs = lazy begin
      Adventurers.fold_with_pos
       (fun (state' : State.t, rest) (pos, card) ->
          let rest', cast' = add_card card.cast count in
          ( State.set_adventurer_at pos cast' state'
          , rest + rest'
          ))
        (state, 0)
        state.adventurers
      end in

    match target with
      Range.Selected ->
        let state' = State.force_selected state in
        let scenario = State.get_scenario_unsafe state' in
        match scenario.selected with
          Scenario.PC pos ->
            let card = Adventurers.get pos state.adventurers in
            update_cast pos card.cast state'
        | Scenario.Enemy id ->
            State.map_scenario
              (Scenario.map_enemy update_npc id)
              state
        | Scenario.Companion pos ->
            State.map_scenario
              (Scenario.update_companion update_npc pos)
              state
        | Scenario.None ->
            state
    | Range.Random ->
        let pos, cast = State.get_random_pc state in
        update_cast pos cast state
    | Range.Party ->
        let state', rest = add_all_advs.Force () in
        State.add_to_bag rest goods state'
    | Range.Backpack ->
        State.add_to_bag count goods state
    | Range.PartyAndBackpack ->
        let state', rest = add_all_advs.Force () in
        State.add_to_bag (count + rest) goods state'
    (* 取得・消失では「フィールド全体」は無効 *)
    | _ -> state

  let inline private maybe_add get_card add_card goods count target state =
    state
    |> State.get_scenario_unsafe
    |> get_card
    |> Option.fold
      (fun _ card -> add (add_card card) (goods card) count target state)
      state

  let inline private remove remove_from_cast goods count target state =

    let inline update_npc cast = remove_from_cast count cast

    let inline update_cast pos cast (state : State.t) =
      let cast' = update_npc cast in
      State.set_adventurer_at pos cast' state

    let remove_all_adv = lazy begin
      Adventurers.foldl_with_pos
        (fun (state : State.t) (pos, card) ->
          update_cast pos card.cast state)
        state
        state.adventurers
      end in

    match target with
      Range.Selected ->
        let state' = State.force_selected state in
        let scenario = State.get_scenario_unsafe state' in
        match scenario.selected with
          Scenario.PC pos ->
            let cast = State.get_adventurer_at pos state in
            update_cast pos cast state'
        | Scenario.Companion pos ->
            State.map_scenario
              (Scenario.update_companion update_npc pos)
              state
        | Scenario.Enemy id -> 
            State.map_scenario
              (Scenario.map_enemy update_npc id)
              state
        | Scenario.None -> state
    | Range.Random ->
        let pos, cast = State.get_random_pc state in
        update_cast pos cast state
    | Range.Party ->
        remove_all_adv.Force ()
    | Range.Backpack ->
        State.remove_from_bag count goods state
    | Range.PartyAndBackpack ->
        State.remove_from_bag count goods <| remove_all_adv.Force ()
    (* 取得・消失では「フィールド全体」は無効 *)
    | _ -> state
  
  let inline private maybe_remove get_card remove_from_cast goods count target state =
    state
    |> State.get_scenario_unsafe
    |> get_card
    |> Option.fold
      (fun _ card -> remove (remove_from_cast card) (goods card) count target state)
      state
  
  (* Items *)
  let item_exists : ItemId -> int -> Range -> State.t -> bool * State.t =
    fun id count target ->
      maybe_exist
        (Scenario.get_item id)
        Cast.item_count
        Party.Item
        count
        target

  let add_item : ItemId -> int -> Range -> State.t -> State.t =
    fun id count target ->
      maybe_add
        (Scenario.get_item id)
        (fun item cast count -> Cast.add_item count item cast)
        Party.Item
        count
        target

  let remove_item : ItemId -> RemoveCount -> Range -> State.t -> State.t =
    fun id count target ->
      maybe_remove
        (Scenario.get_item id)
        (fun item count -> Cast.remove_item count item)
        Party.Item
        count
        target

  (* Skill *)
  let skill_exists : SkillId -> int -> Range -> State.t -> bool * State.t =
    fun id count target ->
      maybe_exist
        (Scenario.get_skill id)
        Cast.count_skill
        Party.Skill
        count
        target

  let add_skill : SkillId -> int -> Range -> State.t -> State.t =
    fun id count target ->
      maybe_add
        (Scenario.get_skill id)
        (fun skill cast count -> Cast.add_skill count skill cast)
        Party.Skill
        count
        target

  let remove_skill : SkillId -> RemoveCount -> Range -> State.t -> State.t =
    fun id count target ->
      maybe_remove
        (Scenario.get_skill id)
        (fun skill count -> Cast.remove_skill count skill)
        Party.Skill
        count
        target

  (* Beast *)
  let beast_exists : BeastId -> int -> Range -> State.t -> bool * State.t =
    fun id count target ->
      maybe_exist
        (Scenario.get_beast id)
        Cast.beast_count
        Party.Beast
        count
        target

  let add_beast : BeastId -> int -> Range -> State.t -> State.t =
    fun id count target ->
      maybe_add
        (Scenario.get_beast id)
        (fun beast cast count -> Cast.add_beast count beast cast)
        Party.Beast
        count
        target

  let remove_beast : BeastId -> RemoveCount -> Range -> State.t -> State.t =
    fun id count target ->
      maybe_remove
        (Scenario.get_beast id)
        (fun skill count -> Cast.remove_beast count skill)
        Party.Beast
        count
        target

  (* Info *)
  let info_exists : InfoId -> State.t -> bool =
    fun id -> State.get_scenario_unsafe >> Scenario.has_info id

  let add_info : InfoId -> State.t -> State.t =
    Scenario.add_info >> State.map_scenario

  let remove_info : InfoId -> State.t -> State.t =
    Scenario.remove_info >> State.map_scenario