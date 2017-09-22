namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Utils
open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
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
      State.update_scenarion begin
        fun scenario ->
          Scenario.get_cast id scenario
          |> Option.fold
            (fun _ companion -> Scenario.add_companion companion scenario)
            scenario
      end
  
  let remove_companion : CastId -> State.t -> State.t =
    fun id ->
      State.update_scenarion <| Scenario.remove_companion id

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

    let check_card =
      fun card -> count_card card >= count in
    let count_backpack =
      lazy(Party.count_card card_in_bag state.party) in

    match target with
      Range.Selected ->
        let bool =
          Option.fold
            (fun _ -> check_card)
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
            (fun count cast ->
              if count <= 0
              then 0
              else count - count_card cast)
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
                -> PC (p, c)
            let scenario = State.get_scenario_unsafe state in
            let maybe_enemies = Scenario.enemies scenario in
            if Option.isSome maybe_enemies then
              let enemies = Option.get maybe_enemies in
              for i, c in Enemies.indexed enemies
                -> Enemy (i, c)
          } in
        let maybe_cast =
          Seq.tryFind
            (function
              PC (_, c) -> check_card c
            | Enemy (_, c) -> check_card c)
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

    let update_npc =
      fun cast -> add_card cast count |> Pair.second in

    let add_all_advs = lazy(
      Adventurers.fold_with_pos
       (fun (state' : State.t, rest) (pos, cast) ->
          let rest', cast' = add_card cast count in
          ( State.set_adventurer_at pos cast' state'
          , rest + rest'
          ))
        (state, 0)
        state.adventurers
      ) in

    match target with
      Range.Selected ->
        let state', cast =
          State.get_selected_or_random state in
        let scenario = State.get_scenario_unsafe state' in
        match scenario.selected with
          Scenario.PC pos ->
            update_cast pos cast state'
        | Scenario.Enemy id ->
            State.update_scenarion
              (Scenario.update_enemy update_npc id)
              state
        | Scenario.Companion pos ->
            State.update_scenarion
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


  let inline private remove remove_from_cast goods count target state =

    let update_npc =
      fun cast -> remove_from_cast count cast in

    let inline update_cast pos cast (state : State.t) =
      let cast' = update_npc cast in
      State.set_adventurer_at pos cast' state

    let remove_all_adv = lazy(
      Adventurers.foldl_with_pos
        (fun (state : State.t) (pos, cast) ->
          update_cast pos cast state)
        state
        state.adventurers
      ) in

    match target with
      Range.Selected ->
        let state', cast =
          State.get_selected_or_random state in
        let scenario = State.get_scenario_unsafe state' in
        match scenario.selected with
          Scenario.PC pos ->
            update_cast pos cast state'
        | Scenario.Companion pos ->
            State.update_scenarion
              (Scenario.update_companion update_npc pos)
              state
        | Scenario.Enemy id -> 
            State.update_scenarion
              (Scenario.update_enemy update_npc id)
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
        remove_all_adv.Force ()
        |> State.remove_from_bag count goods
    (* 取得・消失では「フィールド全体」は無効 *)
    | _ -> state
  
  
  (* Items *)
  let item_exists : ItemId -> int -> Range -> State.t -> bool * State.t =
    fun id count target ->
      maybe_exist
        (Scenario.get_item id)
        Cast.item_count
        Party.Item
        count
        target

  let add_item id count target state =
    state
    |> State.get_scenario_unsafe
    |> Scenario.get_item id
    |> Option.fold
      (fun _ item ->
        add
          (fun cast count ->
            Cast.add_item count item cast)
          (Party.Item item)
          count
          target
          state)
      state

  let remove_item id count target state =
    state
    |> State.get_scenario_unsafe
    |> Scenario.get_item id
    |> Option.fold
      (fun _ item ->
        remove
          (fun count cast ->
            Cast.remove_item count item cast)
          (Party.Item item)
          count
          target
          state)
      state

  (* Skill *)
  let skill_exists : SkillId -> int -> Range -> State.t -> bool * State.t =
    fun id count target ->
      maybe_exist
        (Scenario.get_skilll id)
        Cast.count_skill
        Party.Skill
        count
        target

  let add_skill id count target state =
    state
    |> State.get_scenario_unsafe
    |> Scenario.get_skilll id
    |> Option.fold
      (fun _ skill ->
        add
          (fun cast count ->
            Cast.add_skill count skill cast)
          (Party.Skill skill)
          count
          target
          state)
      state

  (* Beast *)
  let beast_exists : BeastId -> int -> Range -> State.t -> bool * State.t =
    fun id count target ->
      maybe_exist
        (Scenario.get_beast id)
        Cast.beast_count
        Party.Beast
        count
        target

  (* Info *)
  let info_exists id state =
    State.get_scenario_unsafe state
    |> Scenario.has_info id