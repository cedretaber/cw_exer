namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards

module CardOps =
  open CardWirthEngine.GameMasters.Cards.Adventurers
  open CardWirthEngine.GameMasters.Party

  let companion_exists = State.has_companion

  let add_companion id (state : State.t) =
    let companion =
      State.get_cast id state in
    State.add_companion companion state
  
  let remove_companion = State.remove_companion

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
  let inline private exists count_card card_in_bag count (state : State.t) =

    let check_card = fun card -> count_card card >= count in
    let count_backpack = lazy(Party.count_card card_in_bag state.party)

    function
      Range.Selected ->
        state, Option.fold
          (fun _ -> check_card)
          false
          state.selected
    | Range.Random ->
        match
          Adventurers.try_find_with_position
            check_card
            state.adventurers with
          Option.None -> state, false
        | Some (pos, _) ->
            State.set_selected (State.PC pos) state, true
    | Range.Party ->
        let pos, _ = State.get_random_pc state in
        ( State.set_selected (State.PC pos) state
        , Adventurers.forall check_card state.adventurers
        )
    | Range.Backpack ->
        state, count_backpack.Force () <= 0
    | Range.PartyAndBackpack ->
        let of_advs =
          Adventurers.fold
            (fun count cast ->
              if count <= 0
              then 0
              else count - count_card cast)
            count
            state.adventurers
        let rest = count - of_advs
        ( state
        , rest <= 0 || rest - count_backpack.Force () <= 0
        )
    | Range.Field ->
        let targets =
          seq {
            for p, c
              in Adventurers.to_seq_with_pos state.adventurers
                -> PC (p, c)
            if Option.isSome state.enemies then
              let enemies = Option.get state.enemies in
              for i, c in Map.toSeq enemies
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
            ( State.set_selected (State.PC pos) state
            , true
            )
        | Some (Enemy (idx, _)) ->
            ( State.set_selected (State.Enemy idx) state
            , true
            )
        | Option.None ->
            (state, false)
    | _ -> state, false

  let inline private add add_card goods count (state : State.t) =

    let inline update_cast pos cast (state : State.t) =
      let rest, cast' = add_card cast count in
      let party = Party.updated_adventurers pos cast' state.party in
      let state' = State.set_party party state in
      State.add_to_bag rest goods state'

    let add_all_advs = lazy(
      Adventurers.fold_with_pos
       (fun (state' : State.t, rest) (pos, cast) ->
          let rest', cast' = add_card cast count in
          let party = Party.updated_adventurers pos cast' state'.party in
          State.set_party party state', rest + rest')
        (state, 0)
        state.adventurers)

    function
      Range.Selected ->
        let state', cast =
          State.get_selected_or_random state in
        match state.selected_pos with
          State.PC pos ->
            update_cast pos cast state'
        | _ -> raise <| FatalError "Invalid selected PC."
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
    (* 古いシステム。 *)
    | Range.Field ->
        State.add_to_bag count goods state
    | _ -> state
  