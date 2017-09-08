namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards

module CardOps =
  open CardWirthEngine.GameMasters.Cards.Adventurers

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
  let inline private exists card_of_cast card_in_bag count (state : State.t) =

    let check_card = fun card -> card_of_cast card >= count in
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
              else count - card_of_cast cast)
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
              in Seq.zip
                   Adventurers.potisions
                   (Adventurers.to_seq state.adventurers)
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
    
  