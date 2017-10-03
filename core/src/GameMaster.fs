namespace CardWirthEngine

open CardWirthEngine.Util
open CardWirthEngine.Utils
open CardWirthEngine.Data.Types
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Branch

module GameMaster =
  let Void = Output.None

  let rec run : State.t -> Input.t -> Output.t =
    fun state input ->
      match state with
        State.Scenario ({ event_stack = [] }, _, _, _) ->
          state, Void
      | State.Scenario ({ event_stack = contents }, _, _, _) ->
          read state contents input

  and read : State.t -> Scenario.Event list -> Input.t -> Output.t =
    fun state contents input ->
      match contents with
        [] ->
          Output.t state Output.EventEnd
      | Scenario.Content (event, content) :: rest ->
          read_content state event content rest input
      | Scenario.Action :: rest ->
          read state rest input

  and private read_content state event content rest input =
    (* Step next contens *)
    let inline next_line state next =
      read state (Scenario.Content (event, next) :: rest) Input.None
    let inline next_line' next =
      next_line state next

    let inline end_line state =
      read state rest Input.None
    let end_line' =
      end_line state

    (* Output with same state *)
    let inline output output =
      Output.t state output

    (* Check step or flag *)
    let inline check state =
      function
        Content.CheckFlag (_, name) ->
          FlagOps.get name state
      | Content.CheckStep (_, name, right, cmp) ->
          let left = StepOps.get name state in
          Comparison.compare cmp left right
      | _ -> true

    (* Through next contents *)
    let inline next_content state nexts =
      match List.tryFind (check state) nexts with
        Some next' -> next_line state next'
      | None -> end_line state
    let inline next_content' state nexts =
      let maybe_next =
        List.tryFind
          (function _, t -> check state t)
          nexts in
      match maybe_next with
        Some (_, next') -> next_line state next'
      | None -> end_line state

    let inline through state next =
      match next with
        Nexts nexts -> next_content state nexts
      | List list -> next_content' state list
    let inline through' next =
      through state next

    (* Start Contents *)
    let inline go_start start_name =
      match Event.find start_name event with
        Some next -> next_line' next
      | None -> end_line'

    let inline call_start nexts start_name =
      let callback =
        Scenario.Content
          ( event
          , Content.CallStart (nexts, start_name, true)
          ) in
      match Event.find start_name event with
        Some next ->
          read
            state
            (Scenario.Content (event, next) :: callback :: rest)
            Input.None
      | None ->
          read state (callback :: rest) Input.None

    (* Package Contents *)
    let inline go_package package_id =
      if Terminal.is_loaded state package_id
      then
        read
          state
          (rest)
          Input.None
      else
        Output.t state <| Output.LoadPackage package_id

    let inline call_package nexts package_id =
      if Terminal.is_loaded state package_id
      then
        let callback =
          Scenario.Content
            ( event
            , Content.CallPackage (nexts, package_id, true)
            ) in
        read
          state
          (callback :: rest)
          Input.None
      else
        Output.t state <| Output.LoadPackage package_id

    (* Branch *)
    let inline next_branch state f nexts =
      let check =
        function
          key, next when f key ->
            if check state next then Some next else None
        | _, _ ->
            None in
      match Content.next check nexts with
        Some next -> next_line state next
      | None -> end_line state
    let inline next_branch' f nexts =
      next_branch state f nexts

    (* Text selection *)
    let inline select_message selected nexts =
      match Content.select selected nexts with
        Some (_, next) -> next_line' next
      | None -> end_line'

    (* Multi selection *)
    let inline select_multi selected nexts =
      match Content.select selected nexts with
        Some next -> next_line' next
      | None -> end_line'

    (* Flag change *)
    let inline flag_change state flag_name flag_state =
      Output.t state <| Output.Flag (flag_name, flag_state)

    (* Money change *)
    let inline money_change state balance =
      Output.t state <| Output.Money balance

    match content, input with
    (* Terminal *)
      Start (nexts, _), _ ->
        through' <| Nexts nexts

    | StartBattle battle_id, _ ->
        output <| Terminal.start_battle battle_id

    | End is_completed, _ ->
        output <| Terminal.end_scenario is_completed

    | EndBadEnd, _ ->
        output Terminal.gameover

    | ChangeArea area_id, _ ->
        output <| Terminal.move_area area_id

    | EffectBreak, _ ->
        output Terminal.effect_break

    | LinkStart start_name, _ ->
        go_start start_name

    | LinkPackage package_id, _ ->
        go_package package_id

    (* Standard *)
    | TalkMessage (nexts, message), Input.None -> // メッセージ表示
        output <| Standard.message message nexts state
    | TalkMessage (nexts, _), Input.NextMessage selected -> // 選択肢選択
        select_message selected nexts
    | TalkMessage (nexts, _), _ ->
        through' <| List nexts
      
    | TalkDialog (nexts, dialog), Input.None ->
        output <| Standard.dialog dialog nexts state
    | TalkDialog (nexts, _), Input.NextMessage selected ->
        select_message selected nexts
    | TalkDialog (nexts, _), _ ->
        through' <| List nexts

    | PlayBgm (_, bgm, play), Input.None ->
        Output.t <|| Standard.bgm state bgm play
    | PlayBgm (nexts, _, _), _ ->
        through' <| Nexts nexts

    | PlaySound (_, sound, play), Input.None ->
        output <| Standard.sound sound play
    | PlaySound (nexts, _, _), _ ->
        through' <| Nexts nexts

    | Wait (_, deciseconds), Input.None ->
        output <| Standard.wait deciseconds
    | Wait (nexts, _), _ ->
        through' <| Nexts nexts
        
    | ElaspeTime nexts, _ ->
        through
          (Standard.elaspe_time state)
          (Nexts nexts)

    | Effect (_, effect), Input.None ->
        output <| Standard.effect effect
    | Effect (nexts, _), _ ->
        through' <| Nexts nexts
        
    | CallStart ([], start_name, false), _ ->
        go_start start_name
    | CallStart (nexts, start_name, false), _ ->
        call_start nexts start_name
    | CallStart (nexts, _, true), _ ->
        through' <| Nexts nexts
        
    | CallPackage ([], package_id, false), _ ->
        go_package package_id
    | CallPackage (nexts, package_id, false), _ ->
        call_package nexts package_id
    | CallPackage (nexts, _, true), _ ->
        through' <| Nexts nexts

    (* Data *)
    | BranchFlag (bools, name), _ ->
        next_branch' ((=) <| FlagOps.get name state) bools
          
    | SetFlag (_, name, flag), Input.None ->
        flag_change
          (FlagOps.set name flag state)
          name flag
    | SetFlag (nexts, _, _), _ ->
        through' <| Nexts nexts
        
    | ReverseFlag (_, name), Input.None ->
        let state', flag = FlagOps.flip name state in
        flag_change state' name flag
    | ReverseFlag (nexts, _), _ ->
        through' <| Nexts nexts
        
    | SubstituteFlag (_, source, target), Input.None ->
        let state', flag = FlagOps.substitute source target state in
        flag_change state' target flag
    | SubstituteFlag (nexts, _, _), _ ->
        through' <| Nexts nexts

    | BranchFlagCmp (bools, left, right), _ ->
        next_branch'
          ((=) <| FlagOps.compare left right state)
          bools

    (* フラグのチェックは直前のコンテントで行う *)
    | CheckFlag (nexts, _), _ ->
        through' <| Nexts nexts

    (* ステップ比較は 以上=true, 未満=false *)
    | BranchStep (bools, name, value), _ ->
        next_branch'
          ((=) (StepOps.get name state >= value))
          bools

    | SetStep (nexts, name, value), _ ->
        through
          (StepOps.set name value state)
          <| Nexts nexts

    | SetStepUp (nexts, name), _ ->
        through
          (StepOps.increment name state)
          <| Nexts nexts

    | SetStepDown (nexts, name), _ ->
        through
          (StepOps.decrement name state)
          <| Nexts nexts

    | SubstituteStep (nexts, source, target), _ ->
        through
          (StepOps.substitute source target state)
          <| Nexts nexts

    | BranchMultiStep (steps, name), _ ->
        next_branch'
          ((=) <| StepOps.get name state)
          steps

    | BranchStepCmp (trios, left, right), _ ->
        next_branch'
          (fun cmp -> Comparison.compare cmp left right)
          trios

    (* CheckFlagに同じ *)
    | CheckStep (nexts, _, _, _), _ ->
        through' <| Nexts nexts

    (* Utility *)
    | BranchSelect (bools, _), Input.SelectPlayerCharactor index ->
        next_branch
          (Branch.Select.set_selected_pc index state)
          id
          bools
    | BranchSelect (bools, _), Input.Cancel ->
        next_branch' not bools
    | BranchSelect (bools, select), _ ->
        match Branch.Select.select select state with
          _, (Output.SelectPlayerCharactor _ as out) ->
            output out
        | new_state, Output.None ->
            next_branch new_state id bools
        | _, _ ->
            next_branch' id bools

    | BranchAbility (bools, ability), _ ->
        let state', bool =
          Branch.Adventurer.judge ability state in
        next_branch state' ((=) bool) bools

    | BranchRandom (bools, percent), _ ->
        next_branch'
          ((=) <| Branch.Random.dice (int percent) state)
          bools

    | BranchMultiRandom nexts, _ ->
        select_multi
          (Branch.Random.multi nexts state)
          nexts

    | BranchLevel (bools, target, level), _ ->
        next_branch'
          ((=) <| Branch.Adventurer.level target level state)
          bools

    | BranchStatus (bools, target, status), _ ->
        let new_state, bool =
          Branch.Adventurer.status target status state in
        next_branch new_state ((=) bool) bools

    | BranchPartyNumber (bools, value), _ ->
        next_branch'
          ((=) <| Branch.Adventurer.party_count value state)
          bools

    | BranchArea (bools, id), _ ->
        next_branch'
          ((=) <| Branch.AreaOrBattle.is_area_in id state)
          bools

    | BranchBattle (bools, id), _ ->
        next_branch'
          ((=) <| Branch.AreaOrBattle.is_battle_in id state)
          bools

    | BranchIsBattle bools, _ ->
        next_branch'
          ((=) <| Branch.AreaOrBattle.is_battle state)
          bools

    | BranchRandomSelect (bools, condition), _ ->
        let state', bool =
          Branch.Adventurer.random_select condition state in
        next_branch state' ((=) bool) bools

    | BranchRound (bools, value, cmp), _ ->
        next_branch'
          ((=) <| Branch.AreaOrBattle.round cmp value state)
          bools
    
    // TODO: カード系コンテントは、異なるシナリオ由来のイベントであれば動かないようにするべき
    (* Branch *)
    | BranchCast (bools, id), _ ->
        next_branch'
          ((=) <| CardOps.companion_exists id state)
          bools

    | BranchItem (bools, id, count, range), _ ->
        let bool, state' = CardOps.item_exists id count range state in
        next_branch state' ((=) bool) bools

    | BranchSkill (bools, id, count, range), _ ->
        let bool, state' = CardOps.skill_exists id count range state in
        next_branch state' ((=) bool) bools

    | BranchInfo (bools, id), _ ->
        next_branch'
          ((=) <| CardOps.info_exists id state)
          bools

    | BranchBeast (bools, id, count, range), _ ->
        let bool, state' = CardOps.beast_exists id count range state in
        next_branch state' ((=) bool) bools

    | BranchMoney (bools, value), _ ->
        next_branch'
          ((=) <| Party.has_money value state.party)
          bools

    | BranchCoupon (bools, range, matching_type, values), _ ->
        let state', bool =
          Adventurer.has_coupon range matching_type values state in
        next_branch state' ((=) <| bool) bools

    // TODO: 未実装
    | BranchMultiCoupon (texts, _), _ ->
        through state <| Nexts (List.map Pair.second texts)

    | BranchCompleteStamp (bools, value), _ ->
        next_branch'
          ((=) <| State.is_completed value state)
          bools

    | BranchGossip (bools, value), _ ->
        next_branch'
          ((=) <| State.has_gossip value state)
          bools

    | BranchKeyCode (bools, { range = range
                            ; card_type = card_type
                            ; key_code = key_code
                            }), _ ->
        let state', bool =
          KeyCode.has_key_code range card_type key_code state in
        next_branch state' ((=) <| bool) bools
    
    (* Get *)
    | GetCast (nexts, id, start_action), _ ->
        next_content
          (CardOps.add_companion id start_action state)
          nexts

    | GetItem (nexts, id, target, value), _ ->
        next_content
          (CardOps.add_item id value target state)
          nexts

    | GetSkill (nexts, id, target, value), _ ->
        next_content
          (CardOps.add_skill id value target state)
          nexts

    | GetInfo (nexts, id), _ ->
        next_content
          (CardOps.add_info id state)
          nexts

    | GetBeast (nexts, id, target, value), _ ->
        next_content
          (CardOps.add_beast id value target state)
          nexts

    | GetMoney (_, value), Input.None ->
        let balance, state' = State.add_money value state in
        money_change state' balance
    | GetMoney (nexts, _), _ ->
        through' <| Nexts nexts

    | GetCoupon (_, target, point, value), Input.None ->
        CouponOps.add_coupon target value point state, Output.Coupon
    | GetCoupon (nexts, _, _, _), _ ->
        through' <| Nexts nexts

    | GetCompleteStamp (nexts, value), _ ->
        through
          (State.get_completed value state)
          <| Nexts nexts

    | GetGossip (nexts, value), _ ->
        through
          (State.get_gossip value state)
          <| Nexts nexts

    (* Lost *)
    | LoseCast (nexts, id), _ ->
        through
          (CardOps.remove_companion id state)
          <| Nexts nexts

    | LoseItem (nexts, id, target, count), _ ->
        through
          (CardOps.remove_item id count target state)
          <| Nexts nexts

    | LoseSkill (nexts, id, target, count), _ ->
        through
          (CardOps.remove_skill id count target state)
          <| Nexts nexts

    | LoseInfo (nexts, id), _ ->
        through
          (CardOps.remove_info id state)
          <| Nexts nexts

    | LoseBeast (nexts, id, target, count), _ ->
        through
          (CardOps.remove_beast id count target state)
          <| Nexts nexts

    | LoseMoney (_, value), Input.None ->
        let balance, state' = State.add_money (-value) state in
        money_change state' balance
    | LoseMoney (nexts, _), _ ->
        through' <| Nexts nexts

    | LoseCoupon (_, target, name), Input.None ->
        CouponOps.remove_coupon target name state, Output.Coupon
    | LoseCoupon (nexts, _, _), _ ->
        through' <| Nexts nexts

    | LoseCompleteStamp (nexts, scenario_name), _ ->
        through
          (State.lose_completed scenario_name state)
          <| Nexts nexts
          
    | LoseGossip (nexts, gossip_name), _ ->
        through
          (State.lose_gossip gossip_name state)
          <| Nexts nexts
    
    (* Visual *)
    | ShowParty _, Input.None ->
        state, Output.PartyUp
    | ShowParty nexts, _ ->
        through' <| Nexts nexts

    | HideParty _, Input.None ->
        state, Output.PartyDown
    | HideParty nexts, _ ->
        through' <| Nexts nexts

    | ChangeBgImage (_, transition_speed, transition, images), Input.None ->
        let state' = State.change_background images state in
        state', Output.ChangeBackground (transition_speed, transition, State.get_backgrounds state')
    | ChangeBgImage (nexts, _, _, _), _ ->
        through' <| Nexts nexts

    (*
    | ChangeBgImage of Nexts
    | MoveBgImage of Nexts
    | ReplaceBgImage of Nexts
    | LoseBgImage of Nexts
    | Redisplay of Nexts
    *)