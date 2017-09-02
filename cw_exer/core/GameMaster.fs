﻿namespace CardWirthEngine

open CardWirthEngine.Data
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters

module rec GameMaster =

  let Void = Output.None

  let run : State.t -> Input.t -> Output.t =
    fun state input ->
      match state with
        { state = State.OnEvent contents } -> read state contents input
      | { state = State.OnField } -> state, Void
      | { state = State.OnBattle } -> state, Void
      
  let rec read : State.t -> State.Event list -> Input.t -> Output.t =
    fun state contents input ->
      match contents with
        [] -> state, Output.EventEnd
      | State.Content (event, content) :: rest ->

        (* Step next contens *)
        let inline next_line state next =
          read state (State.Content (event, next) :: rest) Input.None
        let inline next_line' next =
          next_line state next

        let inline end_line state =
          read state rest Input.None
        let end_line' =
          end_line state
        
        (* Output with same state *)
        let inline output output =
          state, output
        
        (* Check step or flag *)
        let inline check state =
          function
            Content.CheckFlag (_, name) ->
              FlagOps.get name state
          | Content.CheckStep (_, name, right, cmp) ->
              let left = StepOps.get name state in
              Types.compare cmp left right
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
            State.Content
              ( event
              , Content.CallStart (nexts, start_name, true)
              ) in
          match Event.find start_name event with
            Some next ->
              read
                state
                (State.Content (event, next) :: callback :: rest)
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
            state, Output.LoadPackage package_id

        let inline call_package nexts package_id =
          let callback =
            State.Content
              ( event
              , Content.CallPackage (nexts, package_id, true)
              ) in
          if Terminal.is_loaded state package_id
          then
            read
              state
              (callback :: rest)
              Input.None
          else
            state, Output.LoadPackage package_id

        (* Branch *)
        let inline next_branch f nexts =
          let check =
            function
              key, next when f key ->
                if check state next then Some next else None
            | _, _ ->
                None in
          match Content.next check nexts with
            Some next -> next_line' next
          | None -> end_line'

        (* Message next *)
        let inline message_select selected nexts =
          match Content.select selected nexts with
            Some (_, next) -> next_line' next
          | None -> end_line'

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
        | TalkMessage (_, message), Input.None -> // メッセージ表示
            output <| Standard.message message
        | TalkMessage (nexts, _), Input.NextMessage selected -> // 選択肢選択
            message_select selected nexts
        | TalkMessage (nexts, _), _ ->
            through' <| List nexts
          
        | TalkDialog (_, dialog), Input.None ->
            output <| Standard.dialog dialog
        | TalkDialog (nexts, _), Input.NextMessage selected ->
            message_select selected nexts
        | TalkDialog (nexts, _), _ ->
            through' <| List nexts

        | PlayBgm (_, bgm), Input.None ->
            Standard.bgm state bgm
        | PlayBgm (nexts, _), _ ->
            through' <| Nexts nexts

        | PlaySound (_, sound), Input.None ->
            output <| Standard.sound sound
        | PlaySound (nexts, _), _ ->
            through' <| Nexts nexts

        | Wait (_, deciseconds), Input.None ->
            output <| Standard.wait deciseconds
        | Wait (nexts, _), _ ->
            through' <| Nexts nexts
            
        | ElaspeTime nexts, Input.None ->
            through
              (Standard.elaspe_time state)
              (Nexts nexts)

        | Effect (_, effect), Input.None ->
            Standard.effect state effect
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
            next_branch
              (fun name' -> name' = FlagOps.get name state)
              bools

        | SetFlag (nexts, name, flag), _ ->
            through
              (FlagOps.set name flag state)
              (Nexts nexts)

        | ReverseFlag (nexts, name), _ ->
            through
              (FlagOps.flip name state)
              (Nexts nexts)

        | SubstituteFlag (nexts, source, target), _ ->
            through
              (FlagOps.substitute source target state)
              (Nexts nexts)

        | BranchFlagCmp (bools, left, right), _ ->
            next_branch
              (fun bool -> bool = FlagOps.compare left right state)
              bools

        (* フラグのチェックは直前のコンテントで行う *)
        | CheckFlag (nexts, _), _ ->
            through' <| Nexts nexts

        (* ステップ比較は 以上=true, 未満=false *)
        | BranchStep (bools, name, value), _ ->
            next_branch
              (fun bool -> bool = (StepOps.get name state >= value))
              bools

        | SetStep (nexts, name, value), _ ->
            through
              (StepOps.set name value state)
              (Nexts nexts)

        | SetStepUp (nexts, name), _ ->
            through
              (StepOps.increment name state)
              (Nexts nexts)

        | SetStepDown (nexts, name), _ ->
            through
              (StepOps.decrement name state)
              (Nexts nexts)

        | SubstituteStep (nexts, source, target), _ ->
            through
              (StepOps.substitute source target state)
              (Nexts nexts)

        | BranchMultiStep (steps, name), _ ->
            next_branch
              (fun step -> step = StepOps.get name state)
              steps

        | BranchStepCmp (trios, left, right), _ ->
            next_branch
              (fun cmp -> Types.compare cmp left right)
              trios

        (* CheckFlagに同じ *)
        | CheckStep (nexts, _, _, _), _ ->
            through' <| Nexts nexts