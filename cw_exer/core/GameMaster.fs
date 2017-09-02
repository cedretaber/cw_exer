﻿namespace CardWirthEngine

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

        let inline next_line state next =
          read state (State.Content (event, next) :: rest) Input.None
        let inline next_line' next =
          next_line state next

        let inline end_line state =
          read state rest Input.None
        let end_line' =
          end_line state

        let inline output output =
          state, output

        let inline next_branch key nexts =
          match Map.tryFind key nexts with
            Some next -> next_line' next
          | None -> end_line'

        let inline through state next =
          match next with
            Next None -> end_line state
          | Next (Some next') -> next_line state next'
          | Nexts nexts ->
            match Array.tryHead nexts with
              Some next' -> next_line state next'
            | None -> end_line state
          | Map map ->
            let maybe_next =
              Map.tryPick
                (fun _ next' -> Some next')
                map in
            match maybe_next with
              Some next' -> next_line state next'
            | None -> end_line state
        let inline through' next =
          through state next

        let inline go_start start_name =
          match Event.find start_name event with
            Some next -> next_line' next
          | None -> end_line'

        let inline call_start post_next start_name =
          match Event.find start_name event with
            Some next ->
              read
                state
                (State.Content (event, next) :: State.Content (event, post_next) :: rest)
                Input.None
          | None ->
              next_line' post_next

        let inline go_package package_id =
          if Terminal.is_loaded state package_id
          then
            read
              state
              rest
              Input.None
          else
            state, Output.LoadPackage package_id

        let inline call_package post_next package_id =
          if Terminal.is_loaded state package_id
          then
            read
              state
              (State.Content (event, post_next) :: rest)
              Input.None
          else
            state, Output.LoadPackage package_id

        let inline select_message selected texts =
          match Content.next' selected texts with
            Some next -> next_line' next
          | None -> end_line'

        match content, input with
        (* Terminal *)
          Start (next, _), _ ->
            through' <| Next next

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
            select_message selected nexts
        | TalkMessage (nexts, _), _ ->
            through' <| Nexts nexts
          
        | TalkDialog (_, dialog), Input.None ->
            output <| Standard.dialog dialog
        | TalkDialog (nexts, _), Input.NextMessage selected ->
            select_message selected nexts
        | TalkDialog (nexts, _), _ ->
            through' <| Nexts nexts

        | PlayBgm (_, bgm), Input.None ->
            Standard.bgm state bgm
        | PlayBgm (next, _), _ ->
            through' <| Next next

        | PlaySound (_, sound), Input.None ->
            output <| Standard.sound sound
        | PlaySound (next, _), _ ->
            through' <| Next next

        | Wait (_, deciseconds), Input.None ->
            output <| Standard.wait deciseconds
        | Wait (next, _), _ ->
            through' <| Next next
            
        | ElaspeTime next, Input.None ->
            through
              (Standard.elaspe_time state)
              (Next next)

        | Effect (_, effect), Input.None ->
            Standard.effect state effect
        | Effect (next, _), _ ->
            through' <| Next next

        | CallStart (Some next, start_name), _ ->
            call_start next start_name
        | CallStart (None, start_name), _ ->
            go_start start_name

        | CallPackage (Some next, package_id), _ ->
            call_package next package_id
        | CallPackage (None, package_id), _ ->
            go_package package_id

        (* Data *)
        | BranchFlag (bools, name), _ ->
            next_branch
              (FlagOps.get name state)
              bools

        | SetFlag (next, name, flag), _ ->
            through
              (FlagOps.set name flag state)
              (Next next)

        | ReverseFlag (next, name), _ ->
            through
              (FlagOps.flip name state)
              (Next next)

        | SubstituteFlag (next, source, target), _ ->
            through
              (FlagOps.substitute source target state)
              (Next next)

        | BranchFlagCmp (bools, left, right), _ ->
            next_branch
              (FlagOps.compare left right state)
              bools

        | CheckFlag (next, name), _ ->
            if FlagOps.get name state
            then
              through' <| Next next
            else
              end_line'
        
        
        of Next * flag : Flag.Name



        | BranchMultiStep of Steps * step : Step.Name
        | BranchStep of Bools * step : Step.Name * value : Step.State
        | SetStep of Next * step : Step.Name * value : Step.State
        | SetStepUp of Next * step : Step.Name
        | SetStepDown of Next * step : Step.Name
        | SubstituteStep of Next * source : Step.State * target : Step.State
        | BranchStepCmp of Trios * left : Step.Name * right : Step.Name
        | CheckStep of Next * step : Step.Name