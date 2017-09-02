namespace CardWirthEngine

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

        let inline through state next =
          match next with
            Next None -> end_line state
          | Next (Some next') -> next_line state next'
          | Texts texts ->
            match Array.tryHead texts with
              Some next' -> next_line state next'
            | None -> end_line state
          | List list ->
            match List.tryHead list with
              Some (_, next') -> next_line state next'
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
              read state
                (State.Content (event, next) :: State.Content (event, post_next) :: rest)
                Input.None
          | None ->
              next_line' post_next

        let inline go_package package_id =
          state, Void

        let inline call_package post_next package_id =
          state, Void

        let inline select_message selected texts =
          match Content.next' selected texts with
            Some next -> next_line' next
          | None -> end_line'

        match content, input with
        (* Terminal *)
          Start (next, _), _ ->
            through' <| Next next

        | StartBattle battle_id, _ ->
            Terminal.start_battle state battle_id

        | End is_completed, _ ->
            Terminal.end_scenario state is_completed

        | EndBadEnd, _ ->
            Terminal.gameover state

        | ChangeArea area_id, _ ->
            Terminal.move_area state area_id

        | EffectBreak, _ ->
            Terminal.effect_break state

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
            through' <| Texts nexts
          
        | TalkDialog (_, dialog), Input.None ->
            output <| Standard.dialog dialog
        | TalkDialog (nexts, _), Input.NextMessage selected ->
            select_message selected nexts
        | TalkDialog (nexts, _), _ ->
            through' <| Texts nexts

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

        | CallStart (Some next, start_name), Input.None ->
            call_start next start_name
        | CallStart (None, start_name), Input.None ->
            go_start start_name
        | CallStart (next, _), _ ->
            through' <| Next next

        | CallPackage (Some next, package_id), Input.None ->
            call_package next package_id
        | CallPackage (None, package_id), Input.None ->
            go_package package_id
        | CallPackage (next, _), _ ->
            through' <| Next next