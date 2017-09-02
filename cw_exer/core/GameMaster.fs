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

        let inline end_scenario is_completed =
          state, Output.EndScenario is_completed

        let gameover =
          state, Output.Gameover

        let inline move_area area_id =
          state, Output.MoveArea area_id

        let inline start_battle battle_id =
          state, Output.StartBattle battle_id

        let effect_break =
          state, Output.Break

        let inline next_line next =
          read state (State.Content (event, next) :: rest) Input.None

        let end_line =
          read state rest Input.None

        let inline through next =
          match next with
            Next None -> end_line
          | Next (Some next') -> next_line next'
          | Texts texts ->
            match Array.tryHead texts with
              Some next' -> next_line next'
            | None -> end_line
          | List list ->
            match List.tryHead list with
              Some (_, next') -> next_line next'
            | None -> end_line

        let inline go_start start_name =
          match Event.find start_name event with
            Some next -> next_line next
          | None -> end_line

        let inline call_start post_next start_name =
          match Event.find start_name event with
            Some next ->
              read state
                (State.Content (event, next) :: State.Content (event, post_next) :: rest)
                Input.None
          | None ->
              next_line post_next

        let inline go_package package_id =
          state, Void

        let inline call_package post_next package_id =
          state, Void

        let inline output_message message =
          state, Output.Message ("", [""])

        let inline output_dialog dialog =
          state, Output.Message ("", [""])

        let inline select_message selected texts =
          match Content.next' selected texts with
            Some next -> next_line next
          | None -> end_line

        match content, input with
        (* Terminal *)
          Start (next, _), _ ->
            through <| Next next

        | StartBattle battle_id, _ ->
            start_battle battle_id

        | End is_completed, _ ->
            end_scenario is_completed

        | EndBadEnd, _ ->
            gameover

        | ChangeArea area_id, _ ->
            move_area area_id

        | EffectBreak, _ ->
            effect_break

        | LinkStart start_name, _ ->
            go_start start_name

        | LinkPackage package_id, _ ->
            state, Void
        
        (* Standard *)
        | TalkMessage (_, message), Input.None -> // メッセージ表示
            output_message message
        | TalkMessage (nexts, _), Input.NextMessage selected -> // 選択肢選択
            select_message selected nexts
        | TalkMessage (nexts, _), _ ->
            through <| Texts nexts
          
        | TalkDialog (_, dialog), Input.None ->
            output_dialog dialog
        | TalkDialog (nexts, _), Input.NextMessage selected ->
            select_message selected nexts
        | TalkDialog (nexts, _), _ ->
            through <| Texts nexts

        | PlayBgm (_, bgm), Input.None ->
            { state with bgm = bgm }, Output.Bgm bgm
        | PlayBgm (next, _), _ ->
            through <| Next next

        | PlaySound (_, sound), Input.None ->
            state, Output.Sound sound
        | PlaySound (next, _), _ ->
            through <| Next next

        | Wait (_, deciseconds), Input.None ->
            state, Output.Wait deciseconds
        | Wait (next, _), _ ->
            through <| Next next

        | ElaspeTime next, _ ->
            through <| Next next

        | Effect (_, effect), Input.None ->
            state, Void
        | Effect (next, _), _ ->
            through <| Next next

        | CallStart (Some next, start_name), Input.None ->
            call_start next start_name
        | CallStart (None, start_name), Input.None ->
            go_start start_name
        | CallStart (next, _), _ ->
            through <| Next next

        | CallPackage (Some next, package_id), Input.None ->
            call_package next package_id
        | CallPackage (None, package_id), Input.None ->
            go_package package_id
        | CallPackage (next, _), _ ->
            through <| Next next