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

        let end_scenario is_completed = state, Output.EndScenario is_completed in
        let gameover = state, Output.Gameover in
        let move_area area_id = state, Output.MoveArea area_id in
        let start_battle battle_id = state, Output.StartBattle battle_id in
        let effect_break = state, Output.Break in

        let next_line next = read state (State.Content (event, next) :: rest) Input.None in
        let end_line = read state rest Input.None in

        let through =
          function
            [] -> end_line
          | next :: _ -> next_line next
        let through' =
          function
            [] -> end_line
          | (_, next) :: _ -> next_line next

        let go_start start_name =
          match Event.find start_name event with
            Some next -> next_line next
          | None -> end_line

        let call_start post_next start_name =
          match Event.find start_name event with
            Some next ->
              read state
                (State.Content (event, next) :: State.Content (event, post_next) :: rest)
                Input.None
          | None ->
              next_line post_next

        let select_message selected nexts =
          match find_next selected nexts with
            Some(next) -> next_line next
          | None -> end_line

        match content, input with
        (* Terminal *)
          Start (nexts, _), _ ->
            through nexts

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
            state, Output.Message ("", [""])
        | TalkMessage (nexts, _), Input.NextMessage selected -> // 選択肢選択
            select_message selected nexts
        | TalkMessage (nexts, _), _ ->
            through' nexts
          
        | TalkDialog (_, message), Input.None ->
            state, Output.Message ("", [""])
        | TalkDialog (nexts, _), Input.NextMessage selected ->
            select_message selected nexts
        | TalkDialog (nexts, _), _ ->
            through' nexts

        | PlayBgm (_, bgm), Input.None ->
            { state with bgm = bgm }, Output.Bgm bgm
        | PlayBgm (nexts, _), _ ->
            through nexts

        | PlaySound (_, sound), Input.None ->
            state, Output.Sound sound
        | PlaySound (nexts, _), _ ->
            through nexts

        | Wait (_, deciseconds), Input.None ->
            state, Output.Wait deciseconds
        | Wait (nexts, _), _ ->
            through nexts

        | ElaspeTime nexts, _ ->
            through nexts

        | Effect (_, effect), Input.None ->
            state, Void
        | Effect (nexts, _), _ ->
            through nexts

        | CallStart (next :: _, start_name), Input.None ->
            call_start next start_name
        | CallStart ([], start_name), Input.None ->
            go_start start_name
        | CallStart (nexts, _), _ ->
            through nexts

        | CallPackage (next :: _, package_id), Input.None ->
            state, Void
        | CallPackage ([], package_id), Input.None ->
            state, Void
        | CallPackage (nexts, _), _ ->
            through nexts