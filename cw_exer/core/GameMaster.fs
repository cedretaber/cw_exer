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
      | { state = State.OnEvent contents } -> read state contents input
      | { state = State.OnField } -> state, Void
      | { state = State.OnBattle } -> state, Void
      
  let rec read : State.t -> (Event.t * Content.t) list -> Input.t -> Output.t =
    fun state contents input ->
      match contents with
      | [] -> state, Output.None
      | (event, content) :: rest ->

        let end_scenario is_completed = state, Output.EndScenario is_completed in
        let gameover = state, Output.Gameover in
        let move_area area_id = state, Output.MoveArea area_id in
        let start_battle battle_id = state, Output.StartBattle battle_id in
        let effect_break = state, Output.Break in
        let next_line next = read state ((event, next) :: rest) Input.None in
        let end_line = read state rest Input.None in

        let go_start start_name =
          match Event.find start_name event with
          | Some next -> next_line next
          | None -> end_line

        match content with
        (* Terminal *)
        | Start ((next :: _), _) -> next_line next
        | Start _ -> end_line
        | StartBattle battle_id -> start_battle battle_id
        | End is_completed -> end_scenario is_completed
        | EndBadEnd -> gameover
        | ChangeArea area_id -> move_area area_id
        | EffectBreak -> effect_break
        | LinkStart start_name -> go_start start_name
        | LinkPackage package_id -> state, Void