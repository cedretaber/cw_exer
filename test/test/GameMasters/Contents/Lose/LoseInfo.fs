module CardWirthEngineTest.GameMasters.Contents.Lose.Info

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let remove_info =
  testList "CardWirthEngine.Scenario.Events.Content.LoseInfo" [
    test "情報カードを削除した場合" {
      let info = empty_info in
      let id = info.id in
      let contents = LoseInfo ([], id) in
      let scenario = Scenario.set_infos_gd (Set.ofList [id]) empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.isEmpty
        (State.get_scenario_unsafe state').global_state.infos
        "指定されたIDのカードが全て削除されていること"
    }
  ]