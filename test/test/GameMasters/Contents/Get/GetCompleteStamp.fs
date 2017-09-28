module CardWirthEngineTest.GameMasters.Contents.Get.CompleteStamp

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let get_completed_stamp =
  testList "CardWirthEngine.Scenario.Events.Content.GetCompleteStamp" [
    test "済み印を追加した場合" {
      let scenario_name = "scenario1" in
      let contents = GetCompleteStamp ([], scenario_name) in
      let state = State.Scenario (empty_scenario, minimal_party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.contains state'.global_data.completed_scenarii scenario_name "正しくシナリオ名が追加されていること"
    }
  ]