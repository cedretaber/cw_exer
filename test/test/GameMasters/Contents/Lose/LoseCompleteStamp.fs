module CardWirthEngineTest.GameMasters.Contents.Lose.CompleteStamp

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let lose_completed_stamp =
  testList "CardWirthEngine.Scenario.Events.Content.LoseCompleteStamp" [
    test "済み印を削除した場合" {
      let scenario_name = "scenario1" in
      let global_data = { empty_global_data with completed_scenarii = Set.ofList [scenario_name] } in
      let state = State.Scenario (empty_scenario, minimal_party, global_data, state_random) in
      let contents = LoseCompleteStamp ([], scenario_name) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.isEmpty (State.get_completeds state') "正しくシナリオ名が削除されていること"
    }
  ]