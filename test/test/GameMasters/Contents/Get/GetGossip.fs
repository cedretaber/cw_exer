module CardWirthEngineTest.GameMasters.Contents.Get.Gossip

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let get_gossip =
  testList "CardWirthEngine.Scenario.Events.Content.GetGossip" [
    test "ゴシップを追加した場合" {
      let gossip = "gossip1" in
      let contents = GetGossip ([], gossip) in
      let state = State.Scenario (empty_scenario, minimal_party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.contains (State.get_gossips state') gossip "正しくシナリオ名が追加されていること"
    }
  ]
