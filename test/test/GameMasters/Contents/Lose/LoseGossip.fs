module CardWirthEngineTest.GameMasters.Contents.Lose.Gossip

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let get_gossip =
  testList "CardWirthEngine.Scenario.Events.Content.LoseGossip" [
    test "ゴシップを削除した場合" {
      let gossip = "gossip1" in
      let global_data = { empty_global_data with gossips = Set.ofList [gossip] } in
      let state = State.Scenario (empty_scenario, minimal_party, global_data, state_random) in
      let contents = LoseGossip ([], gossip) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.isEmpty state'.global_data.gossips "正しくゴシップが削除されていること"
    }
  ]
