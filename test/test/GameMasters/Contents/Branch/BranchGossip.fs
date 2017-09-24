namespace CardWirthEngineTest.GameMasters.Contents.Branch

module Gossip =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil
  open CardWirthEngineTest.GameMasters.Contents.Branch.TestUtil
  
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters
  open CardWirthEngine.GameMasters.Scenario
  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMaster

  [<Tests>]
  let branch_gossip =
    testList "CardWirthEngine.Scenario.Events.Content.BranchGossip" [
      test "ゴシップが存在する場合" {
        let gossip = "gossip1" in
        let global_data = { empty_global_data with gossips = Set.ofList [gossip] } in
        let state =
          State.Scenario (empty_scenario, minimal_party, global_data, state_random) in
        let contents =
          BranchGossip
            ( true_false
            , gossip
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isTrue (get_flag flag_name scenario') "判定が成功すること"
      }

      test "ゴシップが存在しない場合" {
        let gossip = "gossip1" in
        let state =
          State.Scenario (empty_scenario, minimal_party, empty_global_data, state_random) in
        let contents =
          BranchGossip
            ( true_false
            , gossip
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isFalse (get_flag flag_name scenario') "判定が失敗すること"
      }
    ]