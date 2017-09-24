namespace CardWirthEngineTest.GameMasters.Contents.Branch

module CompleteStamp =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil
  open CardWirthEngineTest.GameMasters.Contents.Branch.TestUtil
  
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters
  open CardWirthEngine.GameMasters.Scenario
  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMaster

  [<Tests>]
  let branch_complete_stamp =
    testList "CardWirthEngine.Scenario.Events.Content.BranchCompleteStamp" [
      test "済印が存在する場合" {
        let completed_scenario_name = "scenario1" in
        let global_data =
          { empty_global_data with completed_scenarii = Set.ofList [completed_scenario_name] } in
        let state =
          State.Scenario (empty_scenario, minimal_party, global_data, state_random) in
        let contents =
          BranchCompleteStamp
            ( true_false
            , completed_scenario_name
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isTrue (get_flag flag_name scenario') "判定が成功すること"
      }
      
      test "済印が存在しない場合" {
        let completed_scenario_name = "scenario1" in
        let state =
          State.Scenario (empty_scenario, minimal_party, empty_global_data, state_random) in
        let contents =
          BranchCompleteStamp
            ( true_false
            , completed_scenario_name
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isFalse (get_flag flag_name scenario') "判定が失敗すること"
      }
    ]