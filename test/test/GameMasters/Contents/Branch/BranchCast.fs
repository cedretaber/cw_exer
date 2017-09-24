namespace CardWirthEngineTest.GameMasters.Contents.Branch

module Cast =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil
  open CardWirthEngineTest.GameMasters.Contents.Branch.TestUtil
  
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters
  open CardWirthEngine.GameMasters.Scenario
  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMaster

  [<Tests>]
  let branch_cast =
    testList "CardWirthEngine.Scenario.Events.Content.BranchCast" [
      test "キャストが存在する場合" {
        let scenario =
          { empty_scenario with
              global_state = check_flag_state;
              companions = one_adventurer } in
        let state = make_empty_state scenario in
        let contents =
          BranchCast
            ( true_false
            , empty_cast.property.id
            ) in
        let state', _ =
          read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isTrue (get_flag flag_name scenario') "キャストの存在を正しく識別できること"
      }
    ]