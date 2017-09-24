namespace CardWirthEngineTest.GameMasters.Contents.Branch

module Info =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil
  open CardWirthEngineTest.GameMasters.Contents.Branch.TestUtil
  
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters
  open CardWirthEngine.GameMasters.Scenario
  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMaster

  [<Tests>]
  let branch_info =
    testList "CardWirthEngine.Scenario.Events.Content.BranchInfo" [
      test "情報カードを持っている場合" {
        let info_id = empty_info.id in
        let global_state =
          { empty_scenario.global_state with
              infos = Set.ofList [info_id] } in
        let scenario =
          { empty_scenario with
              global_state = global_state } in
        let state = make_empty_state scenario in
        let contents =
          BranchInfo
            ( true_false
            , info_id
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isTrue (get_flag flag_name scenario') "正しくtrueを返すこと"
      }
      
      test "情報カードを持っていない場合" {
        let info_id = empty_info.id in
        let state = make_empty_state empty_scenario in
        let contents =
          BranchInfo
            ( true_false
            , info_id
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isFalse (get_flag flag_name scenario') "正しくfalseを返すこと"
      }
    ]