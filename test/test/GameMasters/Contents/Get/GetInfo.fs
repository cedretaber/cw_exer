namespace CardWirthEngineTest.GameMasters.Contents.Get

module Info =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil
  
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters
  open CardWirthEngine.GameMasters.Scenario
  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMaster

  [<Tests>]
  let get_info =
    testList "CardWirthEngine.Scenario.Events.Content.GetInfo" [
      test "キャストを追加した場合" {
        let info = empty_info in
        let id = info.id in
        let contents = GetInfo ([], id) in
        let scenario = Scenario.set_infos_gd (Set.ofList [id]) empty_scenario in
        let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state'
        Expect.contains (scenario'.global_state.infos) id "正しく追加されること"
      }
    ]