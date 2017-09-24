namespace CardWirthEngineTest.GameMasters.Contents.Branch

module TestUtil =
  open CardWirthEngineTest.GameMasterTestUtil
  
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters.Scenario

  let flag_name = "test_flag" 
  let check_flag_state =
    { empty_scenario.global_state with
        flags = Map.ofList [flag_name, false] }
  let true_false =
    [ true, SetFlag ([], flag_name, true)
    ; false, SetFlag ([], flag_name, false)
    ]