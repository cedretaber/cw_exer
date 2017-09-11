namespace CardWirthEngineTest

open NUnit.Framework

open CardWirthEngineTest.TestUtils
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

module GameMasterEventTest =
  module BranchTest =
    module BranchCastTest =

      [<Test>]
      let ``キャストの存在を正しく識別できること`` () =
        let cast1 = empty_cast
        let flag_name = "test_flag"
        let scenario =
          { empty_scenario with
              global_state = { empty_scenario.global_state with flags = Map.ofList [flag_name, false] }
              companions = one_adventurer }
        let state = make_empty_state scenario
        let contents =
          BranchCast
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , cast1.property.id
            )
        let state', _ = read state [Content (empty_event, contents)] Input.None
        get_flag flag_name state' === true