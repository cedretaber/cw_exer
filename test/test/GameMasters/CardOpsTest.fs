namespace CardWirthEngineTest.GameMasters

open NUnit.Framework

open CardWirthEngineTest.TestUtils
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMasters.CardOps

module CardOpsTest =

  module companion_exists =

    [<Test>]
    let ``NPCがいる場合`` () =
      let state =
        Scenario
          ( { empty_scenario with companions = one_adventurer }
          , minimal_party
          , empty_global_data
          , state_random
          ) in
      assert' <| companion_exists empty_cast.property.id state