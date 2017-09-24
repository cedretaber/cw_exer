namespace CardWirthEngineTest.GameMasters

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

module CardOpsTest =

  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMasters.CardOps
  
  [<Tests>]
  let card_ops =
    testList "CardWirthEngine.GameMasters.CardOps" [
      testList "companion_exists" [
        test "NPCがいる場合" {
          let state =
            Scenario
              ( { empty_scenario with companions = one_adventurer }
              , minimal_party
              , empty_global_data
              , state_random
              ) in
          Expect.isTrue (companion_exists empty_cast.property.id state) "trueが返ること"
        }
      ]
    ]