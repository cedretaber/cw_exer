module CardWirthEngineTest.GameMasters.Contents.Visual.HideParty

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let hide_party =
  testList "CardWirthEngine.Scenario.Events.Content.HideParty" [
    test "パーティ隠蔽" {
      let contents = HideParty [] in
      let state = State.Scenario (empty_scenario, minimal_party, empty_global_data, state_random) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      Expect.equal output Output.PartyDown "パーティ隠蔽の出力が成されること"
      Expect.equal state' state "状態に変化は無いこと"
    }
  ]