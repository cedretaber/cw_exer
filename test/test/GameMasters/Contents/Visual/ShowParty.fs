module CardWirthEngineTest.GameMasters.Contents.Visual.ShowParty

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let show_party =
  testList "CardWirthEngine.Scenario.Events.Content.ShowParty" [
    test "パーティ表示" {
      let contents = ShowParty [] in
      let state = State.Scenario (empty_scenario, minimal_party, empty_global_data, state_random) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      Expect.equal output Output.PartyUp "パーティ表示の出力が成されること"
      Expect.equal state' state "状態に変化は無いこと"
    }
  ]