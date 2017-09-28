module CardWirthEngineTest.GameMasters.Contents.Lose.Cast

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster
open CardWirthEngine.GameMasters.Cards

[<Tests>]
let lose_cast =
  testList "CardWirthEngine.Scenario.Events.Content.LoseCast" [
    test "キャストを削除した場合" {
      let cast = empty_cast in
      let id = cast.property.id in
      let contents = LoseCast ([], id)
      let one_companions = Adventurers.add cast no_adventurers in
      let scenario = { empty_scenario with companions = one_companions } in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.isEmpty (State.get_scenario_unsafe state').companions "正しくキャストが削除されていること"
    }
  ]