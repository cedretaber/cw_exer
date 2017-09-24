namespace CardWirthEngineTest.GameMasters.Contents.Get

module Cast =
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
  let get_cast =
    testList "CardWirthEngine.Scenario.Events.Content.GetCast" [
      test "キャストを追加した場合" {
        let cast = empty_cast in
        let id = cast.property.id in
        let contents = GetCast ([], id, StartAction.NextRound) in
        let scenario =
          { empty_scenario with
              cards = { empty_scenario.cards with casts = Map.ofList [id, cast] } } in
        let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let [|Adventurers.Exist cast'|] = (State.get_scenario_unsafe state').companions in
        Expect.equal cast' cast "正しく追加されること"
      }
    ]