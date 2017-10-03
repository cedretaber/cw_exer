module CardWirthEngineTest.GameMasters.Contents.Get.Beast

open Expecto
open CardWirthEngineTest.GameMasterTestUtil
  
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let get_beast =
  testList "CardWirthEngine.Scenario.Events.Content.GetBeast" [
    test "荷物袋に召喚獣を追加した場合" {
      let beast = empty_beast in
      let id = beast.property.id in
      let contents = GetBeast ([], id, Range.Backpack, 1) in
      let scenario =
        { empty_scenario with
            cards = { empty_scenario.cards with beasts = Map.ofList [id, beast] } } in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      let (Party.Beast beast') :: _ = get_bag state' in
      Expect.equal beast' beast "正しく追加されること"
    }
  ]