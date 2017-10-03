module CardWirthEngineTest.GameMasters.Contents.Get.Item

open Expecto
open CardWirthEngineTest.GameMasterTestUtil
  
open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let get_item =
  testList "CardWirthEngine.Scenario.Events.Content.GetItem" [
    test "荷物袋にアイテムを追加した場合" {
      let item = empty_item in
      let id = item.property.id in
      let contents = GetItem ([], id, Range.Backpack, 1) in
      let scenario =
        { empty_scenario with
            cards = { empty_scenario.cards with items = Map.ofList [id, item] } } in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      let (Party.Item item') :: _ = get_bag state' in
      Expect.equal item' item "正しく追加されること"
    }
  ]