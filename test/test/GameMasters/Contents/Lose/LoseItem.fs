module CardWirthEngineTest.GameMasters.Contents.Lose.Item

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
let lose_item =
  testList "CardWirthEngine.Scenario.Events.Content.LoseItem" [
    test "荷物袋の中のアイテムを全て削除した場合" {
      let item = empty_item in
      let id = item.property.id in
      let contents = LoseItem ([], id, Range.Backpack, RemoveCount.All) in
      let scenario = Scenario.set_items (Map.ofList [id, item]) empty_scenario in
      let party = Party.set_bag [Party.Item item; Party.Item item] minimal_party in
      let state = State.Scenario (scenario, party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.isEmpty (get_bag state') "全て削除されていること"
    }

    test "荷物袋の中のアイテムを1つだけ削除した場合" {
      let item = empty_item in
      let id = item.property.id in
      let contents = LoseItem ([], id, Range.Backpack, RemoveCount.Count 1) in
      let scenario = Scenario.set_items (Map.ofList [id, item]) empty_scenario in
      let party = Party.set_bag [Party.Item item; Party.Item item] minimal_party in
      let state = State.Scenario (scenario, party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.equal (state' |> get_bag |> List.length) 1 "1つだけ削除されていること"
      let [Party.Item item'] = get_bag state' in
      Expect.equal item' item "アイテムは変わっていないこと"
    }
  ]