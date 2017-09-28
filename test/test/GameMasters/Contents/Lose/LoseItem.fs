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
      let party = { minimal_party with
                      bag = [Party.Item item; Party.Item item] } in
      let state = State.Scenario (empty_scenario, party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.isEmpty state'.party.bag "全て削除されていること"
    }

    test "荷物袋の中のアイテムを1つだけ削除した場合" {
      let item = empty_item in
      let id = item.property.id in
      let contents = LoseItem ([], id, Range.Backpack, RemoveCount.Count 1) in
      let party = { minimal_party with
                      bag = [Party.Item item; Party.Item item] } in
      let state = State.Scenario (empty_scenario, party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.equal state'.party.bag.Length 1 "1つだけ削除されていること"
      let [Party.Item item'] = state'.party.bag in
      Expect.equal item' item "アイテムは変わっていないこと"
    }
  ]