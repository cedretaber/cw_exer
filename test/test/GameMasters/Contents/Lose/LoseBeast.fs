module CardWirthEngineTest.GameMasters.Contents.Lose.Beast

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let lose_beast =
  testList "CardWirthEngine.Scenario.Events.Content.LoseBeast" [
    test "荷物袋の中のスキルを全て削除した場合" {
      let beast = empty_beast in
      let id = beast.property.id in
      let contents = LoseBeast ([], id, Range.Backpack, RemoveCount.All) in
      let scenario =
        { empty_scenario with
            cards = { empty_scenario.cards with
                        beasts = Map.ofList [id, beast] } } in
      let party =
        { minimal_party with
            bag = [Party.Beast beast; Party.Beast beast] } in
      let state = State.Scenario (scenario, party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.isEmpty (get_bag state') "全て削除されていること"
    }

    test "荷物袋の中のアイテムを1つだけ削除した場合" {
      let beast = empty_beast in
      let id = beast.property.id in
      let contents = LoseBeast ([], id, Range.Backpack, RemoveCount.Count 1) in
      let scenario =
        { empty_scenario with
            cards = { empty_scenario.cards with
                        beasts = Map.ofList [id, beast] } } in
      let party = { minimal_party with
                      bag = [Party.Beast beast; Party.Beast beast] } in
      let state = State.Scenario (scenario, party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.equal (state' |> get_bag |> List.length) 1 "1つだけ削除されていること"
      let [Party.Beast beast'] = get_bag state' in
      Expect.equal beast' beast "アイテムは変わっていないこと"
    }
  ]