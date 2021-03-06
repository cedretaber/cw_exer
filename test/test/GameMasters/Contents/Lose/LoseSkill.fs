﻿module CardWirthEngineTest.GameMasters.Contents.Lose.Skill

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let lose_skill =
  testList "CardWirthEngine.Scenario.Events.Content.LoseSkill" [
    test "荷物袋の中のスキルを全て削除した場合" {
      let skill = empty_skill in
      let id = skill.property.id in
      let contents = LoseSkill ([], id, Range.Backpack, RemoveCount.All) in
      let scenario = Scenario.set_skills (Map.ofList [id, skill]) empty_scenario in
      let party = Party.set_bag [Party.Skill skill; Party.Skill skill] minimal_party in
      let state = State.Scenario (scenario, party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.isEmpty (get_bag state') "全て削除されていること"
    }

    test "荷物袋の中のアイテムを1つだけ削除した場合" {
      let skill = empty_skill in
      let id = skill.property.id in
      let contents = LoseSkill ([], id, Range.Backpack, RemoveCount.Count 1) in
      let scenario = Scenario.set_skills (Map.ofList [id, skill]) empty_scenario in
      let party = Party.set_bag [Party.Skill skill; Party.Skill skill] minimal_party in
      let state = State.Scenario (scenario, party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      Expect.equal (state' |> get_bag |> List.length) 1 "1つだけ削除されていること"
      let [Party.Skill skill'] = get_bag state' in
      Expect.equal skill' skill "アイテムは変わっていないこと"
    }
  ]