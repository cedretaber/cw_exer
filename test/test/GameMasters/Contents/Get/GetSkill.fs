module CardWirthEngineTest.GameMasters.Contents.Get.Skill

open Expecto
open CardWirthEngineTest.GameMasterTestUtil
  
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

[<Tests>]
let get_skill =
  testList "CardWirthEngine.Scenario.Events.Content.GetSkill" [
    test "荷物袋にスキルを追加した場合" {
      let skill = empty_skill in
      let id = skill.property.id in
      let contents = GetSkill ([], id, Range.Backpack, 1) in
      let scenario =
        { empty_scenario with
            cards = { empty_scenario.cards with skills = Map.ofList [id, skill] } } in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let state', _ = read state [Content (empty_event, contents)] Input.None in
      let (Party.Skill skill') :: _ = get_bag state' in
      Expect.equal skill' skill "正しく追加されること"
    }
  ]