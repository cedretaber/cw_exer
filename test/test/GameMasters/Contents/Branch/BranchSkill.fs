namespace CardWirthEngineTest.GameMasters.Contents.Branch

module Skill =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil
  open CardWirthEngineTest.GameMasters.Contents.Branch.TestUtil
  
  open CardWirthEngine.Data.Type
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters
  open CardWirthEngine.GameMasters.Scenario
  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMaster
  open CardWirthEngine.GameMasters.Cards

  [<Tests>]
  let branch_skill =
    testList "CardWirthEngine.Scenario.Events.Content.BranchSkill" [
      test "対象: Randomでキャストがスキルを保有している場合" {
        let skill_id = empty_skill.property.id in
        let cast1 =
          { empty_cast with skill = [|empty_skill|] } in
        let party =
          { minimal_party with
              adventurers = Adventurers.add cast1 no_adventurers } in
        let scenario =
          { Scenario.set_skills (Map.ofList [skill_id, empty_skill]) empty_scenario with
              global_state = check_flag_state } in
        let state =
          make_empty_state scenario |> State.set_party party in
        let contents =
          BranchSkill
            ( true_false
            , skill_id
            , 1
            , Range.Random
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isTrue (get_flag flag_name scenario') "正しくtrueを返すこと"
      }
    ]