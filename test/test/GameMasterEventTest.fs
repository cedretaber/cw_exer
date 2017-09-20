namespace CardWirthEngineTest

open NUnit.Framework
open FsCheck.NUnit

open CardWirthEngineTest.TestUtils
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster
open CardWirthEngine.GameMasters.Cards

module GameMasterEventTest =
  module BranchTest =

    let flag_name = "test_flag" 
    let check_flag_state =
      { empty_scenario.global_state with
          flags = Map.ofList [flag_name, false] }

    module BranchCastTest =
      [<Test>]
      let ``キャストの存在を正しく識別できること`` () =
        let scenario =
          { empty_scenario with
              global_state = check_flag_state;
              companions = one_adventurer } in
        let state = make_empty_state scenario in
        let contents =
          BranchCast
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , empty_cast.property.id
            ) in
        let state', _ =
          read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        get_flag flag_name scenario' === true

    module BranchItemTest =

      [<Test>]
      let ``キャストの保有するアイテムの存在を正しく識別できること`` () =
        let item_id = empty_item.property.id in
        let cast1 =
          { empty_cast with item = [empty_item] } in
        let party =
          { minimal_party with
              adventurers = Adventurers.add cast1 no_adventurers } in
        let scenario =
          { empty_scenario with
              global_state = check_flag_state;
              cards = { empty_scenario.cards with
                          items = Map.ofList [item_id, empty_item] }} in
        let state =
          make_empty_state scenario |> State.set_party party in
        let contents =
          BranchItem
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , item_id
            , 1
            , Range.Random
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        get_flag flag_name scenario' === true

    module BranchSkillTest =

      [<Test>]
      let ``キャストの保有するスキルの存在を正しく識別できること`` () =
        let skill_id = empty_skill.property.id in
        let cast1 =
          { empty_cast with skill = [empty_skill] } in
        let party =
          { minimal_party with
              adventurers = Adventurers.add cast1 no_adventurers } in
        let scenario =
          { empty_scenario with
              global_state = check_flag_state;
              cards = { empty_scenario.cards with
                          skills = Map.ofList [skill_id, empty_skill] }} in
        let state =
          make_empty_state scenario |> State.set_party party in
        let contents =
          BranchSkill
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , skill_id
            , 1
            , Range.Random
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        get_flag flag_name scenario' === true

    module BranchInfoTest =

      [<Test>]
      let ``情報カードの保有で正しく分岐できること`` () =
        let info_id = empty_info.id in
        let global_state =
          { empty_scenario.global_state with
              infos = Set.ofList [info_id] } in
        let scenario =
          { empty_scenario with
              global_state = global_state } in
        let state = make_empty_state scenario in
        let contents =
          BranchInfo
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , info_id
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        get_flag flag_name scenario' === true

    module BranchBeastTest =

      [<Test>]
      let ``キャストの保有する召喚獣の存在を正しく識別できること`` () =
        let beast_id = empty_beast.property.id in
        let cast1 =
          { empty_cast with beast = [empty_beast] } in
        let party =
          { minimal_party with
              adventurers = Adventurers.add cast1 no_adventurers } in
        let scenario =
          { empty_scenario with
              global_state = check_flag_state;
              cards = { empty_scenario.cards with
                          beasts = Map.ofList [beast_id, empty_beast] }} in
        let state =
          make_empty_state scenario |> State.set_party party in
        let contents =
          BranchBeast
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , beast_id
            , 1
            , Range.Random
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        get_flag flag_name scenario' === true

    module BranchMoneyTest =

      [<Property>]
      let ``充分な所持金を持っている場合`` (amount : int) =
        let party =
          { minimal_party with money = amount } in
        let state =
          make_empty_state { empty_scenario with global_state = check_flag_state }
          |> State.set_party party in
        let amount' =
          if amount - 1 <= amount
          then amount - 1
          else amount
        let contents =
          BranchMoney
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , amount'
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        get_flag flag_name scenario' === true

      [<Property>]
      let ``充分な所持金を持っていない場合`` (amount : int) =
        let party =
          { minimal_party with money = amount } in
        let state =
          make_empty_state { empty_scenario with global_state = check_flag_state }
          |> State.set_party party in
        let amount' =
          if amount + 1 >= amount
          then amount + 1
          else amount
        let contents =
          BranchMoney
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , amount'
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        get_flag flag_name scenario' === false

    module BranchCompleteStampTest =

      [<Test>]
      let ``済印の存在を正しく識別できること`` () =
        let completed_scenario_name = "scenario1" in
        let global_data =
          { empty_global_data with completed_scenarii = Set.ofList [completed_scenario_name] }
        let state =
          State.Scenario (empty_scenario, minimal_party, global_data, state_random) in
        let contents =
          BranchCompleteStamp
            ( [ true, SetFlag ([], flag_name, true)
              ; false, SetFlag ([], flag_name, false)
              ]
            , completed_scenario_name
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        get_flag flag_name scenario' === true