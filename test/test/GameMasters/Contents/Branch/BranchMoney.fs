namespace CardWirthEngineTest.GameMasters.Contents.Branch

module Money =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil
  open CardWirthEngineTest.GameMasters.Contents.Branch.TestUtil
  
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters
  open CardWirthEngine.GameMasters.Scenario
  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMaster

  [<Tests>]
  let branch_money =
    testList "CardWirthEngine.Scenario.Events.Content.BranchMoney" [
      testProperty "充分な所持金を持っている場合" <| fun amount ->
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
            ( true_false
            , amount'
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isTrue (get_flag flag_name scenario') "判定が成功すること"

      testProperty "充分な所持金を持っていない場合" <| fun amount ->
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
            ( true_false
            , amount'
            ) in
        let state', _ = read state [Content (empty_event, contents)] Input.None in
        let scenario' = State.get_scenario_unsafe state' in
        Expect.isFalse (get_flag flag_name scenario') "判定に失敗すること"
    ]