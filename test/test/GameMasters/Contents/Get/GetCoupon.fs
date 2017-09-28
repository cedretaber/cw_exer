namespace CardWirthEngineTest.GameMasters.Contents.Get

module Coupon =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil

  open CardWirthEngine.Data.Type
  open CardWirthEngine.Cards
  open CardWirthEngine.Scenario.Events.Content
  open CardWirthEngine.GameMasters
  open CardWirthEngine.GameMasters.Scenario
  open CardWirthEngine.GameMasters.State
  open CardWirthEngine.GameMaster
  open CardWirthEngine.GameMasters.Cards

  [<Tests>]
  let get_coupon =
    testList "CardWirthEngine.Scenario.Events.Content.GetCoupon" [
      test "選択中のキャストにクーポンを追加した場合" {
        let coupon = "coupon1" in
        let contents = GetCoupon ([], Target.Selected, 0, coupon) in
        let scenario = { empty_scenario with selected = Scenario.PC Adventurers.First } in
        let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
        let state', output = read state [Content (empty_event, contents)] Input.None in
        let (Some adv) = state'.selected_cast in
        Expect.isTrue (Cast.has_coupon coupon adv) "正しくクーポンが追加されていること"
        Expect.equal output Output.Coupon "正しくクーポン変更の通知が返ること"
      }
    ]