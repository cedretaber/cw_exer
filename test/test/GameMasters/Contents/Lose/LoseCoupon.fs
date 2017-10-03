module CardWirthEngineTest.GameMasters.Contents.Lose.Coupon

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Data.Casts
open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster
open CardWirthEngine.GameMasters.Cards

[<Tests>]
let lose_coupon =
  testList "CardWirthEngine.Scenario.Events.Content.LoseCoupon" [
    test "選択中のキャストからクーポンを除去した場合" {
      let coupon = "coupon1" in
      let coupon_set =
        CouponSet.add { name = coupon; value = 0 } CouponSet.empty in
      let cast =
        { empty_cast with property = { empty_cast.property with coupons = coupon_set } } in
      let contents = LoseCoupon ([], Target.Selected, coupon) in
      let scenario = { empty_scenario with selected = Scenario.PC Adventurers.First } in
      let party = { minimal_party with adventurers = [|Adventurers.Exist cast|] } in
      let state = State.Scenario (scenario, party, empty_global_data, state_random) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      let (Some adv) = State.selected_cast state' in
      Expect.isFalse (Cast.has_coupon coupon adv) "正しくクーポンが削除されていること"
      Expect.equal output Output.Coupon "正しくクーポン変更の通知が返ること"
    }
  ]