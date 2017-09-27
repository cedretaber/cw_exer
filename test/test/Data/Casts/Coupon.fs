module CardWirthEngineTest.Data.Casts.Coupon

open Expecto
open CardWirthEngine.Data.Casts

[<Tests>]
let coupon_tests =
  testList "CardWirthEngine.Data.Casts.Coupon" [
    testList "coupon attributes" [
      let judge = function
                    Coupon.System -> "system"
                  | Coupon.Battle -> "battle"
                  | Coupon.Period -> "period"
                  | Coupon.Concealed -> "concealed"
                  | Coupon.Normal -> "normal"

      yield test "先頭に＠の付くクーポンは" {
        Expect.equal (judge "＠システム") "system" "システムクーポン"
      }
      
      yield test "先頭；の付くクーポンは" {
        Expect.equal (judge "；戦闘中") "battle" "戦闘時クーポン"
      }
      
      yield test "先頭に：の付くクーポンは" {
        Expect.equal (judge "：時限") "period" "時限クーポン"
      }
      
      yield test "先頭に＿の付くクーポンは" {
        Expect.equal (judge "＿隠蔽") "concealed" "隠蔽クーポン"
      }
      
      yield test "その他のクーポンは" {
        Expect.equal (judge "クーポン") "normal" "一般クーポン"
      }
    ]
  ]

[<Tests>]
let coupon_set_tests =
  testList "CardWirthEngine.Data.Casts.CouponSet" [
    testList "contains" [
      test "システムクーポンがある場合" {
        let coupon = "＠システム" in
        let cset = { CouponSet.empty with systems = Map.ofList [coupon, 0] } in
        Expect.isTrue (CouponSet.contains coupon cset) "trueを返すこと"
      }
      
      test "システムクーポンが無い場合" {
        let coupon = "＠システム" in
        Expect.isFalse (CouponSet.contains coupon CouponSet.empty) "falseを返すこと"
      }
      
      test "戦闘時クーポンがある場合" {
        let coupon = "；戦闘時" in
        let cset = { CouponSet.empty with battles = Map.ofList [coupon, 0] } in
        Expect.isTrue (CouponSet.contains coupon cset) "trueを返すこと"
      }
      
      test "戦闘時クーポンが無い場合" {
        let coupon = "；戦闘時" in
        Expect.isFalse (CouponSet.contains coupon CouponSet.empty) "falseを返すこと"
      }
      
      test "時限クーポンがある場合" {
        let coupon = "：時限" in
        let cset = { CouponSet.empty with periods = Map.ofList [coupon, 0] } in
        Expect.isTrue (CouponSet.contains coupon cset) "trueを返すこと"
      }
      
      test "時限クーポンが無い場合" {
        let coupon = "：時限" in
        Expect.isFalse (CouponSet.contains coupon CouponSet.empty) "falseを返すこと"
      }
      
      test "隠蔽クーポンがある場合" {
        let coupon = "＿隠蔽" in
        let cset = { CouponSet.empty with concealeds = Map.ofList [coupon, 0] } in
        Expect.isTrue (CouponSet.contains coupon cset) "trueを返すこと"
      }
      
      test "隠蔽クーポンが無い場合" {
        let coupon = "＿隠蔽" in
        Expect.isFalse (CouponSet.contains coupon CouponSet.empty) "falseを返すこと"
      }
      
      test "クーポンがある場合" {
        let coupon = "クーポン" in
        let cset = { CouponSet.empty with normals = Map.ofList [coupon, 0] } in
        Expect.isTrue (CouponSet.contains coupon cset) "trueを返すこと"
      }
      
      test "クーポンが無い場合" {
        let coupon = "クーポン" in
        Expect.isFalse (CouponSet.contains coupon CouponSet.empty) "falseを返すこと"
      }
    ]

    testList "add" [
      test "システムクーポン" {
        let coupon = "＠システム" in
        let point = 1 in
        let cset = CouponSet.add { name = coupon; value = point } CouponSet.empty in
        Expect.equal cset.systems (Map.ofList [coupon, point]) "正しく追加されること"
      }
      
      test "戦闘時クーポン" {
        let coupon = "；戦闘時" in
        let point = 1 in
        let cset = CouponSet.add { name = coupon; value = point } CouponSet.empty in
        Expect.equal cset.battles (Map.ofList [coupon, point]) "正しく追加されること"
      }
      
      test "時限クーポン" {
        let coupon = "：時限" in
        let point = 1 in
        let cset = CouponSet.add { name = coupon; value = point } CouponSet.empty in
        Expect.equal cset.periods (Map.ofList [coupon, point]) "正しく追加されること"
      }
      
      test "隠蔽クーポン" {
        let coupon = "＿隠蔽" in
        let point = 1 in
        let cset = CouponSet.add { name = coupon; value = point } CouponSet.empty in
        Expect.equal cset.concealeds (Map.ofList [coupon, point]) "正しく追加されること"
      }
      
      test "一般クーポン" {
        let coupon = "クーポン" in
        let point = 1 in
        let cset = CouponSet.add { name = coupon; value = point } CouponSet.empty in
        Expect.equal cset.normals (Map.ofList [coupon, point]) "正しく追加されること"
      }
    ]

    testList "elapse" [
      test "時間が経過した時" {
        let cset = { CouponSet.empty with
                       battles = Map.ofList ["；点数付き、消えない", 2; ";点数付き、消える", 1; "；点数無し、消えない", 0];
                       periods = Map.ofList ["：点数付き、消えない", 2; "：点数付き、消える", 1; "：点数無し、消えない", 0];
                       list = [ "；点数付き、消えない"
                              ; ";点数付き、消える"
                              ; "点数無し、消えない"
                              ; "：点数付き、消えない"
                              ; "：点数付き、消える"
                              ; "点数無し、消えない"
                              ] } in
        let cset' = CouponSet.elapse cset in
        Expect.equal cset'.list.Length 4 "クーポンの残りは4つであること"
        Expect.containsAll
          cset'.list
          ["；点数付き、消えない"; "点数無し、消えない"; "：点数付き、消えない"; "点数無し、消えない"]
          "消えないはずのクーポンは残っていること"
        Expect.sequenceEqual
          (Map.toList cset'.battles)
          ["；点数付き、消えない", 1; "；点数無し、消えない", 0]
          "点数が減り、消えないはずのクーポンが残っていること"
        Expect.sequenceEqual
          (Map.toList cset'.periods)
          ["：点数付き、消えない", 1; "：点数無し、消えない", 0]
          "点数が減り、消えないはずのクーポンが残っていること"
      }
    ]

    testList "end_battle" [
      test "戦闘が終了した時" {
        let periods = Map.ofList ["：点数付き", 3; "：点数無し", 0] in
        let systems = Map.ofList ["＠点数付き", 3; "＠点数無し", 0] in
        let concealeds = Map.ofList ["＿点数付き", 3; "＿点数無し", 0] in
        let cset = { CouponSet.empty with
                       battles = Map.ofList ["；点数付き", 3; "；点数無し", 0];
                       periods = periods;
                       systems = systems;
                       concealeds = concealeds;
                       list = [ "；点数付き"
                              ; "；点数無し"
                              ; "：点数付き"
                              ; "：点数無し"
                              ; "＠点数付き"
                              ; "＠点数無し"
                              ; "＿点数付き"
                              ; "＿点数無し"
                              ] } in
        let cset' = CouponSet.end_battle cset in
        Expect.equal cset'.list.Length 6 "戦闘時クーポンは全て削除されていること"
        Expect.isEmpty cset'.battles "戦闘時クーポンは全て削除されていること"
        Expect.sequenceEqual cset'.periods periods "時限クーポンは変化しないこと"
        Expect.sequenceEqual cset'.concealeds concealeds "隠蔽クーポンは変化しないこと"
        Expect.sequenceEqual cset'.systems systems "システムクーポンは変化しないこと"
      }
    ]

    testList "end_scenario" [
      test "シナリオが終了した時" {
        let systems = Map.ofList ["＠点数付き", 3; "＠点数無し", 0] in
        let concealeds = Map.ofList ["＿点数付き", 3; "＿点数無し", 0] in
        let cset = { CouponSet.empty with
                       battles = Map.ofList ["；点数付き", 3; "；点数無し", 0];
                       periods = Map.ofList ["：点数付き", 3; "：点数無し", 0];
                       systems = systems;
                       concealeds = concealeds;
                       list = [ "；点数付き"
                              ; "；点数無し"
                              ; "：点数付き"
                              ; "：点数無し"
                              ; "＠点数付き"
                              ; "＠点数無し"
                              ; "＿点数付き"
                              ; "＿点数無し"
                              ] } in
        let cset' = CouponSet.end_scenario cset in
        Expect.equal cset'.list.Length 4 "戦闘時・時限クーポンは全て削除されていること"
        Expect.isEmpty cset'.battles "戦闘時クーポンは全て削除されていること"
        Expect.isEmpty cset'.periods "時限クーポンは全て削除されていること"
        Expect.sequenceEqual cset'.concealeds concealeds "隠蔽クーポンは変化しないこと"
        Expect.sequenceEqual cset'.systems systems "システムクーポンは変化しないこと"
      }
    ]
  ]