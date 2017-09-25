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