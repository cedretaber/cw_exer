namespace CardWirthEngineTest

module UtilTest =
  open Expecto
  open CardWirthEngine.Util
  
  [<Tests>]
  let util =
    testList "CardWirthEngine.Util" [
      testProperty "const'" <| fun input ->
        let value = "const value" in
        const' value input = value

      testList "is_true" [
        test "trueの場合" { Expect.isTrue (is_true true) "trueになること" }
        test "falseの場合" { Expect.isFalse (is_true false) "falseになること" }
      ]

      testList "is_false" [
        test "trueの場合" { Expect.isFalse (is_false true) "falseになること" }
        test "falseの場合" { Expect.isTrue (is_false false) "trueになること" }
      ]
    ]
