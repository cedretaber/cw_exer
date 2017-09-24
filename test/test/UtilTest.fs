namespace CardWirthEngineTest

open Expecto

module UtilTest =
  open CardWirthEngine.Util

  [<Tests>]
  let const' =
    testProperty "const'" <| fun input ->
      let value = "const value" in
      const' value input = value
  
  [<Tests>]
  let is_true =
    testList "is_true" [
      test "trueの場合" { Expect.isTrue (is_true true) "trueになること" }
      test "falseの場合" { Expect.isFalse (is_true false) "falseになること" }
    ]
  
  [<Tests>]
  let is_false =
    testList "is_false" [
      test "trueの場合" { Expect.isFalse (is_false true) "falseになること" }
      test "falseの場合" { Expect.isTrue (is_false false) "trueになること" }
    ]
