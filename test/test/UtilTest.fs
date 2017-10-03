module CardWirthEngineTest.UtilTest

#nowarn "62"

open Expecto
open CardWirthEngine.Util

[<Tests>]
let util_tests =
  testList "CardWirthEngine.Util" [
    testProperty "const'" <| fun input const_value ->
      Expect.equal (const' const_value input) const_value "常に同じ値を返すこと"
  ]