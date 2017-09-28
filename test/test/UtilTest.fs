module CardWirthEngineTest.UtilTest

#nowarn "62"

open Expecto
open CardWirthEngine.Util

[<Tests>]
let util_tests =
  testList "CardWirthEngine.Util" [
    testProperty "const'" <| fun input ->
      let value = "const value" in
      const' value input = value
  ]