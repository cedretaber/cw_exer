namespace CardWirthEngineTest

open NUnit.Framework
open FsCheck.NUnit
open CardWirthEngineTest.TestUtils

module UtilTest =
  open CardWirthEngine.Util

  module const' =
    [<Property>]
    let ``引数を無視して、常に同じ値を返すこと`` (input : obj) =
      let value = "const value" in
      const' value input = value
  
  module is_true =
    [<Test>]
    let ``trueの場合`` () =
      assert' <| is_true true

    [<Test>]
    let ``falseの場合`` () =
      assert_not <| is_true false
  
  module is_false =
    [<Test>]
    let ``trueの場合`` () =
      assert_not <| is_false true

    [<Test>]
    let ``falseの場合`` () =
      assert' <| is_false false
