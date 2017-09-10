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

  module equals =
    [<Property(Verbose = true)>]
    let ``両者が等しい場合、trueを返すこと`` (obj : obj) =
      equals obj obj
      
    [<Property(Verbose = true)>]
    let ``両者が異なる場合、falseを返すこと`` (obj1 : obj, obj2 : obj) =
      if obj1 = obj2
      then equals obj1 obj2
      else not <| equals obj1 obj2
