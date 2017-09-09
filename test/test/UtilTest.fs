namespace CardWirthEngineTest

open NUnit.Framework
open FsCheck.NUnit

module UtilTest =
  open CardWirthEngine.Util

  module const' =
    [<Property>]
    let ``引数を無視して、常に同じ値を返すこと`` (input : obj) =
      let value = "const value" in
      Assert.AreEqual (const' value input, value)
  
  module is_true =
    [<Property>]
    let ``正しくBool値を返すこと`` (bool : bool) =
      Assert.AreEqual (is_true bool, bool)
  
  module is_false =
    [<Property>]
    let ``正しくBool値を返すこと`` (bool : bool) =
      Assert.AreEqual (is_false bool, not bool)

  module equals =
    [<Property>]
    let ``両者が等しい場合、trueを返すこと`` (obj : obj) =
      Assert.IsTrue (equals obj obj)
      
    [<Property>]
    let ``両者が異なる場合、falseを返すこと`` (obj1 : obj, obj2 : obj) =
      if obj1 = obj2
      then Assert.IsTrue (equals obj1 obj2)
      else Assert.IsFalse (equals obj1 obj2)
