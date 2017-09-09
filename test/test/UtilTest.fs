namespace CardWirthEngineTest

open NUnit.Framework
open FsCheck.NUnit

module UtilTest =
  open CardWirthEngine.Util

  module ``const'`` =

    [<Property>]
    let ``引数を無視して、常に同じ値を返すこと`` (input : obj) =
      let value = "const value" in
      Assert.AreEqual (const' value input, value)