namespace CardWirthEngine

open NUnit.Framework
open FsCheck.NUnit

module UtilTest =
  open Util

  module ``const'`` =

    [<Property>]
    let ``引数を無視して、常に同じ値を返すこと`` (input : obj) =
      let value = "const value" in
      Assert.AreEqual (const' value input, value)

    [<Test>]
    let ``成功させるテスト`` () =
      Assert.AreEqual (1, 1)

    [<Test>]
    let ``失敗させるテスト`` () =
      Assert.AreEqual (1, 2)