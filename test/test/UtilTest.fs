namespace CardWirthEngine

open Xunit
open FsCheck
open FsCheck.Xunit

module UtilTest =
  open Util

  module ``const'`` =

    [<Property>]
    let ``引数を無視して、常に同じ値を返すこと`` (input : obj) =
      let value = "const value" in
      const' value input = value