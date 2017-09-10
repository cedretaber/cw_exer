namespace CardWirthEngineTest.Utils

open NUnit.Framework
open FsCheck.NUnit
open CardWirthEngineTest.TestUtils

module ListUtilTest =
  open CardWirthEngine.Utils.ListUtil

  module filter_limitTest =

    let list = [1;2;3;1;2;3;1;2;3] 
    let f = (fun i -> i = 1)

    [<Test>]
    let ``正しくフィルタできること`` () =
      filter_limit 2 f list === [2;3;2;3;1;2;3]

    [<Test>]
    let ``最大値の方がリストに含まれている要素数より多い場合`` () =
      filter_limit 4 f list === [2;3;2;3;2;3]

    [<Test>]
    let ``最大値が0の場合`` () =
      filter_limit 0 f list === list

