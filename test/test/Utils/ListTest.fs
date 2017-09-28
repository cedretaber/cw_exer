module CardWirthEngineTest.Utils.ListTest

open Expecto
open CardWirthEngine.Utils.List

let list = [1;2;3;1;2;3;1;2;3] 
let f = (fun i -> i = 1)

[<Tests>]
let list_util =
  testList "CardWirthEngine.Utils.List" [
    testList "filter_not_limited" [
      test "正常系" {
        Expect.equal (filter_not_limited 2 f list) [2;3;2;3;1;2;3] "正しくフィルタできること"
      }

      test "最大値の方がリストに含まれている要素数より多い場合" {
        Expect.equal (filter_not_limited 4 f list) [2;3;2;3;2;3] "リスト内の条件を満たす要素が全て取り除かれること"
      }

      test "最大値が0の場合" {
        Expect.equal (filter_not_limited 0 f list) list "元のリストがそのまま返ること"
      }
    ]
  ]