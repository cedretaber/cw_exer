module CardWirthEngineTest.Utils.ListTest

open Expecto
open CardWirthEngine.Utils.List

[<Tests>]
let list_util =
  testList "CardWirthEngine.Utils.List" [
    testList "filter_not_limited" [

      let list = [1;2;3;1;2;3;1;2;3] 
      let f = (fun i -> i = 1)

      yield test "正常系" {
        Expect.equal (filter_not_limited 2 f list) [2;3;2;3;1;2;3] "正しくフィルタできること"
      }

      yield test "最大値の方がリストに含まれている要素数より多い場合" {
        Expect.equal (filter_not_limited 4 f list) [2;3;2;3;2;3] "リスト内の条件を満たす要素が全て取り除かれること"
      }

      yield test "最大値が0の場合" {
        Expect.equal (filter_not_limited 0 f list) list "元のリストがそのまま返ること"
      }
    ]

    testList "fold_right" [
      testProperty "左右で結果は同じになること" <| fun (list : int list) ->
        Expect.equal (fold_right (+) 0 list) (List.fold (+) 0 list)
      
      testProperty "fold_rightのconsはlistのconcatと同じ" <| fun (list1 : int list) (list2 : int list) ->
        let result = fold_right (fun e a -> e :: a) list1 list2 in
        Expect.equal result (list2 @ list1)
    ]
  ]