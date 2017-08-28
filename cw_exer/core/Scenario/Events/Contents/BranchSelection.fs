namespace CardWirthEngine.Scenario.Event.Contents

module BranchSelect =
  type Target
    = Active
    | Party

  type Method
    = Manual // 手動で選択
    | Random // ランダムで選択
    | Valued // 評価条件で選択(Wsn.1)

  type t = {
    target : Target;
    method : Method
  }

  let t target method = { target = target; method = method }