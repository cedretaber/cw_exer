namespace CardWirthEngine.Data.Types

module Comparison =
  type t = Eq | Ne | Lt | Gt

  let inline compare comp x y =
    match comp with
      Eq -> x = y
    | Ne -> x <> y
    | Lt -> x < y
    | Gt -> x > y