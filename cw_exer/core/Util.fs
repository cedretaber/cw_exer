namespace CardWirthEngine

module Util =

  let inline const' value = fun _ -> value

  let first = function value, _ -> value
  let second = function _, value -> value

  let is_true (bool: bool) = bool
  let is_false bool = not bool