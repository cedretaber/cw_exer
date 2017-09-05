namespace CardWirthEngine.Utils

module Pair =
  type ('a, 'b) t = 'a * 'b

  let first = function f, _ -> f
  let second = function _, s -> s

  let inline bimap f g =
    function a, b -> f a, g b

  let fold = (<||)