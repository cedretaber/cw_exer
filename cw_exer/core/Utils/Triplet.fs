namespace CardWirthEngine.Utils

module Triplet =
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let first = function f, _, _ -> f
  let second = function _, s, _ -> s
  let third = function _, _, t -> t

  let inline trimap f g h =
    function a, b, c -> f a, g b, h c

  let fold = (<|||)