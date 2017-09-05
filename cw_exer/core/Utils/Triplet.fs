namespace CardWirthEngine.Utils

module Triplet =
  type ('a, 'b, 'c) t = 'a * 'b * 'c
  let t : 'a -> 'b -> 'c -> ('a, 'b, 'c) t = fun a b c -> a, b, c

  let first : ('a, 'b, 'c) t -> 'a = function f, _, _ -> f
  let second : ('a, 'b, 'c) t -> 'b = function _, s, _ -> s
  let third : ('a, 'b, 'c) t -> 'c = function _, _, t -> t

  let inline trimap f g h =
    function a, b, c -> f a, g b, h c

  let fold : ('a -> 'b -> 'c -> 'd ) -> ('a, 'b, 'c) t -> 'd = (<|||)