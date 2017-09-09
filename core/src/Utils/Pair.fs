namespace CardWirthEngine.Utils

module Pair =
  type ('a, 'b) t = 'a * 'b
  let t : 'a -> 'b -> ('a, 'b) t = fun a b -> a, b

  let first : ('a, 'b) t -> 'a =
    function f, _ -> f
  let second : ('a, 'b) t -> 'b =
    function _, s -> s

  let inline bimap f g =
    function a, b -> f a, g b

  let fold : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c = (<||)