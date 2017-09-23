namespace CardWirthEngine.Data.Casts

open CardWirthEngine.Utils

module Coupon =
  type Name = string
  type Value = int

  type t =
    { name : Name
    ; value : Value
    }

(* 順序を保持し、重複しないクーポン名を持つ *)
module CouponSet =
  type t =
    { map : (Coupon.Name, Coupon.Value) Map
    ; list : Coupon.t list
    }

  let empty : t =
    { map = Map.empty; list = List.empty }

  let contains : Coupon.Name -> t -> bool =
    fun name ->
      function
        { map = map } -> Map.containsKey name map

  let add : Coupon.t -> t -> t =
    function
      { name = name; value = value } as coupon ->
        function
          { map = map; list = list } when Map.containsKey name map ->
            { map = Map.add name value map
            ; list = coupon :: List.filter (function { name = name' } -> name <> name') list
            }
        | { map = map; list = list } ->
            { map = Map.add name value map
            ; list = coupon :: list
            }

  let to_list cset = cset.list
  let to_set cset =
    cset.map
    |> Map.toList
    |> List.map Pair.first
    |> Set.ofList