namespace CardWirthEngine.Data.Casts

open CardWirthEngine
open CardWirthEngine.Utils

module Coupon =
  type Name = string
  type Value = int

  type t =
    { name : Name
    ; value : Value
    }

  let (|System|Battle|Period|Concealed|Normal|) =
    function
      name when String.starts_with "＠" name -> System
    | name when String.starts_with "；" name -> Battle
    | name when String.starts_with "：" name -> Period
    | name when String.starts_with "＿" name -> Concealed
    | _ -> Normal 

(* 順序を保持し、重複しないクーポン名を持つ *)
module CouponSet =

  type CouponMap = (Coupon.Name, Coupon.Value) Map

  type t =
    { normals : CouponMap
    ; concealeds : CouponMap
    ; periods : CouponMap
    ; battles : CouponMap
    ; systems : CouponMap
    ; list : Coupon.Name list
    }

  let empty : t =
    { normals = Map.empty
    ; concealeds = Map.empty
    ; periods = Map.empty
    ; battles = Map.empty
    ; systems = Map.empty
    ; list = List.empty
    }

  let contains : Coupon.Name -> t -> bool =
    fun name cset ->
      let map =
        match name with
          Coupon.System -> cset.systems
        | Coupon.Battle -> cset.battles
        | Coupon.Period -> cset.periods
        | Coupon.Concealed -> cset.concealeds
        | Coupon.Normal -> cset.normals in
      Map.containsKey name map

  let add : Coupon.t -> t -> t =
    function
      { name = name; value = value } ->
        fun cset ->
          let list = cset.list in
          let contains = Map.containsKey name in
          let add_map = Map.add name value in
          let inline add_list list = name :: List.filter ((<>) name) list in
          match name, cset with
            Coupon.System, { systems = systems } when contains systems ->
              { cset with list = add_list list }
          | Coupon.System, { systems = systems } ->
              { cset with systems = add_map systems; list = name :: list }
          | Coupon.Battle, { battles = battles } when contains battles ->
              { cset with list = add_list list }
          | Coupon.Battle, { battles = battles } ->
              { cset with battles = add_map battles; list = name :: list }
          | Coupon.Period, { periods = periods } when contains periods ->
              { cset with list = add_list list }
          | Coupon.Period, { periods = periods } ->
              { cset with periods = add_map periods; list = name :: list }
          | Coupon.Concealed, { concealeds = concealeds } when contains concealeds ->
              { cset with list = add_list list }
          | Coupon.Concealed, { concealeds = concealeds } ->
              { cset with concealeds = add_map concealeds; list = name :: list }
          | Coupon.Normal, { normals = normals } when contains normals ->
              { cset with list = add_list list }
          | Coupon.Normal, { normals = normals } ->
              { cset with normals = add_map normals; list = name :: list }

  let to_list cset = cset.list
  let to_set =
    function
      { systems = systems
      ; battles = battles
      ; periods = periods
      ; concealeds = concealeds
      ; normals = normals
      } ->
        [systems; battles; periods; concealeds; normals]
        |> List.collect
             (Map.toList >> List.map Pair.first)
        |> Set.ofList

  let elapse : t -> t =
    function
      { battles = battles
      ; periods = periods
      ; list = list
      } as cset ->
        let elapse set_init =
          Map.fold
            (fun (map, set) name ->
              function
                0 -> Map.add name 0 map, set
              | 1 -> map, Set.add name set
              | point -> Map.add name (point - 1) map, set)
            (Map.empty, set_init) in
        let battles', set = elapse Set.empty battles in
        let periods', set = elapse set periods in
        { cset with battles = battles';
                    periods = periods';
                    list = List.filter (fun name -> not <| Set.contains name set) list }

  let inline private clean_coupons cset set =
    { cset with list = List.filter (fun name -> not <| Set.contains name set) cset.list }

  let end_battle : t -> t =
    function
      { battles = battles } as cset ->
        battles
        |> Map.toList
        |> List.map Pair.first
        |> Set.ofList
        |> clean_coupons { cset with battles = Map.empty }

  let end_scenario : t -> t =
    function
      { battles = battles
      ; periods = periods
      } as cset ->
        [battles; periods]
        |> List.collect (Map.toList >> List.map Pair.first)
        |> Set.ofList
        |> clean_coupons { cset with battles = Map.empty; periods = Map.empty }