module CardWirthEngine.Utils.Array

let inline updated index value old_arr =
  let new_arr = Array.copy old_arr in
  new_arr.[index] <- value
  new_arr

let inline remove index (arr : 'a array) =
  Array.append arr.[0..index-1] arr.[index+1..]

let inline count_by f = Array.sumBy <| fun e -> if f e then 1 else 0

let inline filter_not_limited count f =
  List.ofArray
  >> List.filter_not_limited count f
  >> List.toArray

let inline multi_cons count elem array =
  Array.append array <| Array.create count elem