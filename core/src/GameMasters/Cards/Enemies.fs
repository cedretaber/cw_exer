namespace CardWirthEngine.GameMasters.Cards

open CardWirthEngine.Cards

module Enemies =
  type t = Cast.t array

  let get = Array.tryItem
 
  let updated idx e es =
    let es' = Array.copy es in
    es'.[idx] <- e
    es'
 
  let removed i (es : t array) =
    Array.append es.[..i] es.[i+1..]

  let to_list = Array.toList

  let indexed = Array.indexed

  let map_at idx f es =
    get idx es
    |> Option.fold
         (fun _ e -> updated idx (f e) es)
         es