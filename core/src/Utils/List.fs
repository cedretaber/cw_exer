﻿module CardWirthEngine.Utils.List

let inline filter_not f = List.filter (f >> not)

let filter_not_limited count f =
  let rec fl_go count acm =
    function
      [] -> List.rev acm
    | list when count = 0 -> (List.rev acm) @ list
    | head :: tail when f head -> fl_go (count - 1) acm tail
    | head :: tail -> fl_go count (head :: acm) tail in
  fl_go count []

let rec multi_cons count elem list =
  if count = 0
  then list
  else multi_cons (count - 1) elem (elem :: list)

let inline count_by f = List.sumBy <| fun e -> if f e then 1 else 0

// Not tailrec
let rec fold_right f start =
  function
    [] ->
      start
  | head :: tail ->
      f head <| fold_right f start tail