module CardWirthEngine.Utils.Array

let inline updated index value old_arr =
  let new_arr = Array.copy old_arr in
  new_arr.[index] <- value
  new_arr

let inline remove index arr =
  [|
    for idx, e in Array.indexed arr do
      if idx <> index then
        yield e
  |]