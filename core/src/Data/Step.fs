namespace CardWirthEngine.Data

module Step =

  type Name = string
  type Label = string
  type State = int

  type t = 
    { steps : Label array
    ; default_status : State
    ; spchars : bool
    }

  let t : string array -> int -> bool -> t =
    fun str_arr default_status spchars ->
      { steps = str_arr
      ; default_status = default_status
      ; spchars = spchars
      }

  let t' : string array -> int -> t =
    fun str_arr default_status ->
      t str_arr default_status false