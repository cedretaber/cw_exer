namespace CardWirthEngine.Data

module Step =
  let step_length = 1024

  type Name = string
  type Label = string
  type State = int

  type t = 
    { steps : Label array
    ; default_status : State
    }

  exception InvalidStepLengthException

  let t : string array -> int -> t =
    fun str_arr default_status ->
      if Array.length str_arr = step_length
        then { steps = str_arr; default_status = default_status }
        else raise InvalidStepLengthException