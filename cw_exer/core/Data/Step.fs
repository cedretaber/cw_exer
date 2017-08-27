namespace CardWirthEngine.Data

module Step =
  type name = string
  type label = string
  type status = int

  type t = label array

  exception InvalidStepLengthException

  let t : string array -> t =
    fun str_arr ->
      if Array.length str_arr = 10
        then str_arr
        else raise InvalidStepLengthException