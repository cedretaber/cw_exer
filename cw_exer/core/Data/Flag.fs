namespace CardWirthEngine.Data

module Flag =
  type name = string
  type label = string
  type status = bool

  type t =
    { true_label : label
    ; false_label : label
    }

  let t : string -> string -> t =
    fun true_label false_label ->
      { true_label = true_label; false_label = false_label }