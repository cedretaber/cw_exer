namespace CardWirthEngine.Data

module Flag =
  type Name = string
  type Label = string
  type State = bool

  type t =
    { true_label : Label
    ; false_label : Label
    ; default_status : State
    }

  let t : string -> string -> bool -> t =
    fun true_label false_label default_status ->
      { true_label = true_label
      ; false_label = false_label
      ; default_status = default_status
      }