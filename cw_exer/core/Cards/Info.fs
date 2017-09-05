namespace CardWirthEngine.Cards

open CardWirthEngine.Data.Type

module Info =
  type t =
    { id : InfoId
    ; name : string
    ; description : string
    }