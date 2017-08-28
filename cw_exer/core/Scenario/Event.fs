namespace CardWirthEngine.Scenario

open CardWirthEngine.Data.Types
open CardWirthEngine.Scenario.Events

module Event =
  type Ignitions =
    { number : int
    ; key_codes : string Set
    }

  type t = 
    { ignitions : Ignitions
    ; lines : (StartName, Content.t list) Map
    }

  let find : StartName -> t -> Content.t Option =
    fun start_name t ->
      t.lines
      |> Map.tryFind start_name
      |> Option.fold
        (fun _ list -> List.tryHead list) Option.None