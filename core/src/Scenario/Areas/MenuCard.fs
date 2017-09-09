namespace CardWirthEngine.Scenario.Areas

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario

module MenuCard =
  type Property =
    { name : string
    ; path : Path
    ; description : string
    ; flag : Flag.Name
    ; size : Percent
    }
  
  type t =
    { property : Property
    ; events : Event.t list
    }

