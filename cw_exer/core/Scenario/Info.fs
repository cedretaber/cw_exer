namespace CardWirthEngine.Scenario

open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Flag
open CardWirthEngine.Data.Step

module Info =
  type t =
    { name : ScenarioName
    ; author : AuthorName
    ; flags : 
    }