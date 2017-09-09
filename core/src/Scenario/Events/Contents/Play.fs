namespace CardWirthEngine.Scenario.Events.Contents

open CardWirthEngine.Data.Types

module Play =
  type t =
    { fadein : int
    ; volume : int
    ; channel : int
    ; loop_count : int
    }