module CardWirthEngine.Scenario.Events.Contents.MoveBackgroundImage

open CardWirthEngine.Scenario
open CardWirthEngine.Data.Type

type Position =
  { coordinate_type : CoordinateType
  ; x : int
  ; y : int
  }

type Size =
  { coordinate_type : CoordinateType
  ; height : int
  ; width : int
  }

type t =
  { cellname : string
  ; position : Position
  ; size : Size
  ; depiction : BackgroundImage.Depiction
  }