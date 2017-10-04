module CardWirthEngine.Scenario.Events.Contents.MoveBackgroundImage

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
  ; transition : Transition
  ; transition_speed : int
  ; doanime : bool
  ; ignore_effectbooster : bool
  }