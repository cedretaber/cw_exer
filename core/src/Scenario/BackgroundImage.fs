module CardWirthEngine.Scenario.BackgroundImage

open CardWirthEngine.Data
open CardWirthEngine.Data.Type

type Property =
  { cellname : string option
  ; location : Location
  ; size : Size
  ; flag : Flag.Name
  }

type BackgroundImage =
  { property : Property
  ; smoothing : Smoothing
  ; mask : bool
  ; path : Path
  }

type ColorCell =
  { property : Property
  ; blend_mode : BlendMode
  ; color : Color
  }

type TextCell =
  { property : Property
  ; text : string
  ; font : Font
  ; vertical : bool
  ; color : Color
  }

type PCCell =
  { property : Property
  ; smoothing : Smoothing
  ; pc_number : int
  }

type t
  = BackgroundImage of BackgroundImage
  | ColorCell of ColorCell
  | TextCell of TextCell
  | PCCell of PCCell