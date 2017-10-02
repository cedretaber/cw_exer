module CardWirthEngine.Scenario.BackgroundImage

open CardWirthEngine.Data
open CardWirthEngine.Data.Type

type Property =
  { cellname : string option
  ; location : Location
  ; size : Size
  ; flag : Flag.Name option
  }

type BackgroundImage =
  { smoothing : Smoothing
  ; mask : bool
  ; path : Path
  }

type ColorCell =
  { blend_mode : BlendMode
  ; color : Color
  }

type TextCell =
  { text : string
  ; font : Font
  ; vertical : bool
  ; color : Color
  }

type PCCell =
  { smoothing : Smoothing
  ; pc_number : int
  }

type t
  = BackgroundImage of Property * BackgroundImage
  | ColorCell of Property * ColorCell
  | TextCell of Property * TextCell
  | PCCell of Property * PCCell
  with
    member this.property =
      match this with
        BackgroundImage (property, _) -> property
      | ColorCell (property, _) -> property
      | TextCell (property, _) -> property
      | PCCell (property, _) -> property
    
    member this.cellname = this.property.cellname
    member this.location = this.property.location
    member this.size = this.property.size
    member this.flag = this.property.flag

let is_inherited (image : t) : bool =
  match image.flag, image.location, image.size with
    Some _, _, _ ->
      (* フラグがある場合は無条件で背景継承 *) true
  | Option.None, { left = left; top = top }, _ when left > 0 || top > 0 ->
      (* 左上に達していない場合は背景継承 *) true
  | Option.None, { left = left; top = top }, { width = width; height = height } ->
      (* 画面全体を覆っていない場合は背景継承 *)
      width + left < 632 || height + top < 420
      (* そうでない場合、則ち画面全体をフラグを持たない画像が覆っている場合は背景を継承しない *)