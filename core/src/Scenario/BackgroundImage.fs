module CardWirthEngine.Scenario.BackgroundImage

open Aether
open Aether.Operators

open CardWirthEngine.Data
open CardWirthEngine.Data.Type

type Property =
  { cellname : string option
  ; location : Location
  ; size : Size
  ; flag : Flag.Name option
  }
  with
    static member cellname_ =
      (fun p -> p.cellname), (fun c p -> { p with cellname = c })
    static member location_ =
      (fun p -> p.location), (fun l p -> { p with location = l })
    static member size_ =
      (fun p -> p.size), (fun s p -> ({ p with size = s } : Property))
    static member flag_ =
      (fun p -> p.flag), (fun f p -> { p with flag = f })

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
    static member property_ =
      (function BackgroundImage (p, _) -> p
              | ColorCell (p, _) -> p
              | TextCell (p, _) -> p
              | PCCell (p, _) -> p)
      , (fun p -> function BackgroundImage (_, bi) -> BackgroundImage (p, bi)
                         | ColorCell (_, cc) -> ColorCell (p, cc)
                         | TextCell (_, tc) -> TextCell (p, tc)
                         | PCCell (_, pc) -> PCCell (p, pc))

let get_cellname = t.property_ >-> Property.cellname_ |> Optic.get

let private location_ = t.property_ >-> Property.location_
let get_location = Optic.get location_
let set_location = Optic.set location_
let map_location = Optic.map location_

let private size_ = t.property_ >-> Property.size_
let get_size = Optic.get size_
let set_size = Optic.set size_
let map_size = Optic.map size_

let get_flag = t.property_ >-> Property.flag_ |> Optic.get

let is_inherited image =
  match get_flag image, get_location image, get_size image with
    Some _, _, _ ->
      (* フラグがある場合は無条件で背景継承 *) true
  | Option.None, { left = left; top = top }, _ when left > 0 || top > 0 ->
      (* 左上に達していない場合は背景継承 *) true
  | Option.None, { left = left; top = top }, { width = width; height = height } ->
      (* 画面全体を覆っていない場合は背景継承 *)
      width + left < 632 || height + top < 420
      (* そうでない場合、則ち画面全体をフラグを持たない画像が覆っている場合は背景を継承しない *)