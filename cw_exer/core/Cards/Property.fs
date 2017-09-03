namespace CardWirthEngine.Cards

open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Skills

module Property =
  type t =
    { id : int
    ; name : string
    ; image : Path
    ; description : string
    ; scenario : string
    ; author : string
    ; ability : Ability.t
    ; target : Target.t
    ; effect_type : EffectType.t
    ; resist_type : Resist.t
    ; success_rate : int
    ; visual_effect : CardVisual.t
    ; enhance : Enhance
    ; sound_paths : Path array
    ; key_codes : string list
    ; premium : Premium.t
    ; use_limit : int
    ; materials : Path
    }