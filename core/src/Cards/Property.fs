namespace CardWirthEngine.Cards

open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Skills

module Property =
  type 'id t =
    { id : 'id
    ; name : string
    ; image : Path
    ; description : string
    ; scenario : string
    ; author : string
    ; ability : Ability.t
    ; target : Target.t
    ; effect_type : EffectType.t
    ; resist_type : Resist.t
    ; success_rate : SuccessRate
    ; visual_effect : CardVisual.t
    ; enhance : Enhance.m
    ; sound_paths : Path array
    ; key_codes : KeyCode list
    ; premium : Premium.t
    ; use_limit : UseLimit
    ; materials : Path
    }

  let inline equals left right =
    left.name = right.name
    && left.author = right.author
    && left.description = right.description