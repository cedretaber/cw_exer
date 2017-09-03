namespace CardWirthEngine.Scenario.Event.Contents

open CardWirthEngine.Data
open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Skills

module Effect =
  type SoundSetting =
    { path : Path
    ; volume : int
    ; loop_count : int
    }
  type Settings =
    { effect_type : EffectType.t
    ; level : int
    ; visual : CardVisual.t
    ; sound : SoundSetting
    ; resist_type : Resist.t
    ; success_rate : int
    }
  type Ability =
    { ref : bool
    ; ability : Ability.t
    }
  type Ignite =
    { ignite : bool
    ; coupons : string list
    }

  type t =
    { motions : Motion.t list
    ; settings : Settings
    ; ability : Ability
    ; ignite : Ignite
    }