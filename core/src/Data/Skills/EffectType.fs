namespace CardWirthEngine.Data.Skills

module EffectType =
  type EffectType
    = Physic // 物理
    | Magic // 魔法
    | MagicalPhysic // 魔法的物理
    | PhysicalMagic // 物理的魔法
    | None // 無
  type t =
    { t : EffectType
    ; spell : bool
    }