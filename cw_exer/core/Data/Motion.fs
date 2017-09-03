namespace CardWirthEngine.Data

module Motion =
  type Element
    = All // 全
    | Health // 肉体
    | Mind // 精神
    | Miracle // 神聖
    | Magic // 魔法
    | Fire // 炎
    | Ice // 冷気
  type DamageType
    = LevelRatio // レベル比
    | Normal // 値の直接指定
    | Max // 最大値
    | Fixed // 固定値(Wsn.1)
  type Motion
    = Heal of value : int * damega_type : DamageType
    | Damage of value : int * damega_type : DamageType
    | Absorb of value : int * damega_type : DamageType
    | Paralyze of value : int * damega_type : DamageType
    | DisParalyze of value : int * damega_type : DamageType
    | Poison of value : int * damega_type : DamageType
    | DisPoison of value : int * damega_type : DamageType
    | GetSkillPower of value : int * damega_type : DamageType
    | LoseSkillPower of value : int * damega_type : DamageType
    | Sleep of duration : int
    | Confuse of duration : int
    | Overheat of duration : int
    | Brave of duration : int
    | Panic of duration : int
    | Normal
    | Bind of duration : int
    | DisBind
    | Silence of duration : int
    | DisSilence
    | FaceUp of duration : int
    | FaceDown
    | AntiMagic of duration : int
    | DisAntiMagic
    | EnhanceAction of value : int * duration : int
    | EnhanceAvoid of value : int * duration : int
    | EnhanceResist of value : int * duration : int
    | EnhanceDefense of value : int * duration : int
    | VanishTarget
    | VanishCard
    | VanishBeast
    | DealAttackCard
    | DealPowerfulAttackCard
    | DealCriticalAttackCard
    | DealFeintCard
    | DealDefenseCard
    | DealDistanceCard
    | DealConfuseCard
    | DealSkillCard
    | SummonBeast of int
    | CancelAction
    | NoEffect

  type t =
    { motion : Motion
    ; element : Element
    }