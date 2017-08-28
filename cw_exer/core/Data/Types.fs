namespace CardWirthEngine.Data

module Types =

  (* ID *)
  type AreaId = int
  type BattleId = int
  type PackageId = int
  type CastId = int
  type SkillId = int
  type ItemId = int
  type BeastId = int
  type InfoId = int

  type ScenarioName = string
  type AuthorName = string
  type GossipName = string
  type StartName = string

  type KeyCode = string

  type CouponName = string

  (* シナリオが終了済みか否か *)
  type IsCompleted = bool

  type Path = string

  type Decisecond = uint32

  type Percent = uint8

  type Mentality
    = Normal // 正常
    | Sleep // 睡眠
    | Confuse // 混乱
    | Overheat // 激昂
    | Brave // 勇敢
    | Panic // 恐慌

  type EffectType
    = Physic // 物理
    | Magic // 魔法
    | MagicalPhysic // 魔法的物理
    | PhysicalMagic // 物理的魔法
    | None // 無

  type Resist
    = Avoid// 回避
    | Resist // 抵抗
    | Unfail // 必中

  type CardVisual
    = None// 無し
    | Reverse // 反転
    | Horizontal// 横震動
    | Vertical // 縦振動

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

  type PlayerPosition
    = First
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth

  type Target
    = Selected of sleep : bool // 選択中メンバ
    | Unselected of sleep : bool // 非選択メンバ
    | Random of sleep : bool // ランダムメンバ
    | Party of sleep : bool // 全員

  type Mental
    = Aggressive // 好戦
    | Unaggressive // 平和
    | Cheerful // 社交
    | Uncheerful // 内向
    | Brave // 勇敢
    | Unbrave // 臆病
    | Cautious // 慎重
    | Uncautious // 大胆
    | Trickish // 狡猾
    | Untrickish // 正直

  type Physical
    = Dex // 敏捷度
    | Agl // 器用度
    | Int // 知力
    | Str // 膂力
    | Vit // 生命力
    | Min // 精神力

  type Status
    = Active // 行動可能
    | Inactive // 行動不可
    | Alive // 生存
    | Dead // 非生存
    | Fine // 健康
    | Injured // 負傷
    | HeavyInjured // 重症
    | Unconscious // 意識不明
    | Poison // 中毒
    | Sleep // 睡眠
    | Bind // 呪縛
    | Paralyze // 麻痺/石化
    | Confuse // 混乱(CardWirth Extender 1.30～)
    | Overheat // 激昂(CardWirth Extender 1.30～)
    | Brave // 勇敢(CardWirth Extender 1.30～)
    | Panic // 恐慌(CardWirth Extender 1.30～)
    | Silence // 沈黙(CardWirth 1.50)
    | FaceUp // 暴露(CardWirth 1.50)
    | AntiMagic // 魔法無効化(CardWirth 1.50)
    | UpAction // 行動力上昇(CardWirth 1.50)
    | UpAvoid // 回避力上昇(CardWirth 1.50)
    | UpResist // 抵抗力上昇(CardWirth 1.50)
    | UpDefense // 防御力上昇(CardWirth 1.50)
    | DownAction // 行動力低下(CardWirth 1.50)
    | DownAvoid // 回避力低下(CardWirth 1.50)
    | DownResist // 抵抗力低下(CardWirth 1.50)
    | DownDefense // 防御力低下(CardWirth 1.50)
    | None // 状態指定無し

  type Range
    = Selected // 選択中メンバ
    | Random // 誰か一人
    | Party // パーティ全員
    | Backpack // 荷物袋
    | PartyAndBackpack // 全員と荷物袋
    | Field // フィールド全体
    | CouponHolder // 称号所有者(Wsn.2)
    | CardTarget // カードの効果対象(Wsn.2)。

  type Enhance
    = Action // 行動
    | Avoid // 回避
    | Resist // 抵抗
    | Defense // 防御

  type Premium
    = Normal // 日用品
    | Rare // 希少品
    | Premium // 貴重品

  type CardTarget
    = None // 対象無し
    | User // 使用者
    | Party // 味方
    | Enemy // 敵方
    | Both // 双方

  type Transition
    = Default // ユーザ指定
    | None // アニメーション無し
    | Blinds // ブラインド式
    | PixelDissolve // ピクセルディゾルブ式
    | Fade // フェード式

  type Comparison4 = Eq | Ne | Lt | Gt

  type Comparison3 = LT | EQ | GT

  type BlendMode
    = Normal // 標準
    | Mask // 無効
    | Add // 加算
    | Subtract // 減算
    | Multiply // 乗算

  type GradientDir
    = None // グラデーション無し
    | LeftToRight // 左から右へ
    | TopToBottom // 上から下へ

  type BorderingType
    = None // 縁取り無し
    | Outline // 外側を縁取り
    | Inline // 内側を縁取り

  type CoordinateType
    = None // 座標指定無効
    | Absolute // 絶対位置
    | Relative // 相対位置
    | Percentage // パーセンテージ

  (* キャスト同行時の戦闘行動開始タイミング(Wsn.2) *)
  type StartAction
    = Now // 即時に行動する(無指定の場合のデフォルト)
    | CurrentRound // ラウンドイベントで加入した場合はそのラウンドから行動する
    | NextRound // 次ラウンドから行動する(クラシックなシナリオのデフォルト)

  (* 背景セルのスムージング設定(Wsn.2)。 *)
  type Smoothing
    = Default // エンジンの設定を使用する
    | True // 強制的にスムージングする
    | False // 強制的にスムージングしない

  type MType
    = Heal
    | Damage
    | Absorb
    | Paralyze
    | DisParalyze
    | Poison
    | DisPoison
    | GetSkillPower
    | LoseSkillPower
    | Sleep
    | Confuse
    | Overheat
    | Brave
    | Panic
    | Normal
    | Bind
    | DisBind
    | Silence
    | DisSilence
    | FaceUp
    | FaceDown
    | AntiMagic
    | DisAntiMagic
    | EnhanceAction
    | EnhanceAvoid
    | EnhanceResist
    | EnhanceDefense
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
    | SummonBeast
    | CancelAction
    | NoEffect

  type MArg
    = ValueType // レベル比・直接等、値のタイプ
    | UValue // ダメージ・回復量
    | AValue // ボーナス値
    | Round // 継続ラウンド数
    | Beast // 召喚獣

  type CardImagePosition
    = Default // 指定無し(クラシックな位置に合わせる)
    | Center // 中央寄せ
    | TopLeft // 左上起点

  type MatchingType
    = And // 全てに一致
    | Or // どれか一つに一致

  type SpreadType
    = Auto

  type Bgm
    = Midi of Path
    | Mp3 of Path

  type Sound
    = Wave of Path

  type Location =
    { left : int
    ; top : int
    }

  type Size =
    { width : int
    ; height : int
    }
  
  type Effect = unit