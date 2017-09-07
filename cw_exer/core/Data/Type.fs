namespace CardWirthEngine.Data

module Type =

  type size_t = int

  (* ID *)
  type AreaId = int
  type BattleId = int
  type PackageId = int

  type CastId = int
  type SkillId = int
  type ItemId = int
  type BeastId = int
  type InfoId = int

  type EnemyId = int

  type ScenarioName = string
  type AuthorName = string
  type GossipName = string
  type StartName = string

  type Level = int
  type LevelRange = { max : Level; min : Level }

  type Money = int

  type Round = int

  type KeyCode = string

  (* シナリオが終了済みか否か *)
  type IsCompleted = bool

  type Path = string

  type Decisecond = uint32

  type Percent = uint8
  
  type Target
    = Selected
    | Unselected
    | Random
    | Party

  type BranchTarget =
    { target : Target
    ; sleep : bool
    }

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
    | Stop

  type Sound
    = Wave of Path
    | Stop

  type Location =
    { left : int
    ; top : int
    }

  type Size =
    { width : int
    ; height : int
    }