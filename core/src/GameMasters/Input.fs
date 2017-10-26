module CardWirthEngine.GameMasters.Input

open CardWirthEngine.Data.Type

open CardWirthEngine.GameMasters.Cards

type t
  (* システム系 *)
  = Loaded // ファイル読み込み成功
  | LoadFailed of string // ファイル読み込み失敗
  (* フィールド操作 *)
  | Click of int // カードクリック（カードID）
  | Action of Adventurers.Position * ActionCard * int // カード使用（カード種別・添字）
  (* ダイアログ・選択肢 *)
  | NextMessage of int // 次の選択肢（選択肢番号）
  | SelectPlayerCharactor of Adventurers.Position // PC選択
  | Cancel // PCキャンセル
  (* その他 *)
  | Redisplayed // 描写終了
  | Void // 情報を持たない入力
  | None // 入力ではない = 前コンテンツからの継続

let (|System|Field|Dialog|Else|) =
  function Loaded | LoadFailed _ -> System
         | Click _ | Action (_, _, _) -> Field
         | NextMessage _ | SelectPlayerCharactor _ | Cancel -> Dialog
         | Redisplayed | Void | None -> Else