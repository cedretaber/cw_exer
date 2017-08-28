namespace CardWirthEngine.Scenario.Event.Contents

open CardWirthEngine.Data.Types

module TalkMessage =
  type ImageType
    = PCNumber // PCの画像
    | File of Path // ファイル
    | Selected // 選択中メンバ
    | Unselected // 非選択メンバ
    | Random // ランダムメンバ
    | Card // カード
    | Valued of (CouponName * int) list // 評価メンバ
  
  type t =
    { image : ImageType
    ; text : string
    }

  let t image text = { image = image; text = text }