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
    ; boundarycheck : bool
    ; centeringx : bool
    ; columns : int
    ; text : string
    ; selections : string array
    }

  let t image boundarycheck centeringx columns text selections =
    { image = image
    ; boundarycheck = boundarycheck
    ; centeringx = centeringx
    ; columns = columns
    ; text = text
    ; selections = selections
    }

  let t' image text selections =
    t image false false 1 text selections