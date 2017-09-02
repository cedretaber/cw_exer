namespace CardWirthEngine.Scenario.Event.Contents

open CardWirthEngine.Data.Types

module TalkDialog =
  type ImageType
    = File of Path
    | Selected
    | Unselected
    | Random
    | Card
  type Dialog =
    { required_coupons : CouponName list
    ; text : string
    }

  type t =
    { image : ImageType
    ; boundarycheck : bool
    ; centeringx : bool
    ; columns : int
    ; dialogs : Dialog list
    ; selections: string array
    }

  let t image boundarycheck centeringx columns dialogs selections =
    { image = image
    ; boundarycheck = boundarycheck
    ; centeringx = centeringx
    ; columns = columns
    ; dialogs = dialogs
    ; selections = selections
    }
  
  let t' image dialogs selections = t image false false 1 dialogs selections