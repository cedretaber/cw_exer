namespace CardWirthEngine.Scenario.Events.Contents

open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Casts

module TalkDialog =
  type ImageType
    = File of Path
    | Selected
    | Unselected
    | Random
    | Card
  type Dialog =
    { required_coupons : Coupon.Name list
    ; text : string
    }

  type t =
    { image : ImageType
    ; boundarycheck : bool
    ; centeringx : bool
    ; columns : int
    ; dialogs : Dialog list
    }

  let t image boundarycheck centeringx columns dialogs =
    { image = image
    ; boundarycheck = boundarycheck
    ; centeringx = centeringx
    ; columns = columns
    ; dialogs = dialogs
    }
  
  let t' image dialogs selections = t image false false 1 dialogs