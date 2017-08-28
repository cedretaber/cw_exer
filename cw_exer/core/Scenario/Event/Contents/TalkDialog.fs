namespace CardWirthEngine.Scenario.Event.Contents

open CardWirthEngine.Data.Types

module TalkDialog =
  type ImageType
    = File of Path
    | Selected
    | Unselected
    | Random
    | Card

  type t =
    { image : ImageType
    ; text : string
    }

  let t image text = { image = image; text = text }