namespace CardWirthEngine.Scenario

open CardWirthEngine.Data
open CardWirthEngine.Data.Types
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Areas

module Area =
  type Property =
    { id : AreaId
    ; name : string
    }
  type BackgroundImage =
    { smoothing : string
    ; mask : bool
    ; path : Path
    ; flag : Flag.Name option
    ; location : Location
    ; size : Size
    }
  type MenuCards =
    { spread_type : SpreadType
    ; cards : MenuCard.t list
    }

  type t =
    { property : Property
    ; player_card_events : Event.t list
    ; menu_cards : MenuCards
    ; events : Event.t list
    }