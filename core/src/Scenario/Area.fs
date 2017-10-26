namespace CardWirthEngine.Scenario

open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Areas

module Area =
  type Property =
    { id : AreaId
    ; name : string
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