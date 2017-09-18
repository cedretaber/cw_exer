namespace CardWirthEngineTest

open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Types.Enhance
open CardWirthEngine.Data.Casts
open CardWirthEngine.Data.Skills
open CardWirthEngine.Data.Skills.EffectType
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Summary
open CardWirthEngine.Cards
open CardWirthEngine.Cards.Property
open CardWirthEngine.Cards.Cast.Feature
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State

module GameMasterTestUtil =

  let empty_path : Path = "path/to/hoge"

  let no_adventurers : Adventurers.t
    = Adventurers.Nothing
    , Adventurers.Nothing
    , Adventurers.Nothing
    , Adventurers.Nothing
    , Adventurers.Nothing
    , Adventurers.Nothing

  let empty_cast : Cast.t =
    { property =
      { id = 1
      ; name = "cast1"
      ; image = empty_path
      ; description = ""
      ; level = 1
      ; life =
        { max = 1; current = 1 }
      ; feature =
        { no_effect =
          { weapon = false; magic = false }
        ; body_type =
          { unholy       = false
          ; undead       = false
          ; automaton    = false
          ; constructure = false
          }
        ; resist =
          { ice = false; fire = false }
        ; weakness =
          { ice = false; fire = false }
        }
      ; ability = 
        { physical =
          { intelligence = 6
          ; vitality     = 6
          ; dexterity    = 6
          ; mind         = 6
          ; agility      = 6
          ; strength     = 6
          }
        ; mental =
          { aggressive = 0
          ; cautious   = 0
          ; cheerful   = 0
          ; brave      = 0
          ; trickish   = 0
          }
        ; enhance =
          { action  = 0
          ; avoid   = 0
          ; resist  = 0
          ; defense = 0
          }
        }
      ; status =
        { mentality =
          { duration = 0
          ; mentality = Mentality.Normal
          }
        ; paralyze   = 0
        ; poison     = 0
        ; bind       = 0
        ; silence    = 0
        ; face_up    = 0
        ; anti_magic = 0
        }
      ; enhance =
        { action  = 0
        ; avoid   = 0 
        ; resist  = 0
        ; defense = 0
        }
      ; coupons = []
      }
    ; skill = []
    ; item  = []
    ; beast = []
    }

  let empty_enhance =
    { action  = 0
    ; avoid   = 0
    ; resist  = 0
    ; defense = 0
    }

  let empty_property id name =
    { id = id
    ; name = name
    ; image = empty_path
    ; description = ""
    ; scenario = ""
    ; author = ""
    ; ability =
      { mental = Mental.Aggressive
      ; physical = Physical.Agility
      }
    ; target =
      { ct = CardTarget.Enemy
      ; all_range = false
      }
    ; effect_type =
      { t = EffectType.Physic
      ; spell = false
      }
    ; resist_type = Resist.Avoid
    ; success_rate = 5
    ; visual_effect = CardVisual.None
    ; enhance = empty_enhance
    ; sound_paths = [|empty_path|]
    ; key_codes = []
    ; premium = Premium.Normal
    ; use_limit = 0
    ; materials = empty_path
    }

  let empty_item : Item.t =
    { property = empty_property 1 "item1"
    ; price = 0
    ; enhance_owner = empty_enhance
    ; hold = false
    ; motions = []
    ; beasts = [||]
    ; events = []
    }

  let empty_skill : Skill.t =
    { property = empty_property 1 "skill1"
    ; level = 1
    ; hold = false
    ; motions = []
    ; beasts = [||]
    ; events = []
    }

  let empty_beast : Beast.t =
    { property = empty_property 1 "beast1"
    ; motions = []
    ; beasts = [||]
    ; events = []
    }

  let empty_info : Info.t =
    { id = 1
    ; name = "info1"
    ; description = "info1 description."
    }

  let one_adventurer : Adventurers.t =
    Adventurers.add empty_cast no_adventurers

  let empty_scenario : Scenario.t =
    { summary =
      { data_version = 0
      ; property =
        { name = "empty scenario"
        ; image_paths = [empty_path]
        ; author = "nemo"
        ; description = "empty description"
        ; level = { max = 0; min = 0 }
        ; required_coupons = []
        ; start_area_id = 1
        ; tags = []
        ; scenario_type = ""
        }
      ; flags = Map.empty
      ; steps = Map.empty
      }
    ; cards =
      { casts  = Map.empty
      ; skills = Map.empty
      ; items  = Map.empty
      ; beasts = Map.empty
      ; infos  = Map.empty
      }
    ; current_area = Area 1
    ; global_state =
      { flags = Map.empty
      ; steps = Map.empty
      ; infos = Set.empty
      }
    ; eventStack = []
    ; selected = SelectedCast.None
    ; companions = no_adventurers
    ; bgm = Bgm.Stop
    }

  let minimal_party : Party.t =
    { adventurers = one_adventurer
    ; money = 0
    ; bag = []
    ; name = "team1"
    }

  let empty_global_data =
    { gossips = Set.empty
    ; completed_scenarii = Set.empty
    }

  let state_random = new System.Random ()

  let make_empty_state scenario =
    Scenario
      ( scenario
      , minimal_party
      , empty_global_data
      , state_random
      )

  let empty_scenario_state : State.t =
    make_empty_state empty_scenario

  let empty_event : Event.t =
    { ignitions = { number = 0; key_codes = Set.empty }
    ; lines = Map.empty
    }