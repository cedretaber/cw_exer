namespace CardWirthEngineTest

open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Casts
open CardWirthEngine.Scenario.Info.Summary
open CardWirthEngine.Cards
open CardWirthEngine.Cards.Cast.Feature
open CardWirthEngine.GameMasters.Cards
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

  let one_adventurer : Adventurers.t =
    Adventurers.add empty_cast no_adventurers

  let empty_scenario : Scenario =
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
      }
    ; eventStack = []
    ; selected = SelectedCast.None
    ; companions = one_adventurer // 冒険者は1人
    ; bgm = Bgm.Stop
    }