namespace CardWirthEngineTest

open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario.Info.Summary
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
      { casts = Map.empty
      ; skills = Map.empty
      ; items = Map.empty
      ; beasts = Map.empty
      ; infos = Map.empty
      }
    ; current_area = Area 0
    ; global_state =
      { flags = Map.empty
      ; steps = Map.empty
      }
    ; eventStack = []
    ; selected = SelectedCast.None
    ; companions = no_adventurers
    ; bgm = Bgm.Stop
    }