namespace CardWirthEngine.Scenario.Info

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Casts

module Summary =
  type DataVersion = int

  type Property =
    { name : ScenarioName
    ; image_paths : Path list
    ; author : AuthorName
    ; description : string
    ; level : LevelRange
    ; required_coupons : (Coupon.Name * int) list
    ; start_area_id : AreaId
    ; tags : string list
    ; scenario_type : string
    }
  type t =
    { data_version : DataVersion
    ; property : Property
    ; flags : (Flag.Name, Flag.t) Map
    ; steps : (Step.Name, Step.t) Map
    }