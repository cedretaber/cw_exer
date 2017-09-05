namespace CardWirthEngine.Scenario.Info

open CardWirthEngine.Data
open CardWirthEngine.Data.Type

module Summary =
  type DataVersion = int
  type Level = { max : int; min : int }

  type Property =
    { name : ScenarioName
    ; image_paths : Path list
    ; author : AuthorName
    ; description : string
    ; level : Level
    ; required_coupons : (CouponName * int) list
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