namespace CardWirthEngine.Scenario.Events

open CardWirthEngine.Data
open CardWirthEngine.Data.Types
open CardWirthEngine.Scenario.Event.Contents

module rec Content =

  type Next = t option
  type Nexts = t array
  type Bools = (bool, t) Map
  type Steps = (Step.State, t) Map
  type AreaIds = (AreaId, t) Map
  type BattleIds = (BattleId, t) Map
  type Trios = (Comparison3, t) Map

  module SourceFlag =
    type t
      = Random
      | From of Flag.Name

  type NextContent<'a when 'a : comparison>
    = Next of Next
    | Nexts of Nexts
    | Map of ('a, t) Map
  
  let next : 'a -> ('a, t) Map -> t option =
    Map.tryFind
  let next' : int -> t array -> t option =
    fun idx ->
      function
      | arr when Array.length arr < idx -> Some arr.[idx]
      | _ -> Option.None

  type t
    (* Terminal *)
    = Start of Next * name : StartName
    | StartBattle of battle_id : BattleId
    | End of is_complete : IsCompleted
    | EndBadEnd
    | ChangeArea of area_id : AreaId
    | EffectBreak
    | LinkStart of link_name : string
    | LinkPackage of package_id : PackageId
    (* Standard *)
    | TalkMessage of Nexts * TalkMessage.t
    | TalkDialog of Nexts * TalkDialog.t
    | PlayBgm of Next * bgm : Bgm
    | PlaySound of Next * sound : Sound
    | Wait of Next * value : Decisecond
    | ElaspeTime of Next
    | Effect of Next * effect : Effect
    | CallStart of Next * name : StartName
    | CallPackage of Next * name : PackageId
    (* Data *)
    | BranchFlag of Bools * flag : Flag.Name
    | SetFlag of Next * flag : Flag.Name * value : Flag.State
    | ReverseFlag of Next * flag : Flag.Name
    | SubstituteFlag of Next * source : SourceFlag.t * target : Flag.Name
    | BranchFlagCmp of Bools * left : Flag.Name * right : Flag.Name
    | CheckFlag of Next * flag : Flag.Name
    | BranchStep of Bools * step : Step.Name * value : Step.State
    | SetStep of Next * step : Step.Name * value : Step.State
    | SetStepUp of Next * step : Step.Name
    | SetStepDown of Next * step : Step.Name
    | SubstituteStep of Next * source : Step.State * target : Step.State
    | BranchMultiStep of Steps * step : Step.Name
    | BranchStepCmp of Trios * left : Step.Name * right : Step.Name
    | CheckStep of Next * step : Step.Name
    (* Utility *)
    | BranchSelect of Bools * BranchSelect.t
    | BranchAbility of Bools * BranchAbility.t
    | BranchRandom of Bools * value : Percent
    | BranchMultiRandom of Nexts (* Wsn.2 *)
    | BranchLevel of Bools * target : Target * level : int
    | BranchStatus of Bools * target : Target * status : Status
    | BranchPartyNumber of Bools * value : int
    | BranchArea of AreaIds
    | BranchBattle of BattleIds
    | BranchIsBattle of Bools
    | BranchRandomSelect of Bools * BranchRandomSelect.t
    | BranchRound of Bools * value : int
    (* Branch *)
    | BranchCast of Bools * cast_id : CastId
    | BranchItem of Bools * item_id : ItemId
    | BranchSkill of Bools * skill_id : SkillId
    | BranchInfo of Bools * info_id : InfoId
    | BranchBeast of Bools * beast_id : BeastId
    | BranchMoney of Bools * value : int
    | BranchCoupon of Bools * range : Range * value : CouponName
    | BranchMultiCoupon of Next * target : Target (* Wsn.2 *)
    | BranchCompleteStamp of Bools * value : ScenarioName
    | BranchGossip of Bools * value : GossipName
    | BranchKeyCode of Bools * BranchKeyCode.t
    (* Get *)
    | GetCast of Next * cast_id : CastId * start_action : StartAction
    | GetItem of Next * item_id : ItemId * target : Range * value : int
    | GetSkill of Next * skill_id : SkillId * target : Range * value : int
    | GetInfo of Next * indo_id : InfoId
    | GetBeast of Next * beast_id : BeastId * target : Range * value : int
    | GetMoney of Next * value : int
    | GetCoupon of Next * target : Target * point : int * value : CouponName
    | GetCompleteStamp of Next * value : ScenarioName
    | GetGossip of Next * value : GossipName
    (* Lost *)
    | LoseCast of Next * cast_id : CastId
    | LoseItem of Next * item_id : ItemId * target : Range * value : int
    | LoseSkill of Next * skill_id : SkillId * target : Range * value : int
    | LoseInfo of Next * indo_id : InfoId
    | LoseBeast of Next * beast_id : BeastId * target : Range * value : int
    | LoseMoney of Next * value : int
    | LoseCoupon of Next * target : Target * value : CouponName
    | LoseCompeteStamp of Next * value : ScenarioName
    | LoseGossip of Next * value : GossipName
    (* Visual *)
    | ShowParty of Next
    | HideParty of Next
    | ChangeBgImage of Next
    | MoveBgImage of Next
    | ReplaceBgImage of Next
    | LoseBgImage of Next
    | Redisplay of Next