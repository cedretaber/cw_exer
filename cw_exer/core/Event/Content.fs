namespace CardWirthEngine.Event

open CardWirthEngine.Data
open CardWirthEngine.Data.Types
open CardWirthEngine.Event.Contents

module rec Content =

  type Nexts = t list
  type Texts = (StartName * t) list
  type Bools = (bool * t) list
  type Steps = (Step.status * t) list
  type AreaIds = (AreaId * t) list
  type BattleIds = (BattleId * t) list
  type Trios = (Comparison3 * t) list

  type t
    (* Terminal *)
    = Start of name : StartName
    | StartBattle of battle_id : BattleId
    | End of is_complete : isCompleted
    | EndBadEnd
    | ChangeArea of area_id : AreaId
    | EffectBreak
    | LinkStart of link_name : string
    | LinkPackage of package_id : PackageId
    (* Standard *)
    | TalkMessage of Texts * TalkMessage.t
    | TalkDialog of Texts * TalkDialog.t
    | PlayBgm of Nexts * bgm : Bgm
    | PlaySound of Nexts * sound : Sound
    | Wait of Nexts * value : Decisecond
    | ElaspeTime of Nexts
    | Effect of Nexts * effect : Effect
    | CallStart of Nexts * name : StartName
    | CallPackage of Nexts * name : PackageId
    (* Data *)
    | BranchFlag of Bools * flag : Flag.name
    | SetFlag of Nexts * flag : Flag.name * value : Flag.status
    | ReverseFlag of Nexts * flag : Flag.name
    | SubstituteFlag of Nexts * source : Flag.status * target : Flag.status
    | BranchFlagCmp of Bools * left : Flag.name * right : Flag.name
    | CheckFlag of Nexts * flag : Flag.name
    | BranchMultiStep of Steps * step : Step.name
    | BranchStep of Bools * step : Step.name * value : Step.status
    | SetStep of Nexts * step : Step.name * value : Step.status
    | SetStepUp of Nexts * step : Step.name
    | SetStepDown of Nexts * step : Step.name
    | SubstituteStep of Nexts * source : Step.status * target : Step.status
    | BranchStepCmp of Trios * left : Step.name * right : Step.name
    | CheckStep of Nexts * step : Step.name
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
    | BranchMultiCoupon of Nexts (* Wsn.2 *)
    | BranchCompleteStamp of Bools * value : ScenarioName
    | BranchGossip of Bools * value : GossipName
    | BranchKeyCode of Bools * BranchKeyCode.t
    (* Get *)
    | GetCast of Nexts * cast_id : CastId * start_action : StartAction
    | GetItem of Nexts * item_id : ItemId * target : Range * value : int
    | GetSkill of Nexts * skill_id : SkillId * target : Range * value : int
    | GetInfo of Nexts * indo_id : InfoId
    | GetBeast of Nexts * beast_id : BeastId * target : Range * value : int
    | GetMoney of Nexts * value : int
    | GetCoupon of Nexts * target : Target * point : int * value : CouponName
    | GetCompleteStamp of Nexts * value : ScenarioName
    | GetGossip of Nexts * value : GossipName
    (* Lost *)
    | LoseCast of Nexts * cast_id : CastId
    | LoseItem of Nexts * item_id : ItemId * target : Range * value : int
    | LoseSkill of Nexts * skill_id : SkillId * target : Range * value : int
    | LoseInfo of Nexts * indo_id : InfoId
    | LoseBeast of Nexts * beast_id : BeastId * target : Range * value : int
    | LoseMoney of Nexts * value : int
    | LoseCoupon of Nexts * target : Target * value : CouponName
    | LoseCompeteStamp of Nexts * value : ScenarioName
    | LoseGossip of Nexts * value : GossipName
    (* Visual *)
    | ShowParty of Nexts
    | HideParty of Nexts
    | ChangeBgImage of Nexts
    | MoveBgImage of Nexts
    | ReplaceBgImage of Nexts
    | LoseBgImage of Nexts
    | Redisplay of Nexts