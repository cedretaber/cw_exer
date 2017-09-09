﻿namespace CardWirthEngine.Scenario.Events

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Casts
open CardWirthEngine.Scenario.Events.Contents

module Content =

  let select : int -> 'a list -> 'a option = List.tryItem
  let next : ('a -> 'b option) -> 'a list -> 'b option = List.tryPick

  module SourceFlag =
    type t
      = Random
      | From of Flag.Name

  module SourceStep =
    type t
      = Random
      | SelectedPc
      | From of Step.Name
      
  type Nexts = t list
  and Texts = (string * t) list
  and Bools = (bool * t) list
  and Steps = (Step.State * t) list
  and AreaIds = (AreaId * t) list
  and BattleIds = (BattleId * t) list
  and Trios = (Comparison.t * t) list

  and 'a NextContent
    = Nexts of Nexts
    | List of ('a * t) list
  
  and t
    (* Terminal *)
    = Start of Nexts * name : StartName
    | StartBattle of battle_id : BattleId
    | End of is_complete : IsCompleted
    | EndBadEnd
    | ChangeArea of area_id : AreaId
    | EffectBreak
    | LinkStart of link_name : string
    | LinkPackage of package_id : PackageId
    (* Standard *)
    | TalkMessage of Texts * TalkMessage.t
    | TalkDialog of Texts * TalkDialog.t
    | PlayBgm of Nexts * bgm : Bgm * play : Play.t
    | PlaySound of Nexts * sound : Sound * play : Play.t
    | Wait of Nexts * value : Decisecond
    | ElaspeTime of Nexts
    | Effect of Nexts * effect : Effect.t
    | CallStart of Nexts * name : StartName * returned : bool
    | CallPackage of Nexts * name : PackageId * returned : bool
    (* Data *)
    | BranchFlag of Bools * flag : Flag.Name
    | SetFlag of Nexts * flag : Flag.Name * value : Flag.State
    | ReverseFlag of Nexts * flag : Flag.Name
    | SubstituteFlag of Nexts * source : SourceFlag.t * target : Flag.Name
    | BranchFlagCmp of Bools * left : Flag.Name * right : Flag.Name
    | CheckFlag of Nexts * flag : Flag.Name
    | BranchStep of Bools * step : Step.Name * value : Step.State
    | SetStep of Nexts * step : Step.Name * value : Step.State
    | SetStepUp of Nexts * step : Step.Name
    | SetStepDown of Nexts * step : Step.Name
    | SubstituteStep of Nexts * source : SourceStep.t * target : Step.Name
    | BranchMultiStep of Steps * step : Step.Name
    | BranchStepCmp of Trios * left : Step.Name * right : Step.Name
    | CheckStep of Nexts * step : Step.Name * value : Step.State * cmp : Comparison.t
    (* Utility *)
    | BranchSelect of Bools * BranchSelect.t
    | BranchAbility of Bools * BranchAbility.t
    | BranchRandom of Bools * value : Percent
    | BranchMultiRandom of Nexts (* Wsn.2 *)
    | BranchLevel of Bools * target : Target * level : int
    | BranchStatus of Bools * target : Target * status : Status
    | BranchPartyNumber of Bools * value : int
    | BranchArea of Bools * id : AreaId
    | BranchBattle of Bools * id : BattleId
    | BranchIsBattle of Bools
    | BranchRandomSelect of Bools * BranchRandomSelect.t
    | BranchRound of Bools * value : int * cmp : Comparison.t
    (* Branch *)
    | BranchCast of Bools * cast_id : CastId
    | BranchItem of Bools * item_id : ItemId
    | BranchSkill of Bools * skill_id : SkillId
    | BranchInfo of Bools * info_id : InfoId
    | BranchBeast of Bools * beast_id : BeastId
    | BranchMoney of Bools * value : int
    | BranchCoupon of Bools * range : Range * value : Coupon.Name
    | BranchMultiCoupon of Texts * target : Target (* Wsn.2 *)
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
    | GetCoupon of Nexts * target : Target * point : int * value : Coupon.Name
    | GetCompleteStamp of Nexts * value : ScenarioName
    | GetGossip of Nexts * value : GossipName
    (* Lost *)
    | LoseCast of Nexts * cast_id : CastId
    | LoseItem of Nexts * item_id : ItemId * target : Range * value : int
    | LoseSkill of Nexts * skill_id : SkillId * target : Range * value : int
    | LoseInfo of Nexts * indo_id : InfoId
    | LoseBeast of Nexts * beast_id : BeastId * target : Range * value : int
    | LoseMoney of Nexts * value : int
    | LoseCoupon of Nexts * target : Target * value : Coupon.Name
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