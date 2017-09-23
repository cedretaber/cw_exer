namespace CardWirthEngine.Cards

open CardWirthEngine.Utils
open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Casts
open CardWirthEngine.Scenario.Events.Content

module Cast =
  let max_skill_item level =
    let max = int (ceil (float level / 2.0)) + 2 in
    if max > 10 then 10 else max

  let max_beast level =
    let max = int (float (level + 2) / 4.0) in
    if max > 5 then 5 else max

  let max_hand level =
    let max = int (ceil (float level / 2.0)) + 3 in
    if max > 11 then 11 else max

  type Life =
    { max : int
    ; current : int
    }

  module Feature =
    type NoEffect =
      { weapon : bool
      ; magic : bool
      }
    type BodyType =
      { unholy : bool
      ; undead : bool
      ; automaton : bool
      ; constructure : bool
      }
    type Resist =
      { ice : bool
      ; fire : bool
      }
    type Weakness =
      { ice : bool
      ; fire : bool
      }

    type t =
      { no_effect : NoEffect
      ; body_type : BodyType
      ; resist : Resist
      ; weakness : Weakness
      }

  module Ability =
    type Physical =
      { intelligence : int
      ; vitality : int
      ; dexterity : int
      ; mind : int
      ; agility : int
      ; strength : int
      }
    type Mental =
      { aggressive : int
      ; cautious : int
      ; cheerful : int
      ; brave : int
      ; trickish : int
      }

    type t =
      { physical : Physical
      ; mental : Mental
      ; enhance : Enhance.m
      }

  module Status =
    type Mentality =
      { duration : int
      ; mentality : Mentality.t
      }

    type t =
      { mentality : Mentality
      ; paralyze : int
      ; poison : int
      ; bind : int
      ; silence : int
      ; face_up : int
      ; anti_magic : int
      }

  type Property =
    { id: CastId
    ; name : string
    ; image : Path
    ; description : string
    ; level : Level
    ; life : Life
    ; feature : Feature.t
    ; ability : Ability.t
    ; status : Status.t
    ; enhance : Enhance.m
    ; coupons : CouponSet.t
    }

  type t =
    { property : Property
    ; skill : Skill.t list
    ; item : Item.t list
    ; beast : Beast.t list
    }
    with
      member this.life = this.property.life.current

  let inline update_property f =
    function
      { property = property } as cast ->
        { cast with property = f property }

  (* Coupon Ops *)
  let has_coupon =
    function
      ({ name = name } : Coupon.t) ->
        function
          { property = { coupons = coupons } } ->
            CouponSet.contains name coupons

  let has_coupon_by_name name =
    function
      { property = { coupons = coupons } } ->
        CouponSet.contains name coupons

  let add_coupon coupon =
    update_property <| fun property ->
      { property with coupons = CouponSet.add coupon property.coupons }


  (* Cast statuses *)
  let inline is_alive cast =
    let prop = cast.property in
    match prop with
      (* 体力が0 *)
      { life = { current = life } } when life <= 0 -> false
      (* 麻痺または石化 *)
    | { status = { paralyze = paralyze } } when paralyze > 0 -> false
    | _ -> true

  let is_dead = not << is_alive

  let (|Alive|Dead|) cast =
    if is_alive cast
    then Alive
    else Dead

  let inline is_active cast =
    if is_alive cast
    then
      false
    else
      let status = cast.property.status in
      let m = status.mentality in
      status.bind = 0 && m.mentality <> Mentality.Sleep || m.duration = 0

  let is_inactive = not << is_active

  let (|Active|Inactive|) cast =
    if is_active cast
    then Active
    else Inactive

  let (|Fine|Injured|HeavyInjured|Unconscious|) cast =
    match cast.property.life with
      { current = 0 } -> Unconscious
    | { max = max; current = current } when max = current -> Fine
    | { max = max; current = current } when max / current >= 5 -> HeavyInjured
    | _ -> Injured

  let inline private cast_state d =
    if d > 0
    then Some d
    else Option.None

  let (|Poison|_|) cast = cast_state cast.property.status.poison
  let (|Paralyze|_|) cast = cast_state cast.property.status.paralyze
  let (|Bind|_|) cast = cast_state cast.property.status.bind
  let (|Silence|_|) cast = cast_state cast.property.status.silence
  let (|FaceUp|_|) cast = cast_state cast.property.status.face_up
  let (|AntiMagic|_|) cast = cast_state cast.property.status.anti_magic

  let (|Normal|Sleep|Confuse|Overheat|Brave|Panic|) cast =
    let m = cast.property.status.mentality in
    match m.mentality, m.duration with
      Mentality.Normal, _ -> Normal
    | Mentality.Sleep, d -> Sleep d
    | Mentality.Confuse, d -> Confuse d
    | Mentality.Overheat, d -> Overheat d
    | Mentality.Brave, d -> Brave d
    | Mentality.Panic, d -> Panic d

  let (|UpAction|DownAction|NormalAction|) cast =
    match cast.property.enhance.action with
      a when a > 0 -> UpAction a
    | a when a < 0 -> DownAction a
    | _ -> NormalAction

  let (|UpAvoid|DownAvoid|NormalAvoid|) cast =
    match cast.property.enhance.avoid with
      a when a > 0 -> UpAvoid a
    | a when a < 0 -> DownAvoid a
    | _ -> NormalAvoid

  let (|UpResist|DownResist|NormalResist|) cast =
    match cast.property.enhance.resist with
      r when r > 0 -> UpResist r
    | r when r < 0 -> DownResist r
    | _ -> NormalResist

  let (|UpDefense|DownDefense|NormalDefense|) cast =
    match cast.property.enhance.defense with
      d when d > 0 -> UpDefense d
    | d when d < 0 -> DownDefense d
    | _ -> NormalDefense

  (* Card Ops *)
  let inline add_card max count card cards =
    let free_space = max - List.length cards in
    let add_count = if free_space < count then free_space else count in
    count - add_count, ListUtil.multi_cons add_count card cards

  let inline remove_card remove_count equals cards =
    let f =
      match remove_count with
        RemoveCount.All ->
          List.filter
      | RemoveCount.Count count ->
          ListUtil.filter_limit count in
    f equals cards
  
  (* Skills *)
  let inline add_skill count skill cast =
    let diff, skills =
      add_card
        (max_skill_item cast.property.level)
        count
        skill
        cast.skill in
    diff, { cast with skill = skills }
  let inline remove_skill remove_count skill cast =
    { cast with skill = remove_card
                          remove_count
                          (Skill.equals skill)
                          cast.skill }
  let inline count_skill skill cast =
    ListUtil.count_by
      (Skill.equals skill)
      cast.skill
  
  (* Items *)
  let inline add_item count item cast =
    let diff, items =
      add_card
        (max_skill_item cast.property.level)
        count
        item
        cast.item in
    diff, { cast with item = items }
  let inline remove_item remove_count item cast =
    { cast with item = remove_card
                         remove_count
                         (Item.equals item)
                         cast.item }
  let inline item_count item cast =
    ListUtil.count_by
      (Item.equals item)
      cast.item
   
  (* Beasts *)
  let inline add_beast count beast cast =
    let diff, beasts =
      add_card
        (max_beast cast.property.level)
        count
        beast
        cast.beast in
    diff, { cast with beast = beasts }
  let inline remove_beast remove_count beast cast =
    { cast with beast = remove_card
                          remove_count
                          (Beast.equals beast)
                          cast.beast }
  let inline beast_count beast cast =
    ListUtil.count_by
      (Beast.equals beast)
      cast.beast
        