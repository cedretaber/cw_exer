namespace CardWirthEngine.Cards

open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Casts

module Cast =
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
    type Enhance =
      { avoid : int
      ; resist : int
      ; defense : int
      }

    type t =
      { physical : Physical
      ; mental : Mental
      ; enhance : Enhance
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
    { id: int
    ; name : string
    ; image : Path
    ; description : string
    ; level : int
    ; life : Life
    ; feature : Feature.t
    ; ability : Ability.t
    ; status : Status.t
    ; enhance : Enhance
    ; coupons : Coupon.t list
    }

  type t =
    { property : Property
    ; skills : Skill.t list
    ; items : Item.t list
    ; beasts : Beast.t list
    }

  let inline is_active cast =
    let prop = cast.property in
    match prop with
      (* 体力が0 *)
      { life = { current = life } } when life <= 0 -> false
      (* 麻痺または石化 *)
    | { status = { paralyze = paralyze } } when paralyze > 0 -> false
    | _ -> true

  let inline has_coupon cast (coupon: Coupon.t) =
    cast.property.coupons
    |> List.exists
      (function { name = name } -> name = coupon.name)