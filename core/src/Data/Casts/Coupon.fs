namespace CardWirthEngine.Data.Casts

module Coupon =
  type Name = string
  type Value = int
  type t =
    { name : Name
    ; value : Value
    }
