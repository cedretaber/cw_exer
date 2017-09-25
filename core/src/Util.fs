﻿namespace CardWirthEngine

module Util =

  let inline const' value = fun _ -> value

  let is_true (bool: bool) = bool
  let is_false bool = not bool

module String =
  let starts_with prefix (str : string) = str.StartsWith prefix