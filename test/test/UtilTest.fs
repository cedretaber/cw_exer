namespace CardWirthEngineTest

#nowarn "62"

module UtilTest =
  open Expecto
  open CardWirthEngine.Util
  
  [<Tests>]
  let util_tests =
    testList "CardWirthEngine.Util" [
      testProperty "const'" <| fun input ->
        let value = "const value" in
        const' value input = value

      testList "is_true" [
        test "trueの場合" { Expect.isTrue (is_true true) "trueになること" }
        test "falseの場合" { Expect.isFalse (is_true false) "falseになること" }
      ]

      testList "is_false" [
        test "trueの場合" { Expect.isFalse (is_false true) "falseになること" }
        test "falseの場合" { Expect.isTrue (is_false false) "trueになること" }
      ]
    ]

module StringTest =
  open Expecto
  open CardWirthEngine

  [<Tests>]
  let sring_tests =
    testList "CardWirthEngine.String" [
      testList "starts_with" [
        testProperty "先頭の文字列" <| fun str ->
          if String.length str <> 0
          then Expect.isTrue (String.starts_with (str.[0..0]) str) "trueを返すこと"
      
        testProperty "連結された文字列" <| fun str1 str2 ->
          if String.length str1 <> 0 && String.length str2 <> 0
          then
            Expect.isTrue (String.starts_with str1 (str1 ^ str2)) "前半の文字列"
            if not <| str1.StartsWith str2
            then Expect.isFalse (String.starts_with str2 (str1 ^ str2)) "後半の文字列"
      ]
    ]