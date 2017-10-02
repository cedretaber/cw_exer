module CardWirthEngineTest.Utils.StringTest

#nowarn "62"

open Expecto
open CardWirthEngine.Utils

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
          Expect.isTrue (String.starts_with str1 (str1 ^ str2)) "前半の文字列から始まっていること"
    ]
  ]