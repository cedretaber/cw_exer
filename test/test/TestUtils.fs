namespace CardWirthEngineTest

open NUnit.Framework

open System.Collections.Generic

module TestUtils =
  let (===) a b = Assert.AreEqual (a, b)
  let (!==) a b = Assert.AreNotEqual (a, b)

  let assert' (e : bool) = Assert.IsTrue e
  let assert_not (e : bool) = Assert.IsFalse e

  let assert_contains (e : 'a) (collection : 'a ICollection) =
    Assert.Contains (e, Array.ofSeq collection)