namespace CardWirthEngineTest

open NUnit.Framework

module TestUtils =
  let (===) a b = Assert.AreEqual (a, b)
  let (!==) a b = Assert.AreNotEqual (a, b)

  let assert' (e : bool) = Assert.IsTrue e
  let assert_not (e : bool) = Assert.IsFalse e