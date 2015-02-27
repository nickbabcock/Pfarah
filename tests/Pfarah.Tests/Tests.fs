module Pfarah.Tests

open Pfarah
open NUnit.Framework

[<Test>]
let ``a space is whitespace`` () =
  Assert.AreEqual(Pfarah.isspace (int ' '), true)
