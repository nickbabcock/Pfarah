module Pfarah.Tests

open Pfarah
open NUnit.Framework

[<Test>]
let ``a space is whitespace`` () =
  Assert.AreEqual(Pfarah.isspace (int ' '), true)

[<Test>]
let ``a newline is whitespace`` () =
  Assert.AreEqual(Pfarah.isspace (int '\n'), true)

[<Test>]
let ``a carriage return is whitespace`` () =
  Assert.AreEqual(Pfarah.isspace (int '\r'), true)

[<Test>]
let ``a tab is whitespace`` () =
  Assert.AreEqual(Pfarah.isspace (int '\t'), true)
