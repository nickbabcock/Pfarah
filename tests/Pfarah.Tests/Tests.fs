module Pfarah.Tests

open Pfarah
open NUnit.Framework
open System

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

[<Test>]
let ``parse the earliest date`` () =
  match Pfarah.tryDate "1.1.1" with
  | Some(date) -> Assert.AreEqual(date, new DateTime(1,1,1))
  | None -> Assert.Fail "Expected a date for 1.1.1"

[<Test>]
let ``parse earliest date with hour`` () =
  match Pfarah.tryDate "1.1.1.1" with
  | Some(date) -> Assert.AreEqual(date, new DateTime(1,1,1,1,0,0))
  | None -> Assert.Fail "Expected a date for 1.1.1.1"

[<Test>]
let ``parse the date`` () =
  match Pfarah.tryDate "1492.3.2" with
  | Some(date) -> Assert.AreEqual(date, new DateTime(1492,3,2))
  | None -> Assert.Fail "Expected a date for 1492.3.2"

[<Test>]
let ``parse the hour date`` () =
  match Pfarah.tryDate "1492.3.2.5" with
  | Some(date) -> Assert.AreEqual(date, new DateTime(1492,3,2,5,0,0))
  | None -> Assert.Fail "Expected a date for 1492.3.2.5"