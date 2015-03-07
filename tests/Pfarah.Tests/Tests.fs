module Pfarah.Tests

open Pfarah
open NUnit.Framework
open System
open System.IO
open System.Text
open System.Collections.Generic

let string2stream (str:string) =
  new MemoryStream(Encoding.GetEncoding(1252).GetBytes(str));

let shouldEqual (x : 'a) (y : 'a) = Assert.AreEqual(x, y, sprintf "Expected: %A\nActual: %A" x y)

let parse str = Pfarah.parse (string2stream str)

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

[<Test>]
let ``parse basic object`` () =
  parse "foo=bar"
  |> shouldEqual [| ("foo", ParaValue.String "bar")|]

[<Test>]
let ``parse quoted string`` () =
  parse "foo=\"bar\""
  |> shouldEqual [| ("foo", ParaValue.String "bar")|]

[<Test>]
let ``parse number`` () =
  parse "foo=-2.314"
  |> shouldEqual [| ("foo", ParaValue.Number 2.314)|]

[<Test>]
let ``parse yes`` () =
  parse "foo=yes"
  |> shouldEqual [| ("foo", ParaValue.Bool true)|]

[<Test>]
let ``parse no`` () =
  parse "foo=no"
  |> shouldEqual [| ("foo", ParaValue.Bool false)|]

[<Test>]
let ``parse list of one`` () =
  parse "foo={bar}"
  |> shouldEqual [| ("foo", ParaValue.Array ([|ParaValue.String "bar"|]))|]

[<Test>]
let ``parse list of multiple`` () =
  parse "foo={bar baz}"
  |> shouldEqual [| ("foo", ParaValue.Array 
                      ([|ParaValue.String "bar"; ParaValue.String "baz"|]))|]

[<Test>]
let ``parse object of one`` () =
  parse "foo={bar=baz}"
  |> shouldEqual [| ("foo", ParaValue.Record ([|("bar", ParaValue.String "baz")|]))|]

[<Test>]
let ``parse object of none`` () =
  parse "foo={}"
  |> shouldEqual [| ("foo", ParaValue.Record ([||]))|]

[<Test>]
let ``parse object of two`` () =
  parse "foo={bar=baz qux=zux}"
  |> shouldEqual [| ("foo", ParaValue.Record 
                      ([|("bar", ParaValue.String "baz");
                         ("qux", ParaValue.String "zux")|]))|]

[<Test>]
let ``parse list of one spacing`` () =
  parse " foo = { bar } "
  |> shouldEqual [| ("foo", ParaValue.Array ([|ParaValue.String "bar"|]))|]

[<Test>]
let ``parse list of one quoted`` () =
  parse "foo={\"bar\"}"
  |> shouldEqual [| ("foo", ParaValue.Array ([|ParaValue.String "bar"|]))|]

[<Test>]
let ``parse list of two quoted`` () =
  parse "foo={\"bar\" \"biz baz\"}"
  |> shouldEqual [| ("foo", ParaValue.Array 
                      ([|ParaValue.String "bar";
                         ParaValue.String "biz baz"|]))|]