module Pfarah.StreamingTests

open Pfarah
open Utils
open NUnit.Framework
open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Diagnostics

let shouldEqual (x : 'a) (y : 'a) = Assert.AreEqual(x, y, sprintf "Expected: %A\nActual: %A" x y)

let (``try parse streaming string cases``:obj[][]) = [|
  [| "a=b"; "a"; "b" |]
  [| "a=b\nc=d"; "c"; "d" |]
  [| "a=\"b\"\nc=d"; "c"; "d" |]
  [| "a=\"b  \t  \r  \n ' \"\nc=d"; "c"; "d" |]
  [| "a=yes\nc=d"; "c"; "d" |]
  [| "a=no\nc=d"; "c"; "d" |]
  [| "a=1.000\nc=d"; "c"; "d" |]
  [| "a=-1.000\nc=d"; "c"; "d" |]
  [| "a=1492.1.1\nc=d"; "c"; "d" |]
  [| "a={}\nc=d"; "c"; "d" |]
  [| "a={{}}\nc=d"; "c"; "d" |]
  [| "a={c=a b={}}\nc=d"; "c"; "d" |]
  [| "a={b={} c=a}\nc=d"; "c"; "d" |]
|]

[<Test>]
[<TestCaseSource("try parse streaming string cases")>]
let ``try parse streaming string`` (str:string) property expected =
  let db = Encoding.GetEncoding(1252).GetBytes(str)
  let strm = ParaValue.Stream(new MemoryStream(db))
  let actual = strm.op_Dynamic  property |> asString
  actual |> shouldEqual expected