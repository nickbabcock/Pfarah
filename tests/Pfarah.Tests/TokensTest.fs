module Pfarah.TokensTest

open Pfarah
open System
open System.IO
open System.Text
open NUnit.Framework
open Utils
open Tokens

let shouldEqual (x : 'a) (y : 'a) = CollectionAssert.AreEquivalent(x, y, sprintf "Expected: %A\nActual: %A" x y)

let str (x:string) =
  let encoding = Encoding.GetEncoding(1252)
  let length = BitConverter.GetBytes(uint16 (encoding.GetByteCount(x)))
  let data = encoding.GetBytes(x)
  Array.concat([BitConverter.GetBytes(0x000fs); length; data])

let lo (x:float) = 
  let data = BitConverter.GetBytes(int x * 1000)
  Array.concat([BitConverter.GetBytes(0x000ds); data])

let obje arr =
  Array.concat([BitConverter.GetBytes(0x0003s)
                arr |> Array.collect id
                BitConverter.GetBytes(0x0004s)])

let eq value key = Array.concat([key; [| 0x01uy; 0x00uy; |]; value])
let toko = dict([("node", 0x015Es); ("day", 0x007Cs); ("id", 0x000Bs)])
let token x = BitConverter.GetBytes(toko.[x])

let strm (arr:byte[]) = new MemoryStream(arr)
let loadBin data = ParaValue.LoadBinary(strm data, dict([]), None)

[<Test>]
let ``tokens single key`` () =
  let txtdata = "node=bar"
  let bindata = token "node" |> eq (str "bar")
  let binary = loadBin bindata
  let text = ParaValue.Parse txtdata
  let found = Tokens.deduce text binary
  found |> shouldEqual (dict([("node", "350")]))

[<Test>]
let ``tokens multiple identical keys`` () =
  let txtdata = "node=bar\nnode=foo"
  let bindata =
    [| token "node" |> eq (str "bar")
       token "node" |> eq (str "foo") |] |> Array.collect id
  let binary = loadBin bindata
  let text = ParaValue.Parse txtdata
  let found = Tokens.deduce text binary
  found |> shouldEqual (dict([("node", "350")]))

[<Test>]
let ``tokens multiple keys`` () =
  let txtdata = "node=bar\nnode=foo\nday=1.000"
  let bindata =
    [| token "node" |> eq (str "bar")
       token "node" |> eq (str "foo")
       token "day" |> eq (lo 1.0) |] |> Array.collect id
  let binary = loadBin bindata
  let text = ParaValue.Parse txtdata
  let found = Tokens.deduce text binary
  found |> shouldEqual (dict([("node", "350"); ("day", "124")]))

[<Test>]
let ``tokens single object`` () =
  let txtdata = "node={day=1.000}"
  let bindata =
    token "node" |> eq (obje [|token "day" |> eq (lo 1.0) |])
  let binary = loadBin bindata
  let text = ParaValue.Parse txtdata
  let found = Tokens.deduce text binary
  found |> shouldEqual (dict([("day", "124"); ("node", "350")]))

[<Test>]
let ``tokens double nested objects`` () =
  let txtdata = "node={id={day=1.000}}"
  let bindata =
    token "node" |> eq (obje
      [| token "id" |> eq (obje
          [| (token "day" |> eq (lo 1.0)) |]) |])
  let binary = loadBin bindata
  let text = ParaValue.Parse txtdata
  let found = Tokens.deduce text binary
  found |> shouldEqual (dict([("day", "124"); ("node", "350"); ("id", "11")]))