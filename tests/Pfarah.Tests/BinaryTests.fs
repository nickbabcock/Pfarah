module Pfarah.BinaryTests

open Pfarah
open System
open System.IO
open NUnit.Framework

let shouldEqual (x : 'a) (y : 'a) = Assert.AreEqual(x, y, sprintf "Expected: %A\nActual: %A" x y)

let parse str lookup header = 
  match (ParaValue.LoadBinary(str, lookup, header)) with
  | ParaValue.Record properties -> properties
  | _ -> failwith "Expected a record"

let strm (arr:int[]) = new MemoryStream([| for i in arr -> byte(i) |])

[<Test>]
let ``binary parse basic date`` () =
  let lookup = dict([(0x284ds, "date")])
  let stream = strm([|0x4d; 0x28; 0x01; 0x00; 0x0c; 0x00; 0x10; 0x77; 0x5d; 0x03|])
  parse stream lookup None
  |> shouldEqual [| ("date", ParaValue.Date(DateTime(1444, 11, 11))) |]

[<Test>]
let ``binary parse basic string`` () =
  let lookup = dict([(0x2a38s, "player")])
  let stream = strm([|0x38; 0x2a; 0x01; 0x00; 0x0f; 0x00; 0x03; 0x00; 0x45; 0x4e; 0x47|])
  parse stream lookup None
  |> shouldEqual [| ("player", ParaValue.String "ENG") |]


[<Test>]
let ``binary parse basic date with header`` () =
  let lookup = dict([(0x284ds, "date")])
  let stream = strm([|0x45; 0x55; 0x34; 0x62; 0x69; 0x6e; 0x4d; 0x28;
                      0x01; 0x00; 0x0c; 0x00; 0x10; 0x77; 0x5d; 0x03|])
  parse stream lookup (Some("EU4bin"))
  |> shouldEqual [| ("date", ParaValue.Date(DateTime(1444, 11, 11))) |]