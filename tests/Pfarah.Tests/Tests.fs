module Pfarah.Tests

open Pfarah
open Pfarah.Operators
open Pfarah.ParaResult
open Utils
open NUnit.Framework
open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Diagnostics

let shouldEqual (x : 'a) (y : 'a) = Assert.AreEqual(x, y, sprintf "Expected: %A\nActual: %A" x y)

let parse str =
  match (ParaValue.Parse str) with
  | ParaValue.Record properties -> properties
  | _ -> failwith "Expected a record"

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
  |> shouldEqual [| ("foo", ParaValue.Number -2.314)|]

[<Test>]
let ``parse yes`` () =
  parse "foo=yes"
  |> shouldEqual [| ("foo", ParaValue.Bool true)|]

[<Test>]
let ``parse colon`` () =
  parse "foo=bar:qux"
  |> shouldEqual [| ("foo", ParaValue.String "bar:qux")|]

[<Test>]
let ``parse quoted key`` () =
  parse "\"foo\"=\"11\""
  |> shouldEqual [| ("foo", ParaValue.String "11")|]

[<Test>]
let ``parse double equal key`` () =
  parse "flags={bar=a ==b}"
  |> shouldEqual [| ("flags",
                      ParaValue.Record ([|("bar", ParaValue.String "a")
                                          ("=", ParaValue.String("b")) |]))|]

[<Test>]
let ``parse date`` () =
  parse "foo=1492.3.2"
  |> shouldEqual [| ("foo", ParaValue.Date (new DateTime(1492,3,2)))|]

[<Test>]
let ``parse variable`` () =
  parse "@foo = 2"
  |> shouldEqual [| ("@foo", ParaValue.Number 2.0 )|]

[<Test>]
let ``parse hsv`` () =
  parse "color = hsv { 0.5 0.2 0.8 }"
  |> shouldEqual [| ("color", ParaValue.Hsv(0.5, 0.2, 0.8) )|]

[<Test>]
let ``parse rgb`` () =
  parse "color = rgb { 100 150 200 }"
  |> shouldEqual [| ("color", ParaValue.Rgb(100uy, 150uy, 200uy) )|]

[<Test>]
let ``parse invalid date is none`` () =
  parse "foo=1.a.2"
  |> shouldEqual [| ("foo", ParaValue.String "1.a.2")|]

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
let ``parse list of multiple single`` () =
  parse "foo={1}"
  |> shouldEqual [| ("foo", ParaValue.Array
                      ([|ParaValue.Number 1.0|]))|]

[<Test>]
let ``parse list of multiple number`` () =
  parse "foo={1 0}"
  |> shouldEqual [| ("foo", ParaValue.Array
                      ([|ParaValue.Number 1.0; ParaValue.Number 0.0|]))|]

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
let ``parse list of one quoted date`` () =
  parse "foo={\"1821.2.3\"}"
  |> shouldEqual [| ("foo", ParaValue.Array
                      ([|ParaValue.Date (new DateTime(1821, 2, 3))|]))|]

[<Test>]
let ``parse list of two quoted`` () =
  parse "foo={\"bar\" \"biz baz\"}"
  |> shouldEqual [| ("foo", ParaValue.Array
                      ([|ParaValue.String "bar";
                         ParaValue.String "biz baz"|]))|]

[<Test>]
let ``parse comments in objects`` () =
  parse "# I'm a comment\r\nfoo=bar\r\n# I'm another comment"
  |> shouldEqual [| ("foo", ParaValue.String "bar")|]

[<Test>]
let ``parse comments in arrays`` () =
  parse "# I'm a comment\r\nfoo={1\r\n#comment\r\n2}\r\n# I'm another comment"
  |> shouldEqual [| ("foo", ParaValue.Array
                      ([|ParaValue.Number 1.0; ParaValue.Number 2.0|]))|]

[<Test>]
let ``parse the triple comment threat`` () =
  parse """
	id = 7
#############################
# 11 EASY ACHIEVEMENTS
#############################
"""
  |> shouldEqual [| ("id", ParaValue.Number 7.0)|]

[<Test>]
let ``ignore empty objects`` () =
  parse "foo={1} {} church=yes"
  |> shouldEqual [| ("foo", ParaValue.Array ([|ParaValue.Number 1.0|]));
                    ("church", ParaValue.Bool true)|]

[<Test>]
let ``ignore empty objects at the start of other objects`` () =
  let data = """
history={
  {
  }
  blah={foo=bar}
}"""

  parse data
  |> shouldEqual
    [| ("history", ParaValue.Record(
         [| ("blah", ParaValue.Record(
              [| ("foo", ParaValue.String "bar") |]
          ))|]
    ))|]

[<Test>]
let ``ignore empty objects at end of other objects`` () =
  let data = """
history={
  blah={foo=bar}
  {
  }
}"""

  parse data
  |> shouldEqual
    [| ("history", ParaValue.Record(
         [| ("blah", ParaValue.Record(
              [| ("foo", ParaValue.String "bar") |]
          ))|]
    ))|]

[<Test>]
let ``parse longer continuous nested arrays`` () =
  let data = """
  primary_mapmodes={
	{ 0 0 } { 1 1 } { 4 4 } { 3 3 } { 2 2 } { 5 5 } { } { } { } { }
}"""
  parse data
  |> shouldEqual
    [| ("primary_mapmodes", ParaValue.Array(
          [|
            ParaValue.Array([| ParaValue.Number 0.0; ParaValue.Number 0.0 |])
            ParaValue.Array([| ParaValue.Number 1.0; ParaValue.Number 1.0 |])
            ParaValue.Array([| ParaValue.Number 4.0; ParaValue.Number 4.0 |])
            ParaValue.Array([| ParaValue.Number 3.0; ParaValue.Number 3.0 |])
            ParaValue.Array([| ParaValue.Number 2.0; ParaValue.Number 2.0 |])
            ParaValue.Array([| ParaValue.Number 5.0; ParaValue.Number 5.0 |])
            ParaValue.Record([| |])
            ParaValue.Record([| |])
            ParaValue.Record([| |])
            ParaValue.Record([| |])
          |])
    )|]

[<Test>]
let ``parse continuous nested arrays`` () =
  let data = """
  primary_mapmodes={
	{ 0 } { 1 } { 4 } { 3 } { 2 } { 5 } { } { } { } { }
}"""
  parse data
  |> shouldEqual
    [| ("primary_mapmodes", ParaValue.Array(
          [|
            ParaValue.Array([| ParaValue.Number 0.0 |])
            ParaValue.Array([| ParaValue.Number 1.0 |])
            ParaValue.Array([| ParaValue.Number 4.0 |])
            ParaValue.Array([| ParaValue.Number 3.0 |])
            ParaValue.Array([| ParaValue.Number 2.0 |])
            ParaValue.Array([| ParaValue.Number 5.0 |])
            ParaValue.Record([| |])
            ParaValue.Record([| |])
            ParaValue.Record([| |])
            ParaValue.Record([| |])
          |])
    )|]

[<Test>]
let ``parse object with dynamic`` () =
  let obj = ParaValue.Parse "foo={bar=baz qux=zux}"
  let actual = obj?foo |> ParaResult.bind (fun x -> x?bar)
  actual |> shouldEqual (Ok (ParaValue.String "baz"))

[<Test>]
let ``parse number as int`` () =
  let obj = ParaValue.Parse "foo=-2"
  obj?foo >>= ParaValue.asInteger |> shouldEqual (Ok -2)

[<Test>]
let ``parse obj as string`` () =
  let obj = ParaValue.Parse "foo=\"bar baz\" qux=dis"
  obj?foo>>= ParaValue.asString |> shouldEqual (Ok "bar baz")
  obj?qux>>= ParaValue.asString |> shouldEqual (Ok "dis")

[<Test>]
let ``parse obj as bool`` () =
  let obj = ParaValue.Parse "foo=yes qux=no baz=1 bar=0"
  obj?foo>>= ParaValue.asBool |> shouldEqual (Ok true)
  obj?qux>>= ParaValue.asBool |> shouldEqual (Ok false)
  obj?baz>>= ParaValue.asBool |> shouldEqual (Ok true)
  obj?bar>>= ParaValue.asBool |> shouldEqual (Ok false)

[<Test>]
let ``parse obj as float`` () =
  let obj = ParaValue.Parse "foo=-2.234"
  obj?foo >>= ParaValue.asNumber |> shouldEqual (Ok -2.234)

[<Test>]
let ``parse invalid quoted dates`` () =
  let obj = ParaValue.Parse "foo=\"1.b.c\""
  obj?foo >>= ParaValue.asString |> shouldEqual (Ok "1.b.c")

[<Test>]
let ``parse obj as date`` () =
  let obj = ParaValue.Parse "foo=1492.1.2"
  obj?foo>>= ParaValue.asDate |> shouldEqual (Ok (DateTime(1492, 1, 2)))

[<Test>]
let ``parse obj as tough array`` () =
  let obj = ParaValue.Parse "foo={1 bar 2.000 {qux=baz}}"
  let actual = para {
    let! arr = obj?foo |> ParaResult.bind ParaValue.asArray
    let! (fst, snd, thrd, frth) =
      match arr with
      | [| a; b; c; d |] -> Ok (a, b, c, d)
      | x -> Error("unexpected number of elements")
    let! fst = ParaValue.asInteger fst
    let! snd = ParaValue.asString snd
    let! thrd = ParaValue.asNumber thrd
    let! frth = frth?qux |> ParaResult.bind ParaValue.asString
    return (fst, snd, thrd, frth)
  }
  actual |> shouldEqual (Ok (1, "bar", 2., "baz"))

[<Test>]
let ``parse obj as nested objects`` () =
  let obj = ParaValue.Parse "bar={{foo=qux}{foo=qix zoo=zob}}"
  para {
    let! arr = obj?bar
    let! fst = arr.[0]?foo |> ParaResult.bind ParaValue.asString
    let! snd = arr.[1]?foo |> ParaResult.bind ParaValue.asString
    let! thrd = arr.[1]?zoo |> ParaResult.bind ParaValue.asString
    return fst, snd, thrd
  } |> shouldEqual (Ok ("qux", "qix", "zob"))

[<Test>]
let ``parse obj as nested objects whitespace`` () =
  let obj = ParaValue.Parse "bar = { { foo = qux } { foo = qix zoo = zob } }"
  para {
    let! arr = obj?bar
    let! fst = arr.[0]?foo |> ParaResult.bind ParaValue.asString
    let! snd = arr.[1]?foo |> ParaResult.bind ParaValue.asString
    let! thrd = arr.[1]?zoo |> ParaResult.bind ParaValue.asString
    return fst, snd, thrd
  } |> shouldEqual (Ok ("qux", "qix", "zob"))

[<Test>]
let ``parse gameplay settings`` () =
  let data = """gameplaysettings=
{
	setgameplayoptions=
	{
1 1 2 1 1 0 0 1 0 1 0 	}
}"""
  let obj = ParaValue.Parse data
  let actual = para {
    let! settings = obj?gameplaysettings
    let! options = settings?setgameplayoptions
    return! ParaValue.flatMap ParaValue.asInteger options
  }
  actual |> shouldEqual (Ok [|1;1;2;1;1;0;0;1;0;1;0|])

[<Test>]
let ``ignore header`` () =
  let data = """EU4txt
date="1763.4.1"
player="RFR" """
  parse data |> shouldEqual [| ("date", ParaValue.Date (new DateTime(1763, 4, 1)));
                               ("player", ParaValue.String "RFR")|]


[<Test>]
let ``collect on a non-iterable returns an empty array`` () =
  collect "name" (ParaValue.String "hey") |> shouldEqual (ParaValue.Array [| |])

[<Test>]
let ``collect on a array returns sub-objects`` () =
  let data =
    ParaValue.Array [| ParaValue.Record [| ("name", ParaValue.String "bob")
                                           ("name", ParaValue.String "steve") |]
                       ParaValue.Record [| |]
                       ParaValue.Record [| ("name", ParaValue.String "wilson") |] |]

  let expected =
    [| "bob"; "steve"; "wilson" |] |> Array.map ParaValue.String |> ParaValue.Array
  collect "name" data |> shouldEqual expected

[<Test>]
let ``collect operator`` () =
  let data = ParaValue.Record [| ("name", ParaValue.String "steve") |]
  data / "name" |> shouldEqual (ParaValue.Array [| (ParaValue.String "steve") |])

[<Test>]
let ``nested collect operator`` () =
  let data =
    ParaValue.Record [|
      "people", ParaValue.Array [| ParaValue.Record [| ("name", ParaValue.String "steve") |] |] |]
  data / "people" / "name" |> shouldEqual (ParaValue.Array [| (ParaValue.String "steve") |])

[<Test>]
let ``collectAll on a non-iterable returns an empty array`` () =
  collectAll "name" (ParaValue.String "hey") |> shouldEqual (ParaValue.Array [| |])

[<Test>]
let ``collectAll on a array returns sub-objects`` () =
  let data =
    ParaValue.Array [| ParaValue.Record [| ("name", ParaValue.String "bob")
                                           ("name", ParaValue.String "steve") |]
                       ParaValue.Record [| |]
                       ParaValue.Record [| ("name", ParaValue.String "wilson") |] |]

  let expected =
    [| "bob"; "steve"; "wilson" |] |> Array.map ParaValue.String |> ParaValue.Array
  collectAll "name" data |> shouldEqual expected

[<Test>]
let ``collectAll operator`` () =
  let data = ParaValue.Record [| ("name", ParaValue.String "steve") |]
  data /./ "name" |> shouldEqual (ParaValue.Array [| (ParaValue.String "steve") |])

[<Test>]
let ``nested collectAll operator`` () =
  let data =
    ParaValue.Record [|
      "people", ParaValue.Array [| ParaValue.Record [| ("name", ParaValue.String "steve") |] |] |]
  data /./ "name" |> shouldEqual (ParaValue.Array [| (ParaValue.String "steve") |])

[<Test>]
let ``ParaValue asArray happy path`` () =
  let data = ParaValue.Array [| ParaValue.Bool true |]
  ParaValue.asArray data |> shouldEqual (Ok [| ParaValue.Bool true |])

[<Test>]
let ``ParaValue asArray sad path`` () =
  let data = ParaValue.Bool true
  ParaValue.asArray data |> shouldEqual (Error "Expected array but received true")

[<Test>]
let ``ParaValue asRecord happy path`` () =
  let data = ParaValue.Record [| "young", ParaValue.Bool true |]
  ParaValue.asRecord data |> shouldEqual (Ok [| "young", ParaValue.Bool true |])

[<Test>]
let ``ParaValue asRecord sad path`` () =
  let data = ParaValue.Bool true
  ParaValue.asRecord data |> shouldEqual (Error "Expected record but received true")

[<Test>]
let ``ParaValue asBool happy path`` () =
  let data = ParaValue.Bool true
  ParaValue.asBool data |> shouldEqual (Ok true)

[<Test>]
let ``ParaValue asBool is true for non-zero numbers`` () =
  let data = ParaValue.Number 1.0
  ParaValue.asBool data |> shouldEqual (Ok true)

[<Test>]
let ``ParaValue asBool is false for zero`` () =
  let data = ParaValue.Number 0.0
  ParaValue.asBool data |> shouldEqual (Ok false)

[<Test>]
let ``ParaValue asBool sad path`` () =
  let data = ParaValue.String "Ha"
  ParaValue.asBool data |> shouldEqual (Error "Expected boolean but received Ha")

[<Test>]
let ``ParaValue asNumber happy path`` () =
  let data = ParaValue.Number 1.0
  ParaValue.asNumber data |> shouldEqual (Ok 1.0)

[<Test>]
let ``ParaValue asNumber sad path`` () =
  let data = ParaValue.String "NaN"
  ParaValue.asNumber data |> shouldEqual (Error "Expected number but received NaN")

[<Test>]
let ``ParaValue asString happy path`` () =
  let data = ParaValue.String "NaN"
  ParaValue.asString data |> shouldEqual (Ok "NaN")

[<Test>]
let ``ParaValue asString sad path`` () =
  let data = ParaValue.Array [| ParaValue.String "NaN" |]
  ParaValue.asString data |> shouldEqual (Error "Expected string but received [\n  NaN\n]")

[<Test>]
let ``ParaValue asDate happy path`` () =
  let data = ParaValue.Date (DateTime(1, 1, 1))
  ParaValue.asDate data |> shouldEqual (Ok (DateTime(1, 1, 1)))

[<Test>]
let ``ParaValue asDate sad path`` () =
  let data = ParaValue.Bool true
  ParaValue.asDate data |> shouldEqual (Error "Expected date but received true")

[<Test>]
let ``ParaValue asHsv happy path`` () =
  let data = ParaValue.Hsv (1.0, 0.5, 0.1)
  ParaValue.asHsv data |> shouldEqual (Ok (1.0, 0.5, 0.1))

[<Test>]
let ``ParaValue asHsv sad path`` () =
  let data = ParaValue.Bool true
  ParaValue.asHsv data |> shouldEqual (Error "Expected hsv but received true")

[<Test>]
let ``ParaValue asRgb happy path`` () =
  let data = ParaValue.Rgb (100uy, 50uy, 20uy)
  ParaValue.asRgb data |> shouldEqual (Ok (100uy, 50uy, 20uy))

[<Test>]
let ``ParaValue asRgb sad path`` () =
  let data = ParaValue.Bool true
  ParaValue.asRgb data |> shouldEqual (Error "Expected rgb but received true")

[<Test>]
let ``ParaValue getAll should collect all properties with a given key`` () =
  let data = "core=AAA core=BBB core=CCC"
  ParaValue.Parse data
  |> ParaValue.getAll "core"
  |> shouldEqual ([| "AAA"; "BBB"; "CCC" |] |> Array.map ParaValue.String |> Ok)

[<Test>]
let ``mapping an objects results in mapping the values`` () =
  let data = ParaValue.Record [| "people", ParaValue.Number 2.0 |]
  let fn = function | ParaValue.Number x -> ParaValue.Number (x + 1.)
                    | x -> x
  ParaValue.map fn data |> shouldEqual (ParaValue.Record [| "people", ParaValue.Number 3.0 |])

[<Test>]
let ``mapping an array results in mapping the values`` () =
  let data = ParaValue.Array [| (ParaValue.Number 1.0); (ParaValue.Number 2.0) |]
  let fn = function | ParaValue.Number x -> ParaValue.Number (x + 1.)
                    | x -> x
  ParaValue.map fn data |> shouldEqual (ParaValue.Array [| (ParaValue.Number 2.0); (ParaValue.Number 3.0) |])

[<Test>]
let ``mapping a non-iterable results in mapping itself`` () =
  let data = ParaValue.String "bob"
  let fn = function | ParaValue.String x -> ParaValue.String (x + "by")
                    | x -> x
  ParaValue.map fn data |> shouldEqual (ParaValue.String "bobby")

[<Test>]
let ``parse obj be used in a seq`` () =
  let obj = ParaValue.Parse "ids = {1 2 3 4 5}"
  obj?ids |> (ParaResult.bind (ParaValue.flatMap ParaValue.asInteger)) 
          |> shouldEqual (Ok [| 1 .. 5 |])

let army = """
army=
{
    name="1st army"
    unit={
        name="1st unit"
    }
}
army=
{
    name="2nd army"
    unit={
        name="1st unit"
    }
    unit={
        name="2nd unit"
    }
}"""

[<Test>]
let ``parse army example`` () =
  let obj = parse army
  let expected =
    [| ("army",
        ParaValue.Record(
          [| ("name", ParaValue.String "1st army");
             ("unit", ParaValue.Record(
                [|("name", ParaValue.String "1st unit")|]))
          |]));
       ("army",
        ParaValue.Record(
          [| ("name", ParaValue.String "2nd army");
             ("unit", ParaValue.Record(
                [|("name", ParaValue.String "1st unit")|]));
             ("unit", ParaValue.Record(
                [|("name", ParaValue.String "2nd unit")|]))
          |]))
    |]
  obj |> shouldEqual expected

[<Test>]
let ``parse army and collect`` () =
  let armyData =
    ParaValue.Parse army / "army"
    |> ParaValue.flatMap (fun x -> para {
      let! unitnames = 
        x / "unit" |> ParaValue.flatMap (fun u -> u?name >>= ParaValue.asString)
      let! armyname = x?name |> ParaResult.bind ParaValue.asString
      return armyname, unitnames
    })

  ParaResult.map Array.length armyData |> shouldEqual (Ok 2)
  ParaResult.map Seq.head armyData |> shouldEqual (Ok ("1st army", [|"1st unit"|]))
  ParaResult.map Array.last armyData |> shouldEqual (Ok ("2nd army", [|"1st unit"; "2nd unit"|]))

[<Test>]
let ``tryFind patrol`` () =
  let ships = """
  ship={
      name="1st ship"
      patrol=yes
  }
  ship={
    name="2nd ship"
  }"""

  let parseShip o = para {
    let! name = o?name >>= ParaValue.asString
    let! patrol = ParaValue.tryGet "patrol" o
    let dd = Option.map ParaValue.asBool patrol
    let! c =
      match patrol with
      | Some(x) -> x |> ParaValue.asBool
      | None -> Ok(false)
    return name, c 
  }

  let shipData = para {
    let! shipss = ParaValue.Parse ships / "ship" |> ParaValue.flatMap parseShip
    return shipss |> Array.filter snd |> Array.map fst
  }
  // Let's print the name of ships on patrol

  shipData |> shouldEqual (Ok [| "1st ship" |])

[<Test>]
let ``tryFind returns Some ParaValue`` () =
  let data = ParaValue.Parse "foo=1.000"
  data |> tryFind "foo" |> shouldEqual (Some (ParaValue.Number 1.0))
  data |> tryFind "bar" |> shouldEqual None

[<Test>]
let ``findOptional works`` () =
  let data = [| ("hello", ParaValue.String "foo"); ("world", ParaValue.String "") |]
  let data2 = [| ("hello", ParaValue.String "foo"); |]
  let required, optional = findOptional [data; data2]
  CollectionAssert.AreEquivalent(["hello"], required)
  CollectionAssert.AreEquivalent(["world"], optional)

[<Test>]
let ``save data format example`` () =
  let data = """
foo=bar
baz=" hello  ##
      cheese"
start=1841.2.3
end="1300.10.1"
type=49
strength=10.435
colorHsv = hsv { 0.1 0.2 0.3 }
colorRgb = rgb { 100 200 150 }
nums={1 2 3 4}
core=YOU
core=MEE
army={
    unit={
        name="1st unit"
    }
    unit={
        name="1st unit"

        patrol=yes
    }
    { }
    attachments={
        {
            id=34
        }
        {
            id=55
        }
    }
}"""

  let parsed = ParaValue.Parse data
  use mem = new MemoryStream()
  ParaValue.Save(mem, parsed)
  mem.Flush()
  let saved = Encoding.GetEncoding(1252).GetString(mem.ToArray())
  let parsed2 = ParaValue.Parse saved
  parsed2 |> shouldEqual parsed

let (``try parse double cases``:obj[][]) = [|
  [| "1.000"; Some(1.0) |]
  [| "1.0"; Some(1.0) |]
  [| "-1.000"; Some(-1.0) |]
  [| "1.542"; Some(1.542) |]
  [| "15"; Some(15.0) |]
  [| "-15"; Some(-15.0) |]
  [| ""; None |]
  [| "1.0000"; None |]
  [| "1.a00"; None |]
  [| "1.0a0"; None |]
  [| "1.00a"; None |]
  [| "1e10"; None |]
  [| "1.1.1"; None |]
  [| "1.00000"; Some(1.0) |]
  [| "-1.00000"; Some(-1.0) |]
  [| "1.54206"; Some(1.54206) |]
  [| "1.a0000"; None |]
  [| "1.0a000"; None |]
  [| "1.00a00"; None |]
  [| "1.000a0"; None |]
  [| "1.0000a"; None |]
|]

[<Test>]
[<TestCaseSource("try parse double cases")>]
let ``try parse double`` str expected =
  tryDoubleParse str |> shouldEqual expected

let (``try parse date cases``:obj[][]) = [|
  [| "1.1.1"; Some(new DateTime(1, 1, 1)) |]
  [| "2015.8.1"; Some(new DateTime(2015, 8, 1)) |]
  [| "1942.5.2.4"; Some(new DateTime(1942, 5, 2, 4, 0, 0)) |]
  [| "1942.5.2.14"; Some(new DateTime(1942, 5, 2, 14, 0, 0)) |]
  [| "1942.5.2.24"; None |]
  [| "2015.8.32"; None |]
  [| "99999.8.1"; None |]
  [| "1942.0.1"; None |]
  [| "1942.13.1"; None |]
  [| "1942.1.0"; None |]
  [| "0.8.1"; None |]
  [| "50.50.50"; None |]
  [| "1.a.1"; None |]
  [| "1!1.1"; None |]
  [| "1.1"; None |]
|]

[<Test>]
[<TestCaseSource("try parse date cases")>]
let ``try parse date`` str expected =
  tryDateParse str |> shouldEqual expected

let (``ParaValue toString cases``:obj[][]) = [|
  [| ParaValue.String "a"; "a" |]
  [| ParaValue.Number 1.500; "1.500" |]
  [| ParaValue.Date (DateTime(1441, 10, 5)); "1441.10.5" |]
  [| ParaValue.Hsv (0.1, 0.2, 0.3); "(0.100, 0.200, 0.300)" |]
  [| ParaValue.Rgb (100uy, 150uy, 200uy); "(100, 150, 200)" |]
  [| ParaValue.Bool true; "true" |]
  [| ParaValue.Bool false; "false" |]
  [| ParaValue.Array([||]); "[]" |]
  [| ParaValue.Array([|ParaValue.String "a"|]); "[\n  a\n]" |]
  [| ParaValue.Array([|ParaValue.String "a"
                       ParaValue.String "b"|]); "[\n  a,\n  b\n]" |]
  [| ParaValue.Record([||]); "{}" |]
  [| ParaValue.Record(
      [| ("a", ParaValue.String "b") |]); "{\n  a: b\n}" |]
  [| ParaValue.Record(
      [| ("a", ParaValue.String "b")
         ("b", ParaValue.String "c") |]); "{\n  a: b,\n  b: c\n}" |]
  [| ParaValue.Record(
      [| ("a", ParaValue.Record [| ("b", ParaValue.String "c") |])
        |]); "{\n  a: {\n    b: c\n  }\n}" |]
|]

[<Test>]
[<TestCaseSource("ParaValue toString cases")>]
let ``ParaValue toString`` x expected =
  (x.ToString()) |> shouldEqual expected

[<Test>]
let ``arrays can be iterated`` () =
  let arr = ParaValue.Array([| ParaValue.Number 1.0; ParaValue.Number 2.0 |])
  for x in arr do
    match x with
    | ParaValue.Number(x) when x = 1.0 || x = 2.0 -> ()
    | _ -> Assert.Fail("Unexpected number")

[<Test>]
let ``load with header textual`` () =
  let data = "EU4txt\rbar=foo\r"
  let strm = new MemoryStream(Encoding.GetEncoding(1252).GetBytes(data))
  ParaValue.Load(strm, "EU4bin", "EU4txt", (lazy dict([])))
  |> shouldEqual (ParaValue.Record([| ("bar", ParaValue.String "foo") |]))

[<Test>]
let ``load with mismatched headers`` () =
  let data = "EU4txt\rbar=foo\r"
  let strm = new MemoryStream(Encoding.GetEncoding(1252).GetBytes(data))
  let ex = Assert.Throws(fun () ->
    ParaValue.Load(strm, "EU4bi", "EU4txt", (lazy dict([]))) |> ignore)
  ex.Message |> shouldEqual "Headers should be the same length"

[<Test>]
let ``load with unknown header`` () =
  let data = "hip"
  let strm = new MemoryStream(Encoding.GetEncoding(1252).GetBytes(data))
  let ex = Assert.Throws(fun () ->
    ParaValue.Load(strm, "EU4bin", "EU4txt", (lazy dict([]))) |> ignore)
  ex.Message |> shouldEqual "Unexpected header: hip"

[<Test>]
let ``load plain text file`` () =
  let path = Path.Combine(TestContext.CurrentContext.TestDirectory, "data", "eu4txt.eu4")
  ParaValue.Load(path, "EU4bin", "EU4txt", lazy(dict([])))
  |> shouldEqual (ParaValue.Record([| ("date", ParaValue.Date (DateTime(1821, 1, 1)))|]))

[<Test>]
let ``load zip text file`` () =
  let path = Path.Combine(TestContext.CurrentContext.TestDirectory, "data", "eu4txt-zip.eu4")
  ParaValue.Load(path, "EU4bin", "EU4txt", lazy(dict([])))
  |> shouldEqual (ParaValue.Record([| ("date", ParaValue.Date (DateTime(1821, 1, 1)))|]))

[<Test>]
let ``deserialize true boolean`` () =
  deserialize (ParaValue.Bool true) |> shouldEqual true

[<Test>]
let ``deserialize false boolean`` () =
  deserialize (ParaValue.Bool false) |> shouldEqual false

[<Test>]
let ``deserialize zero number as false boolean`` () =
  deserialize (ParaValue.Number 0.0) |> shouldEqual false

[<Test>]
let ``deserialize non-zero number as true boolean`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual true

[<Test>]
let ``deserialize number as a signed 32-bit integer`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1

[<Test>]
let ``deserialize a number as a signed 8-bit integer`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1y

[<Test>]
let ``deserialize a number as a unsigned 8-bit natural number`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1uy

[<Test>]
let ``deserialize a number as a signed 16-bit integer`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1s

[<Test>]
let ``deserialize a number as a unsigned 16-bit natural number`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1us

[<Test>]
let ``deserialize a number as a unsigned 32-bit natural number`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1u

[<Test>]
let ``deserialize a number as a signed 64-bit integer`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1L

[<Test>]
let ``deserialize a number as a unsigned 64-bit natural number`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1UL

[<Test>]
let ``deserialize a number as a 32-bit floating point number`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1.0f

[<Test>]
let ``deserialize a number as a 64-bit floating point number`` () =
  deserialize (ParaValue.Number 1.0) |> shouldEqual 1.0

[<Test>]
let ``deserialize a string`` () =
  deserialize (ParaValue.String "hello") |> shouldEqual "hello"

[<Test>]
let ``deserialize a string into an option`` () =
  deserialize (ParaValue.String "hello") |> shouldEqual (Some "hello")

[<Test>]
let ``deserialize a single instance array into string`` () =
  deserialize (ParaValue.Array [| ParaValue.String "bob" |])
  |> shouldEqual "bob"

[<Test>]
let ``deserialize a single instance record into string`` () =
  deserialize (ParaValue.Record [| "name", ParaValue.String "bob" |])
  |> shouldEqual "bob"

[<Test>]
let ``deserialize an array into an array`` () =
  deserialize (ParaValue.Array [| ParaValue.String "a"; ParaValue.String "b" |])
  |> shouldEqual ([| "a"; "b"; |])

[<Test>]
let ``fail deserialize an string into an array`` () =
  let data = ParaValue.String "hello"
  let expected = ParaResult<int[]>.Error<int[]> "Expected array but received hello"
  fromPara data |> shouldEqual expected

[<Test>]
let ``fail deserialize a string array into an int array`` () =
  let data = ParaValue.Array [| ParaValue.String "hello" |]
  let expected : ParaResult<int[]> = Error "Expected number but received hello"
  fromPara data |> shouldEqual expected

[<Test>]
let ``deserialize an array into list`` () =
  deserialize (ParaValue.Array [| ParaValue.String "a"; ParaValue.String "b" |])
  |> shouldEqual ([ "a"; "b"; ])

type Cheese = {
  Label: string
  Age: int
} with
  static member inline Create label age = { Cheese.Label = label; Age = age }
  static member inline FromPara (_:Cheese) =
    Cheese.Create <!> (!. "label") <*> (!. "age")

[<Test>]
let ``deserialize a record`` () =
  let data =
    ParaValue.Record [| ("label", ParaValue.String "american")
                        ("age", ParaValue.Number 1.0) |]
  deserialize data |> shouldEqual { Cheese.Label = "american"; Age = 1 }

[<Test>]
let ``fail deserialization without property`` () =
  let data =
    ParaValue.Record [| ("label", ParaValue.String "american") |]
  let expected : ParaResult<Cheese> = Error "Found not 1 but 0 of age"
  fromPara data |> shouldEqual expected

[<Test>]
let ``fail deserialization because not a record`` () =
  let data = ParaValue.String "hello"
  let expected : ParaResult<Cheese> = Error "Expected record but received hello"
  fromPara data |> shouldEqual expected

[<Test>]
let ``fail deserialization because mismatching types`` () =
  let data =
    ParaValue.Record [| ("label", ParaValue.String "american")
                        ("age", ParaValue.Array [| |]) |]
  let expected : ParaResult<Cheese> = Error "Expected number but received []"
  fromPara data |> shouldEqual expected

[<Test>]
let ``simple para builder`` () =
  let value = ParaValue.String "bob"
  let b = para {
    return! ParaValue.asString value
  }

  b |> shouldEqual (ParaResult<String>.Ok "bob")

[<Test>]
let ``pget should work with the para builder`` () =
  let value = ParaValue.Record [| "name", ParaValue.String "bob" |]
  let b = para {
    return! pget "name" value
  }
  
  b |> shouldEqual (ParaResult<String>.Ok "bob")

[<Test>]
let ``pget operator should work with the para builder`` () =
  let value = ParaValue.Record [| "name", ParaValue.String "bob" |]
  let b = para {
    return! value .@ "name"
  }
  
  b |> shouldEqual (ParaResult<String>.Ok "bob")

[<Test>]
let ``para builder should work with army data`` () =
  let obj = ParaValue.Parse army

  let unitDes o : ParaResult<string> = para {
    return! o .@ "name"
  }

  let armyDes o : ParaResult<string * string list> = para {
    let! armyName = o .@ "name"
    let! units = ParaValue.flatMap unitDes (o / "unit")
    return armyName, List.ofArray units
  }

  let actual = para {
    let! armies = ParaValue.flatMap armyDes (obj / "army")
    return List.ofArray armies
  }

  let expected = [ ("1st army", ["1st unit"])
                   ("2nd army", ["1st unit"; "2nd unit"]) ]

  actual |> shouldEqual (ParaResult.Ok(expected))

[<Test>]
let ``flatMap works with objects`` () =
  let data = ParaValue.Parse "a=1 b=2 c=3 d=4"
  let actual = ParaValue.flatMap (function | ParaValue.Number x -> Ok(x + 1.) 
                                           | _ -> Error "Unexpected") data

  actual |> shouldEqual (ParaResult.Ok([| 2.0; 3.0; 4.0; 5.0; |]))

[<Test>]
let ``tryPget should work with optionals`` () =
 let value = ParaValue.Record [| "name", ParaValue.String "bob" |]
 let b : ParaResult<int option> = para {
   return! tryPget "age" value
 }

 b |> shouldEqual (Ok None)

[<Test>]
let ``pgetAll should work with same keys`` () =
 let value = ParaValue.Parse "core=AAA core=BBB core=CCC"
 let actual : ParaResult<string[]> = pgetAll "core" value
 actual |> shouldEqual (Ok [| "AAA"; "BBB"; "CCC" |])