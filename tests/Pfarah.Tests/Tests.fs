module Pfarah.Tests

open Pfarah
open Utils
open NUnit.Framework
open System
open System.IO
open System.Text
open System.Collections.Generic

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
let ``parse date`` () =
  parse "foo=1492.3.2"
  |> shouldEqual [| ("foo", ParaValue.Date (new DateTime(1492,3,2)))|]

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
let ``ignore empty objects`` () =
  parse "foo={1} {} church=yes"
  |> shouldEqual [| ("foo", ParaValue.Array ([|ParaValue.Number 1.0|]));
                    ("church", ParaValue.Bool true)|]

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
let ``parse object with dynamic`` () =
  let obj = ParaValue.Parse "foo={bar=baz qux=zux}"
  obj?foo?bar |> shouldEqual (ParaValue.String "baz")

[<Test>]
let ``parse number as int`` () =
  let obj = ParaValue.Parse "foo=-2"
  obj?foo|> asInteger |> shouldEqual -2

[<Test>]
let ``parse obj as string`` () =
  let obj = ParaValue.Parse "foo=\"bar baz\" qux=dis"
  obj?foo|> asString |> shouldEqual "bar baz"
  obj?qux|> asString |> shouldEqual "dis"

[<Test>]
let ``parse obj as bool`` () =
  let obj = ParaValue.Parse "foo=yes qux=no baz=1 bar=0"
  obj?foo|> asBool |> shouldEqual true
  obj?qux|> asBool |> shouldEqual false
  obj?baz|> asBool |> shouldEqual true
  obj?bar|> asBool |> shouldEqual false

[<Test>]
let ``parse obj as float`` () =
  let obj = ParaValue.Parse "foo=-2.234"
  obj?foo |> asFloat |> shouldEqual -2.234

[<Test>]
let ``parse invalid quoted dates`` () =
  let obj = ParaValue.Parse "foo=\"1.b.c\""
  obj?foo |> asString |> shouldEqual "1.b.c"

[<Test>]
let ``parse obj as date`` () =
  let obj = ParaValue.Parse "foo=1492.1.2"
  obj?foo|> asDate |> shouldEqual (new DateTime(1492, 1, 2))

[<Test>]
let ``parse obj as tough array`` () =
  let obj = ParaValue.Parse "foo={1 bar 2.000 {qux=baz}}"
  obj?foo.[0]|> asInteger |> shouldEqual 1
  obj?foo.[1]|> asString |> shouldEqual "bar"
  obj?foo.[2]|> asFloat |> shouldEqual 2.0
  obj?foo.[3]?qux|> asString |> shouldEqual "baz"

[<Test>]
let ``parse obj as nested objects`` () =
  let obj = ParaValue.Parse "bar={{foo=qux}{foo=qix zoo=zob}}"
  obj?bar.[0]?foo |> asString |> shouldEqual "qux"
  obj?bar.[1]?foo |> asString |> shouldEqual "qix"
  obj?bar.[1]?zoo |> asString |> shouldEqual "zob"

[<Test>]
let ``parse gameplay settings`` () =
  let data = """gameplaysettings=
{
	setgameplayoptions=
	{
1 1 2 1 1 0 0 1 0 1 0 	}
}"""
  let obj = ParaValue.Parse data
  let actual =
    obj?gameplaysettings?setgameplayoptions |> asArray |> Array.map asInteger
  actual |> shouldEqual [|1;1;2;1;1;0;0;1;0;1;0|]

[<Test>]
let ``ignore header`` () =
  let data = """EU4txt
date="1763.4.1"
player="RFR" """
  parse data |> shouldEqual [| ("date", ParaValue.Date (new DateTime(1763, 4, 1)));
                               ("player", ParaValue.String "RFR")|]

[<Test>]
let ``parse obj be used in a seq`` () =
  let obj = ParaValue.Parse "ids = {1 2 3 4 5}"
  let nums = obj?ids |> asArray |> Array.map asInteger
  nums |> shouldEqual [| 1 .. 5 |]

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
    ParaValue.Parse army
    |> collect "army"
    |> Array.map (fun x ->
      let units = x |> collect "unit" |> Array.map (fun u -> u?name |> asString)
      x?name |> asString, units)

  Array.length armyData |> shouldEqual 2
  armyData.[0] |> fst |> shouldEqual "1st army"
  armyData.[0] |> snd |> shouldEqual [|"1st unit"|]
  armyData.[1] |> fst |> shouldEqual "2nd army"
  armyData.[1] |> snd |> shouldEqual [|"1st unit"; "2nd unit"|]

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

  // Let's print the name of ships on patrol
  let shipData =
    ParaValue.Parse ships
    |> collect "ship"
    |> Array.filter (tryFind "patrol" >> Option.isSome)
    |> Array.map (fun x -> x?name |> asString)

  shipData |> shouldEqual [| "1st ship" |]

[<Test>]
let ``tryFind returns Some ParaValue`` () =
  let data = ParaValue.Parse "foo=1.000"
  data |> tryFind "foo" |> shouldEqual (Some (ParaValue.Number 1.0))
  data |> tryFind "bar" |> shouldEqual None

[<Test>]
let ``findOptional works`` () =
  let data =
    ParaValue.Record([| ("hello", ParaValue.String "foo");
                        ("world", ParaValue.String "") |])

  let data2 =
      ParaValue.Record([| ("hello", ParaValue.String "foo"); |])

  let actual = findOptional [data; data2]
  let expected = [("hello", false); ("world", true)]
  CollectionAssert.AreEquivalent(expected, actual)

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
  str |> tryDoubleParse |> shouldEqual expected

let (``try parse date cases``:obj[][]) = [|
  [| "1.1.1"; Some(new DateTime(1, 1, 1)) |]
  [| "1942.5.2.4"; Some(new DateTime(1942, 5, 2, 4, 0, 0)) |]
  [| "1.a.1"; None |]
  [| "1!1.1"; None |]
  [| "1.1"; None |]
|]

[<Test>]
[<TestCaseSource("try parse date cases")>]
let ``try parse date`` str expected =
  str |> tryDateParse |> shouldEqual expected

let (``ParaValue toString cases``:obj[][]) = [|
  [| ParaValue.String "a"; "a" |]
  [| ParaValue.Number 1.500; "1.500" |]
  [| ParaValue.Date (DateTime(1441, 10, 5)); "1441.10.5" |]
  [| ParaValue.Bool true; "true" |]
  [| ParaValue.Bool false; "false" |]
  [| ParaValue.Array([||]); "[]" |]
  [| ParaValue.Array([|ParaValue.String "a"|]); "[a]" |]
  [| ParaValue.Array([|ParaValue.String "a"
                       ParaValue.String "b"|]); "[a, b]" |]
  [| ParaValue.Record([||]); "[]" |]
  [| ParaValue.Record(
      [| ("a", ParaValue.String "b") |]); "[(a, b)]" |]
  [| ParaValue.Record(
      [| ("a", ParaValue.String "b")
         ("b", ParaValue.String "c") |]); "[(a, b), (b, c)]" |]
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
let ``integer default tests`` () =
  Some(ParaValue.Number 1.0) |> integerDefault |> shouldEqual 1
  None |> integerDefault |> shouldEqual 0

[<Test>]
let ``string default tests`` () =
  Some(ParaValue.String "ENG") |> stringDefault |> shouldEqual "ENG"
  None |> stringDefault |> shouldEqual ""

[<Test>]
let ``float default tests`` () =
  Some(ParaValue.Number 1.2) |> floatDefault |> shouldEqual 1.2
  None |> floatDefault |> shouldEqual 0.0

[<Test>]
let ``bool default tests`` () =
  Some(ParaValue.Bool true) |> boolDefault |> shouldEqual true
  None |> boolDefault |> shouldEqual false

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
  let path = Path.Combine("data", "eu4txt.eu4")
  ParaValue.Load(path, "EU4bin", "EU4txt", lazy(dict([])))
  |> shouldEqual (ParaValue.Record([| ("date", ParaValue.Date (DateTime(1821, 1, 1)))|]))

[<Test>]
let ``load zip text file`` () =
  let path = Path.Combine("data", "eu4txt-zip.eu4")
  ParaValue.Load(path, "EU4bin", "EU4txt", lazy(dict([])))
  |> shouldEqual (ParaValue.Record([| ("date", ParaValue.Date (DateTime(1821, 1, 1)))|]))
