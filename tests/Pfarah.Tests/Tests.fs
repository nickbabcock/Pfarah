module Pfarah.Tests

open Pfarah
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
let ``parse obj as date`` () =
  let obj = ParaValue.Parse "foo=1492.1.2"
  obj?foo|> asDate |> shouldEqual (new DateTime(1492, 1, 2))

[<Test>]
let ``parse obj as tough array`` () =
  let obj = ParaValue.Parse "foo={1 bar 2.0 {qux=baz}}"
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