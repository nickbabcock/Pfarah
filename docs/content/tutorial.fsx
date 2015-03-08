(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
# Introducing Pfarah

This document is a quick overview of the most important features of Pfarah.
You can also get this page as an [F# script file from GitHub][fscript] and run
the samples interactively.

[fscript]: https://github.com/nickbabcock/Pfarah/blob/master/docs/content/tutorial.fsx

There are two ways to parse data. The first way is to parse a specific file,
and the second is to parse a given string. The norm should be parsing a file,
but for the sake of the tutorial it is shown the second way.

If you're familiar with [FSharp.Data JSON][fdj] library, you'll be familiar with this.

[fdj]: http://fsharp.github.io/FSharp.Data/library/JsonValue.html

## Quickstart

*)
#r "Pfarah.dll"
open Pfarah

let obj = ParaValue.Parse "foo=bar"
(**

`obj` is a [discriminated union][du], and if you're not familiar with
discriminated unions, it may be best to read up on them because that is how
one interfaces with the data

[du]: https://msdn.microsoft.com/en-us/library/dd233226.aspx

## Working with the Data

*)

// Print the property's keys, so this will print "foo"
match obj with
| ParaValue.Record properties ->
  printfn "%A" (properties |> Seq.map fst)
| _ ->
  failwith "Expected `obj` to be a `Record` type"

// Print the property's value of "foo"
match obj with
| ParaValue.Record properties ->
  match (Array.tryFind (fst >> (=) "foo") properties) with
  | Some(key, value) ->
    match value with
    | ParaValue.String str -> printfn "%s" str
    | _ -> failwith "Expected foo to be a string"
  | None -> failwith "Expected foo to exist"
| _ -> failwith "Expected `obj` to be a `Record` type"

(**

## Convenience Methods

The methods shown are very boilerplate heavy and writing this kind of code can
make a project become bloated very quickly. Hence why Pfarah provides a bit of
syntatic sugar and helper methods to make dealing with the data easier.

*)

// The previous example of printing foo's value can be shortened to
let obj2 = ParaValue.Parse "foo=bar"
printfn "%s" (obj2?foo |> asString)

let data = ParaValue.Parse "y={1 2 4 8 16 32}"
printfn "%A" (data?y |> asArray |> Array.map asInteger)

(**

## Dealing with Multiple Keys

It is often common that the data format will have multiple keys of the same
name on the same level, but the number of occurrences of the key in an object is
subject to differ. In cases such as these it is best to use `collect`, which
will aggregate values in an object with that particular key.

*)

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

// Let's create a list of tuples such that the result is army name * list of
// unit names
let armyData =
  ParaValue.Parse army
  |> collect "army"
  |> Array.map (fun x ->
    let units = x |> collect "unit" |> Array.map (fun u -> u?name |> asString)
    x?name |> asString, units)
printfn "%A" armyData

(**

## Inconsistent Data

Not all objects of a given instance will have the exact same property keys.
Some may only have a limited subset of the properties wanted. The `tryFind`
function will return `Some ParaValue` if the property exists and `None` if it
doesn't.

*)

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
printfn "%A" armyData