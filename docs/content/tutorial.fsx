(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "Pfarah.dll"
open System

(**
# Introducing Pfarah

This document is a quick overview of the most important features of Pfarah.
You can also get this page as an [F# script file from GitHub][fscript] and run
the samples interactively. Type annotations are used in examples to ease
understanding

[fscript]: https://github.com/nickbabcock/Pfarah/blob/master/docs/content/tutorial.fsx

There are two ways to parse data. The first way is to parse a specific file,
and the second is to parse a given string. The norm should be parsing a file,
but for the sake of the tutorial it is shown the second way.

This library takes inspiration from [FSharp.Data JSON][fdj], [Chessie][],
[Chiron][], [Fleece][], [Haskell][], and [json4s][].

[fdj]: http://fsharp.github.io/FSharp.Data/library/JsonValue.html
[Chessie]: https://github.com/fsprojects/Chessie
[Chiron]: https://github.com/xyncro/chiron
[Fleece]: https://github.com/mausch/Fleece
[Haskell]: https://www.haskell.org/
[json4s]: https://github.com/json4s/json4s

## Quickstart

We're going to start with querying parsed data and as an example the data will
represent a ship named bessie with 22 men onboard.

*)

open Pfarah

let (obj : ParaValue) = ParaValue.Parse "name=bessie strength=22"

(**

The next step is extracting the information. There are many ways to accomplish
this and each one will be demonstrated. Which one you choose will be personal
preference. As the demonstrations become concise they introduce more advanced
concepts, so initial examples will use as simple constructs.

`ParaValue` is a [discriminated union][du] and the first example queries it
directly for more information.

[du]: https://msdn.microsoft.com/en-us/library/dd233226.aspx

*)

// Print the property's keys -- will print "seq ["name"; "strength"]"
// Also demonstrates all the current cases for `ParaValue`
match obj with
| ParaValue.Record (properties : (string * ParaValue)[]) ->
  printfn "%A" (properties |> Seq.map fst)
| ParaValue.Array (arr : ParaValue[]) ->
  failwith "Did not expect `obj` to be an array"
| ParaValue.Bool (x : bool) ->
  failwith "Did not expect `obj` to be a bool"
| ParaValue.Date (x : DateTime) ->
  failwith "Did not expect `obj` to be a DateTime"
| ParaValue.Hsv ((h : float), (s : float), (v : float)) ->
  failwith "Did not expect `obj` to be hsv"
| ParaValue.Number (n : float) ->
  failwith "Did not expect `obj` to be a number"
| ParaValue.Rgb ((r : byte), (g : byte), (b : byte)) ->
  failwith "Did not expect `obj` to be rgb"
| ParaValue.String (s : string) ->
  failwith "Did not expect `obj` to be a string"

(**

Whoa! Those are a lot of data types. To see each of the data types in use, see
the [Data Format](data-format.html) page.

Instead of exhaustively enumerating all the cases every time we query the data,
the next example will use F# default case and print the name of the ship

*)

// Try to find the first key with "name". If name is found then see if the
// value is a string. If so, print the ship's name. If none of the happy path
// is followed print errors
match obj with
| ParaValue.Record properties ->
  match (Array.tryFind (fst >> (=) "name") properties) with
  | Some(key, value) ->
    match value with
    | ParaValue.String name -> printfn "Ship's name: %s" name
    | _ -> failwith "Expected name to be a string"
  | None -> failwith "Expected name to exist"
| _ -> failwith "Expected `obj` to be a `Record` type"

(**

By now, being flustered with how cumbersome and verbose the examples is ok.
This will be fixed by introducing some additional concepts and types. First,
the previous example rewritten and an explanation after.

*)

let (nameVal : ParaResult<ParaValue>) = ParaValue.get "name" obj
let (name : ParaResult<string>) = ParaResult.bind ParaValue.asString nameVal
match name with
| Ok(nm : string) -> printfn "Ship's name: %s" nm
| Error(err : string) -> printfn "%s" err

(**

Already we've gone from nine lines down to five, and it's all possible because
the values returned by the functions are now wrapped in `ParaResult<T>`, which
is a very simple type encapsulating either a value or an error. The
`ParaValue.get` either returns the value of the field with a key of "name" or
an error. This error, which is not an exception, could be anything from `obj`
not being a record to there not being a single field with a key of "name"

For similar data types (to name a few) see:

- Rust's [Result](https://doc.rust-lang.org/book/error-handling.html#the-result-type)
- Scala's [Either](http://www.scala-lang.org/api/2.11.8/#scala.util.Either)
- Haskell's [Either](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Either.html)
- Go's multiple return values with the error type
- F#'s [Choice](https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/core.choice%5B't1,'t2%5D-union-%5Bfsharp%5D)
 
In fact, ParaResult is defined as an alias for `Choice<'a,string>`, so any
libraries or utilities that work with choices like
[ExtCore](https://github.com/jack-pappas/ExtCore) can interface seamlessly.

There is one possible question remaining for the unaccustomed and that is
`ParaResult.bind`:

    [lang=fsharp]
    let (name : ParaResult<string>) = ParaResult.bind ParaValue.asString nameVal

Bind checks to see if the result passed in (nameVal) is an error or a result.
If nameVal is an error (`get` failed earlier) then the error is propogated.
Else if there is a value contained a function is applied to the value
(in this case ParaValue.String "bessie"). The function being applied is
`ParaValue.asString`, which unwraps the value so that just "bessie" is
exposed.

The implementation of bind is quite concise and may prove illustrative

*)

// Can be read as: a function named bind that takes two parameters:
// - fn: a function that when given a value returns a ParaResult
// - m: the value to check if is currently an error
// - returns a new ParaResult
let bind (fn: 'a -> ParaResult<'b>) (m: ParaResult<'a>) : ParaResult<'b> =
  match m with
  | Ok(x) -> fn x
  | Error(x) -> Error(x)

(**

Bind allows Pfarah to define a custom [computation expression][], which
induces syntax sugar using `let~` and `return!` so that one doesn't have
to deal with ParaResult explicitly

[computation expression]: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/computation-expressions-%5Bfsharp%5D

*)

let name2 : ParaResult<string> = para {
  let! nameVal = ParaValue.get "name" obj
  return! ParaValue.asString nameVal
}

match name2 with
| Ok(nm) -> printfn "Ship's name: %s" nm
| Error(err) -> printfn "%s" err

(**

While the number of lines of code in the example grew, computation expressions
start to shine when the queries become complex. Instead of extracting just the
name, extract the number of men on the ship (strength)

*)

let data : ParaResult<string * int> = para {
  let! nameVal = ParaValue.get "name" obj
  let! strengthVal = ParaValue.get "strength" obj
  let! name = ParaValue.asString nameVal
  let! strength = ParaValue.asInteger strengthVal
  return name, strength
}

match data with
| Ok(name, strength) -> printf "Ship: %s. Men: %d" name strength
| Error(err) -> printfn "%s" err

(**

The potential is starting to show. Still some cruft is getting in the way,
which can be solved by defining custom operators:

- `?` is aliased to `ParaValue.get`
- `>>=` is aliased to `ParaResult.bind`

*)

open Pfarah.Operators

para {
  let! name = obj?name >>= ParaValue.asString
  let! strength = obj?strength >>= ParaValue.asInteger
  return name, strength
}

// Or even terser
open Pfarah.ParaValue
para {
  let! name = obj?name >>= asString
  let! strength = obj?strength >>= asInteger
  return name, strength
}

(**

## Dealing with Multiple Ships

Bessie isn't the only ship in the world. Our data is about to get more complex,
but don't worry, Pfarah will be there every step of the way.

*)

let shipsObj = ParaValue.Parse """
  ship={
    name=bessie
    strength=22
  }
  ship={
    name=doris
    strength=40
  }
  ship={
    name=betsy
    strength=10
  }
"""

// Let's define a common function to parse each ship
let parseShip (ship : ParaValue) = para {
  let! name = ship?name >>= asString
  let! strength = ship?strength >>= asInteger
  return name, strength
}

// Collect all the values with a key "ship" into a ParaValue.Array
let (pips : ParaValue) = ParaValue.collect "ship" shipsObj

// Execute `parseShip` on each ship and aggregate the result
// into an array
let (extract : ParaResult<(string * int)[]>) =
  ParaValue.flatMap parseShip pips

// Sort the ships by the most men first
let (sorted : ParaResult<(string * int)[]>) =
  ParaResult.map (Array.sortByDescending snd) extract

match sorted with
| Ok(ships) -> Array.iter (fun (name, strength) -> 
    printfn "Ship: %s. Strength: %d" name strength) ships
| Error(error) -> printfn "%s" error

// In reality the code may be written like:
shipsObj
|> ParaValue.collect "ship"
|> ParaValue.flatMap parseShip
|> ParaResult.map (Array.sortByDescending snd)
|> function
| Ok(ships) -> Array.iter (fun (name, strength) -> 
    printfn "Ship: %s. Strength: %d" name strength) ships
| Error(error) -> printfn "%s" error

(**

Those who like the computation builder don't have to miss out!

*)

para {
  let! (pips : ParaValue[]) = ParaValue.getAll "ship" shipsObj
  let! (ships : (string * int)[]) = ParaValue.reduce parseShip pips
  for (name, strength) in (Array.sortByDescending snd ships) do
    printfn "Ship: %s. Strength: %d" name strength
} |> function Error(err) -> printfn "%s" err | _ -> ()

(**

But why the different functions?

collect vs getAll: Both accept a ParaValue and look for a properties of a
certain key, but collect will also work on ParaValue.Array by iterating over
each element looking for the key. collect returns a ParaValue.Array, which
allows subsequent calls to be chained together:

*)

// Will return Ok [| "bessie"; "doris" "betsy" |]
ParaValue.collect "ship" obj
|> ParaValue.collect "name"
|> ParaValue.flatMap ParaValue.asString

(**

Defined in the operator module there is the `/` operator that will delegates
to `ParaValue.collect`. For those that are familiar with [xpath], this should
appear similar

[xpath]: https://en.wikipedia.org/wiki/XPath

*)

obj / "ship" / "name" |> ParaValue.flatMap ParaValue.asString
obj / "ship" / "name" |> flatMap asString

(**

Going back to the commputation builder vs the pipeline method, another
difference is the function that `parseShip` is passed to. The computation
expression only works array of ParaValue whereas like `collect`,
the pipeline method operates on the values of Records, and can map
singular values like ParaValue.String, etc.

Knowing which one to use is sometimes only a matter of taste.

## Optional Data

Not all objects of a given instance will have the exact same property keys.
Some may only have a limited subset of the properties wanted.

In our ship example, we'll have an optional property, patrol, that denotes
if a ship is on patrol. If absent, the ship is assumed to not be on patrol.

*)

let patrolData = """
  ship={
    name=bessie
    strength=22
    patrol=yes
  }
  ship={
    name=doris
    strength=40
  }
  ship={
    name=betsy
    strength=10
    patrol=yes
  }
"""

// Let's define a common function to parse each ship with patrol
let parseShip2 (ship : ParaValue) = para {
  let! name = ship?name >>= asString
  let! strength = ship?strength >>= asInteger
  let! (patrolVal : ParaValue option) = ParaValue.tryGet "patrol" ship

  // Try converting the potential value to a boolean, if not there
  // then assume the ship is not on patrol.
  let! patrol = patrolVal |> ParaResult.defaultOpt asBool false
  return name, strength, patrol
}

para {
  let obj = ParaValue.Parse patrolData
  let! (pips : ParaValue[]) = ParaValue.getAll "ship" obj
  let! (ships : (string * int * bool)[]) = ParaValue.reduce parseShip2 pips

  // Take the ship name and strength of the ships that are on patrol
  let shipsOnPatrol =
    ships
    |> Array.filter (fun (_, _, patrol) -> patrol)
    |> Array.map (fun (key, strength, patrol) -> (key, strength))

  for (name, strength) in (Array.sortByDescending snd shipsOnPatrol) do
    printfn "Ship: %s. Strength: %d" name strength
} |> function Error(err) -> printfn "%s" err | _ -> ()

(**

## Finding Optional Data

Knowing the data is the first step to any type of analysis. This is made
difficult when there can be thousands of objects, each one having a subset of
the properties available. `findOptional` fixes this problem by dissecting a
list of supposedly similar objects and returning the properties that it knows
are always present and the ones that are optional.

*)


// Find all the optional properties on ships. Append a question mark after the
// property name to signify that the property is optional.
para {
  let obj = ParaValue.Parse patrolData
  let! ships = obj / "ship" |> ParaValue.flatMap asRecord
  let required, optional = findOptional ships
  required |> Seq.iter (printfn "%s")
  optional |> Seq.iter (printfn "%s?")
} |> function Error(err) -> printfn "%s" err | _ -> ()

// Will print:
// name
// strength
// patrol?

(**

## Deserialization

Working with primitives like string and ints are fine, but programs become
much more powerful when compositive data types come into play. While the
previous methods allow for manual deserialization Pfarah offers another step
of convenience.

*)

// Let's simplify the data by removing the optional patrol field.
// We'll add it back in later
let multipleShips = """
  ship={
    name=bessie
    strength=22
  }
  ship={
    name=doris
    strength=40
  }
  ship={
    name=betsy
    strength=10
  }
"""

// Define a simple record to store the name and strength of a ship
// with a convenience `Create` constructor along with a specially
// named `FromPara` function
type Ship = { Name: string; Strength: int }
with
  static member inline Create name strength =
    { Ship.Name = name; Strength = strength }
  static member inline FromPara (_:Ship) =
    Ship.Create <!> (!. "name") <*> (!. "ship")

let (ships : Ship[]) = deserialize (ParaValue.Parse multipleShips)

(**

Full stop. There's a lot of magic in the previous example including a
couple unseen operators.

First, the type annotation on `ships` is critical, without it the compiler
won't know what to deserialize the type to and raise a compiler error. Pfarah
know how to deserialize an array, so it then proceeds to look at the element
type. Primitives like string and ints are no problem, but `Ship` is new. As
long as `Ship` implements a function `FromPara`, Pfarah can deserialize it.
This is known as [statically resolved type parameters][], and it is a very
dark corner of F#.

But let's take a step back because we can use this magic in baby steps

[statically resolved type parameters]: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/statically-resolved-type-parameters-%5Bfsharp%5D

*)

// Remember this function? This time we're using `fromPara` which will infer
// what `as*` function to execute based on the type of the variable.
let parseShip3 (ship : ParaValue) : ParaResult<string * int> = para {
  let! name = ship?name >>= fromPara
  let! strength = ship?strength >>= fromPara
  return name, strength
}

// The function pget simplifies things a bit
let parseShip4 (ship : ParaValue) : ParaResult<string * int> = para {
  let! name = pget "name" ship
  let! strength = pget "strength" ship
  return name, strength
}

// There is also an `.@` that is aliased to pget
let parseShip5 (ship : ParaValue) : ParaResult<string * int> = para {
  let! name = ship .@ "name"
  let! strength = ship .@ "strength"
  return name, strength
}

(**

That probably looks and feels a lot better for the uninitiated. We can rewrite
the magic parts with our new function.

*)

type Ship2 = { Name: string; Strength: int }
with
  static member inline FromPara (_:Ship2) =
    fun ship -> para {
      let! name = ship .@ "name"
      let! strength = ship .@ "strength"
      return { Ship2.Name = name; Strength = strength }
    } |> ApplicativeParaValue.wrap

(**

Pretty neat right? We can throw in our optional patrol pretty easily if you
know the right function!

*)

type Ship3 = { Name: string; Strength: int; Patrol: bool option }
with
  static member inline FromPara (_:Ship3) =
    fun ship -> para {
      let! name = ship .@ "name"
      let! strength = ship .@ "strength"
      let! patrol = tryPget "patrol" ship
      return { Ship3.Name = name; Strength = strength; Patrol = patrol }
    } |> ApplicativeParaValue.wrap

(**

## Binary Data

The examples that we have been working with have been plain text, but
Clausewitz files can come compressed and in binary form. To parse these files,
we'll need a few things:

- The file path.
- The header if it is binary file. For instance, for EU4, the header is
  "EU4bin".
- The header if it is plain text file. For EU4, the header is "EU4txt".
- Since binary files use two byte tokens instead of strings for identifiers,
  we'll need   a dictionary of two byte tokens to strings so that the binary
  file can be queried exactly   like a plain text file. There are many types of
  tokens that can be encountered, so as not to   impose a memory tax
  unnecessarily if it is a plain text file, the dictionary is `lazy`

The following code sample will work for a file that is in any format
(plain text/binary and compressed/uncompressed)

*)

let path = "game.eu4"
let ``binary header`` = "EU4bin"
let ``text header`` = "EU4txt"

// Only if the file is detected to be binary will
// this dictionary be created
let tokens = lazy dict([(0x284ds, "date")])
let game = ParaValue.Load(path, ``binary header``, ``text header``, tokens)
game?date >>= ParaValue.asDate
