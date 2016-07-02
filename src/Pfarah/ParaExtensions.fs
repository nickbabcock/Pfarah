namespace Pfarah

[<AutoOpen>]
module ParaExtensions =
  /// Get the integer value of an object
  let asInteger = function
    | ParaValue.Number n -> int n
    | x -> failwithf "Not an integer: %s" (x.ToString())

  /// Get the string value of an object
  let asString = function
    | ParaValue.String s -> s
    | x -> x.ToString()

  /// Get the floating point precision value of an object
  let asFloat = function
    | ParaValue.Number n -> n
    | x -> failwithf "Not a float: %s" (x.ToString())

  /// Get the boolean value of an object
  let asBool = function
    | ParaValue.Bool b -> b
    | ParaValue.Number n -> (int n) <> 0
    | x -> failwithf "Not a bool: %s" (x.ToString())

  /// Get the date value of an object
  let asDate = function
    | ParaValue.Date d -> d
    | x -> failwithf "Not a date: %s" (x.ToString())

  /// Returns the integer value if it exists else 0
  let integerDefault = function Some(x) -> x |> asInteger | None -> 0

  /// Returns the string value if it exists else the empty string
  let stringDefault = function Some(x) -> x |> asString | None -> ""

  /// Returns the float value if it exists else 0.0
  let floatDefault = function Some(x) -> x |> asFloat | None -> 0.0

  /// Returns the boolean value if it exists else false
  let boolDefault = function Some(x) -> x |> asBool | None -> false

  /// Get the array value of an object
  let asArray = function
    | ParaValue.Array elements -> elements
    | x -> failwithf "Not an array: %s" (x.ToString())

  /// Get the record value of the object
  let asRecord = function
    | ParaValue.Record properties -> properties
    | x -> failwithf "Not a record: %s" (x.ToString())

  /// Finds all the properties of the object with a given key and aggregates
  /// all the values under a single array. If a given object is an array
  /// all sub-objects are aggregated. If not an array or object, an empty
  /// array is returned.
  let collect prop obj : ParaValue =
    let rec findByName prop obj =
      match obj with
      | ParaValue.Record properties ->
        properties
        |> Array.filter (fst >> (=) prop)
        |> Array.map snd
      | ParaValue.Array arr -> arr |> Array.collect (findByName prop)
      | _ -> [| |]
    findByName prop obj |> ParaValue.Array

  /// Finds all the properties of the object and sub-objects with a given key
  /// and aggregates all the values under a single array. If a given object is
  /// an array all sub-objects are aggregated. If not an array or object, an
  /// empty array is returned.
  let collectAll prop obj : ParaValue =
    let rec findAllByName prop obj : seq<ParaValue> =
      match obj with
      | ParaValue.Record properties ->
        let found = properties |> Seq.filter (fst >> (=) prop) |> Seq.map snd
        let sub = properties |> Seq.map snd |> Seq.collect (findAllByName prop)
        Seq.append found sub
      | ParaValue.Array arr -> arr |> Seq.collect (findAllByName prop)
      | _ -> Seq.empty

    ParaValue.Array (findAllByName prop obj |> Seq.toArray)

  /// Tries to find the first property of the object that has the given key.
  /// If a property is found then `Some ParaValue` will be returned else
  /// `None`
  let tryFind prop obj =
    match obj with
    | ParaValue.Record properties ->
      properties |> Array.tryFind (fst >> (=) prop) |> Option.map snd
    | _ -> None

  /// Given a sequence of similar objects, return a tuple of required
  /// properties and optional properties
  let findOptional (objs:seq<ParaValue>) =
    // Boil the given objects down to top level property names
    let props = objs |> Seq.map (asRecord >> (Seq.map fst) >> Set.ofSeq)

    let all = Set.unionMany props
    let optional = props |> Seq.map (Set.difference all) |> Set.unionMany

    (Set.difference all optional), optional

  type ParaValue with
    /// Assumes the object is an array and returns the enumerator for the
    /// array
    member x.GetEnumerator () = asArray(x).GetEnumerator()

    /// Assumes the object is an array and returns the value at a given index
    /// in the array
    member x.Item(index) = asArray(x).[index]

module Operators =
  /// Retrieves the property on the ParaValue Record with the given property
  /// name. If the object is not a Record or the property does not exist on
  /// the Record, the function will fail
  let (?) (obj:ParaValue) propertyName =
    match obj with
    | ParaValue.Record properties ->
      match Array.tryFind (fst >> (=) propertyName) properties with
      | Some (_, value) -> value
      | None -> failwithf "Didn't find property '%s' in %A" propertyName obj
    | _ -> failwithf "Not an object: %A" obj

  /// Slash operator to emulate xpath operations, see collect.
  /// Inspired by json4s
  let (/) (obj:ParaValue) propertyName = collect propertyName obj

  /// Slash operator to emulate xpath operations, see collectAll.
  /// Inspired by json4s. The ideal operator would be `//` but `//`
  /// is interpreted as a comment
  let (/./) (obj:ParaValue) propertyName = collectAll propertyName obj

  open Pfarah.Functional
  /// Apply operator adopted from the chiron project
  let inline (<*>) f m : ParaValue<'a> = apply f m

  /// Lift operator adopted from the chiron project
  let inline (<!>) f m : ParaValue<'a> = map f m

  /// Prefix operator that delegates pget and sweetens the deal
  let inline (!.) key : ParaValue<'a> = fun o -> pget o key, o

  /// Operator inspired from the fleece project -- delegates to pget
  let inline (.@) obj key = pget obj key
