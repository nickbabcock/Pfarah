namespace Pfarah

[<AutoOpen>]
module ParaExtensions =
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
  let findOptional (objs:seq<(string * ParaValue)[]>) =
    // Boil the given objects down to top level property names
    let props = objs |> Seq.map ((Seq.map fst) >> Set.ofSeq)

    let all = Set.unionMany props
    let optional = props |> Seq.map (Set.difference all) |> Set.unionMany

    (Set.difference all optional), optional

  type ParaValue with
    /// Assumes the object is an array and returns the enumerator for the
    /// array
    member x.GetEnumerator () =
      x |> ParaValue.asArray |> ParaResult.get |> fun a -> a.GetEnumerator()

    /// Assumes the object is an array and returns the value at a given index
    /// in the array
    member x.Item(index) =
      x |> ParaValue.asArray |> ParaResult.get |> fun a -> a.[index]

module Operators =
  /// Retrieves the property on the ParaValue Record with the given property
  /// name. If the object is not a Record or the property does not exist on
  /// the Record, the function will return an error
  let (?) (obj:ParaValue) (key:string) : ParaResult<ParaValue> = ParaValue.get key obj

  /// Slash operator to emulate xpath operations, see collect.
  /// Inspired by json4s
  let (/) (obj:ParaValue) propertyName = collect propertyName obj

  /// Slash operator to emulate xpath operations, see collectAll.
  /// Inspired by json4s. The ideal operator would be `//` but `//`
  /// is interpreted as a comment
  let (/./) (obj:ParaValue) propertyName = collectAll propertyName obj

  /// Apply operator adopted from the chiron project
  let inline (<*>) f m : ParaValue<'a> = ApplicativeParaValue.apply f m

  /// Lift operator adopted from the chiron project
  let inline (<!>) f m : ParaValue<'a> = ApplicativeParaValue.map f m

  /// Prefix operator that delegates pget and sweetens the deal
  let inline (!.) key : ParaValue<'a> = ApplicativeParaValue.wrap (pget key)

  /// Operator inspired from the fleece project -- delegates to pget
  let inline (.@) obj key = pget key obj
