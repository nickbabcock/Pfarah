namespace Pfarah

[<AutoOpen>]
module ParaExtensions =
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

  /// Slash operator to emulate xpath operations, see collectAll.
  /// Inspired by json4s. The ideal operator would be `//` but `//`
  /// is interpreted as a comment
  let (/./) (obj:ParaValue) propertyName = ParaValue.collectAll propertyName obj

  /// Delegates to ParaResult.bind. Adopted from Chessie and Haskell
  let inline (>>=) result f = ParaResult.bind f result

  /// Apply operator adopted from the chiron project
  let inline (<*>) f m : ParaValue<'a> = ApplicativeParaValue.apply f m

  /// Lift operator adopted from the chiron project
  let inline (<!>) f m : ParaValue<'a> = ApplicativeParaValue.map f m

  /// Prefix operator that delegates pget and sweetens the deal
  let inline (!.) key : ParaValue<'a> = ApplicativeParaValue.wrap (pget key)

  /// Operator inspired from the fleece project -- delegates to pget
  let inline (.@) obj key = pget key obj

  /// Operator inspired from the fleece project -- delegates to tryPget
  let inline (.@?) obj key = tryPget key obj
