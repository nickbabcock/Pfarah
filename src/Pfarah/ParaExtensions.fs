namespace Pfarah

[<AutoOpen>]
module ParaExtensions =

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

  /// Get the integer value of an object
  let asInteger x =
    match x with
    | ParaValue.Number n -> int n
    | _ -> failwithf "Not an integer: %s" (x.ToString())

  /// Get the string value of an object
  let asString x =
    match x with
    | ParaValue.String s -> s
    | _ -> x.ToString()

  /// Get the floating point precision value of an object
  let asFloat x =
    match x with
    | ParaValue.Number n -> n
    | _ -> failwithf "Not a float: %s" (x.ToString())

  /// Get the boolean value of an object
  let asBool x =
    match x with
    | ParaValue.Bool b -> b
    | ParaValue.Number n -> (int n) <> 0
    | _ -> failwithf "Not a bool: %s" (x.ToString())

  /// Get the date value of an object
  let asDate x =
    match x with
    | ParaValue.Date d -> d
    | _ -> failwithf "Not a date: %s" (x.ToString())

  /// Get the array value of an object
  let asArray x = 
    match x with
    | ParaValue.Array elements -> elements
    | _ -> failwithf "Not an array: %s" (x.ToString())

  /// Get the record value of the object
  let asRecord x =
    match x with
    | ParaValue.Record properties -> properties
    | _ -> failwithf "Not a record: %s" (x.ToString())

  /// Finds all the properties of the object with a given key and aggregates
  /// all the values under a single array.
  let collect prop obj =
    match obj with
    | ParaValue.Record properties ->
      properties
      |> Array.filter (fst >> (=) prop)
      |> Array.map snd
    | _ -> failwithf "Not an object: %A" obj

  /// Tries to find the first property of the object that has the given key.
  /// If a property is found then `Some ParaValue` will be returned else
  /// `None`
  let tryFind prop obj =
    match obj with
    | ParaValue.Record properties ->
      properties |> Array.tryFind (fst >> (=) prop) |> Option.map snd
    | _ -> None

  type ParaValue with
    /// Assumes the object is an array and returns the enumerator for the
    /// array
    member x.GetEnumerator () = asArray(x).GetEnumerator()

    /// Assumes the object is an array and returns the value at a given index
    /// in the array
    member x.Item(index) = asArray(x).[index]