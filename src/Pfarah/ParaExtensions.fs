namespace Pfarah

[<AutoOpen>]
module ParaExtensions =
  let (?) (obj:ParaValue) propertyName =
    match obj with
    | ParaValue.Record properties ->
      match Array.tryFind (fst >> (=) propertyName) properties with 
      | Some (_, value) -> value
      | None -> failwithf "Didn't find property '%s' in %A" propertyName obj
    | _ -> failwithf "Not an object: %A" obj

  let asInteger x =
    match x with
    | ParaValue.Number n -> int n
    | _ -> failwithf "Not an integer: %s" (x.ToString())

  let asString x =
    match x with
    | ParaValue.String s -> s
    | _ -> x.ToString()

  let asFloat x =
    match x with
    | ParaValue.Number n -> n
    | _ -> failwithf "Not a float: %s" (x.ToString())

  let asBool x =
    match x with
    | ParaValue.Bool b -> b
    | ParaValue.Number n -> (int n) <> 0
    | _ -> failwithf "Not a bool: %s" (x.ToString())

  let asDate x =
    match x with
    | ParaValue.Date d -> d
    | _ -> failwithf "Not a date: %s" (x.ToString())

  let asArray x = 
    match x with
    | (ParaValue.Array elements) -> elements
    | _ -> [| |]

  type ParaValue with
    member x.GetEnumerator () = asArray(x).GetEnumerator()
    member x.Item(index) = asArray(x).[index]