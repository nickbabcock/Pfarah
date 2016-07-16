namespace Pfarah

open Utils
open System
open System.IO
open System.Text
open System.Collections.Generic
open Ionic.Zip
open System.Globalization
open Farmhash.Sharp


[<RequireQualifiedAccess>]
type ParaValue =
  | Bool of bool
  | Number of float
  | Hsv of float * float * float
  | Rgb of byte * byte * byte
  | Date of DateTime
  | String of string
  | Array of elements:ParaValue[]
  | Record of properties:(string * ParaValue)[]
with
  /// Returns a string of the prettified structure. For simple cases it will
  /// just print the contents (string, bool, number, and date). For arrays and
  /// records, the result will contain a newline string of each value with
  /// each value indented depending on how nested the structure. The one exception
  /// to is when the array or object is empty, which will just print the brackets
  /// on the same line.
  static member private Prettify value indent =
    let nestedPrint inner = ParaValue.Prettify inner (indent + 1)
    let indentStr = System.String(' ', (indent + 1) * 2)
    match value with
    | String(x) -> x
    | Bool(x) -> sprintf "%b" x
    | Number(x) -> sprintf "%.3f" x
    | Date(x) -> (x.ToString("yyyy.M.d"))
    | Hsv(h, s, v) -> sprintf "(%.3f, %.3f, %.3f)" h s v
    | Rgb(r, g, b) -> sprintf "(%i, %i, %i)" r g b
    | Array(arr) ->
      let stringed = arr |> Array.map (nestedPrint >> (sprintf "%s%s" indentStr))
      let contents = String.Join(",\n", stringed)

      match contents with
      | "" -> "[]"
      | x -> sprintf "[\n%s\n%s]" x (System.String(' ', indent * 2))
    | Record(cord) ->
      let stringed = cord |> Seq.map (fun (key, v) -> sprintf "%s%s: %s" indentStr key (nestedPrint v))
      let contents = String.Join(",\n", stringed)

      match contents with
      | "" -> "{}"
      | x -> sprintf "{\n%s\n%s}" x (System.String(' ', indent * 2))

  override this.ToString() = ParaValue.Prettify this 0

module Frequencies =
  // There is only two possible values for booleans, so cache these bad boys
  let ptrue = ParaValue.Bool true
  let pfalse = ParaValue.Bool false

module internal Common =
  /// Returns whether a given int is considered whitespace
  /// Inlining isspace caused a 10% performance increase (cf72dd4)
  let inline isspace (c:int) = c = 10 || c = 13 || c = 9 || c = 32

  let internal parseArrayFirst first (fn:ResizeArray<_> -> ParaValue[]) =
      let values = ResizeArray<_>()
      values.Add(first)
      ParaValue.Array (fn values)

type private ParaParser (stream:PeekingStream) =
  // Caches that allow us to take the 64bit hash of a byte array of some
  // length and map it to another object thus reducing gc allocations and gc
  // pressure. In theory these caches are not fool proof because a hash
  // collision would be disasterous, but in reality, in a domain of 200,000
  // unique hashes, the chances of a collision is 1 in a billion.
  let cache = Dictionary<uint64, ParaValue>()
  let strCache = Dictionary<uint64, string>()

  let asByte = function
  | ParaValue.Number n -> byte n
  | x -> failwithf "Not an integer: %s" (x.ToString())

  let asFloat = function
  | ParaValue.Number n -> n
  | x -> failwithf "Not a float: %s" (x.ToString())

  /// The max token size of any string, as defined by paradox internal source
  /// code is 256
  let (stringBuffer:byte[]) = Array.zeroCreate 256

  /// Mutable variable to let us know how much of the string buffer is filled
  let mutable stringBufferCount = 0

  /// Advance the stream until a non-whitespace character is encountered
  let skipWhitespace (stream:PeekingStream) =
    while (Common.isspace (stream.Peek())) do
      stream.Read() |> ignore

  /// Trim comments from stream
  let rec skipComments () =
    if (stream.Peek() = 35) then
      let mutable don = false
      while not don do
        let next = stream.Read()
        don <-
          match next with
          | -1 -> true
          | 13 ->
            skipWhitespace stream
            skipComments()
            true
          | _ -> false

  /// Trim leading and trailing whitespace from a value
  let trim fn =
    skipWhitespace(stream)
    let result = fn()
    skipWhitespace(stream)
    result

  let advance fn =
    trim skipComments
    let result = fn()
    trim skipComments
    result

  let rec parseValue () =
    match stream.Peek() with
    | 34 -> parseQuotes()
    | 123 ->
      // Read through '{'
      stream.Read() |> ignore
      let result = parseContainer()

      // Read through '}'
      assert (stream.Peek() = 125)
      stream.Read() |> ignore
      result
    | _ -> narrow ()

  and narrowBuffer() =
    // Now that we have a filled buffer, we have to determine what value it
    // holds. We first try the fast path of hashing the buffer and seeing if
    // the contents are something we have already seen (if so, just return
    // that value). Else we compute the value that the buffer holds.
    let hash = Farmhash.Hash64(stringBuffer, stringBufferCount)
    let result =
      let mutable pval = Frequencies.ptrue
      if cache.TryGetValue(hash, &pval) then pval
      else
        let newCache =
          match stringBufferCount with
          | 3 when stringBuffer.[0] = 121uy && stringBuffer.[1] = 101uy &&
                   stringBuffer.[2] = 115uy -> Frequencies.ptrue
          | 2 when stringBuffer.[0] = 110uy && stringBuffer.[1] = 111uy -> Frequencies.pfalse
          | _ ->
            // Try parsing the buffer as a number and then as a date, in that order.
            // We have somewhat of a waterfall conditional, but we avoid partial
            // active patterns because they do incur a heap allocation
            let num = parseDouble stringBuffer stringBufferCount
            if num.HasValue then
              ParaValue.Number (num.Value)
            else
              let date = parseDate stringBuffer stringBufferCount
              if date.HasValue then
                ParaValue.Date (date.Value)
              else
                ParaValue.String (Utils.getString stringBuffer stringBufferCount)
        cache.Add(hash, newCache)
        newCache
    stringBufferCount <- 0
    result

  and narrow () =
    fillBuffer()
    if stringBufferCount = 3 && stringBuffer.[0] = 104uy &&
      stringBuffer.[1] = 115uy && stringBuffer.[2] = 118uy then
      stringBufferCount <- 0
      match (advance parseValue) with
      | ParaValue.Array [| h; s; v |] -> ParaValue.Hsv(h |> asFloat, s |> asFloat, v |> asFloat)
      | _ -> failwith "Expected an array with three elements for hsv"
    else if stringBufferCount = 3 && stringBuffer.[0] = 114uy &&
      stringBuffer.[1] = 103uy && stringBuffer.[2] = 98uy then
      stringBufferCount <- 0
      match (advance parseValue) with
      | ParaValue.Array [| r; g; b |] -> ParaValue.Rgb(r |> asByte, g |> asByte, b |> asByte)
      | _ -> failwith "Expected an array with three elements for hsv"
    else
      narrowBuffer()

  and fillBuffer () =
    if stream.Peek() = 34 then
      stream.Read() |> ignore
      quotedFillBuffer()
    else
      let mutable isGood = true
      while isGood do
        let next = stream.Peek()

        // We are done reading the current string if we hit whitespace an equal
        // sign, the end of a buffer, or a left curly (end of an object/list)
        isGood <- not (next < 33 || next = 125 || (next = 61 && stringBufferCount <> 0))
        if isGood then
          stringBuffer.[stringBufferCount] <- byte (stream.Read())
          stringBufferCount <- stringBufferCount + 1
  
  and bufferToString () =
    // In order to avoid allocating many instances of the same string and
    // wasting memory in the process, query the string cache. Essentially this
    // is poor man's -- but faster man's string interning
    let hash = Farmhash.Hash64(stringBuffer, stringBufferCount)
    let result =
      let mutable str = ""
      if strCache.TryGetValue(hash, &str) then str
      else
        str <- getString stringBuffer stringBufferCount
        strCache.Add(hash, str)
        str
    stringBufferCount <- 0
    result

  and readString () =
    fillBuffer()
    bufferToString()

  and quotedFillBuffer() =
    // Read until the next quote
    while stream.Peek() <> 34 do
      stringBuffer.[stringBufferCount] <- (byte (stream.Read()))
      stringBufferCount <- stringBufferCount + 1
   
    // Read the trailing quote
    stream.Read() |> ignore

  and parseArray (vals:ResizeArray<_>) =
    while (stream.Peek() <> 125) do
      vals.Add(advance parseValue)
    vals.ToArray()

  and parseArrayElem firstElem =
    assert (stream.Peek() = 125)
    stream.Read() |> ignore
    Common.parseArrayFirst firstElem parseArray

  and parseContainerContents() =
    // The first key or element depending on object or list
    fillBuffer()
    skipWhitespace stream

    match (stream.Peek()) with
    | 125 -> ParaValue.Array ([| narrowBuffer() |])

    // An equals sign means we are parsing an object
    | 61 -> parseObject (bufferToString()) (fun (stream:PeekingStream) -> stream.Peek() = 125)
    | _ -> // parse list
      skipWhitespace stream
      Common.parseArrayFirst (narrowBuffer()) parseArray

  and parseContainer () =
    skipWhitespace stream
    match (stream.Peek()) with

    // Encountering a '}' means an empty object
    | 125 -> ParaValue.Record ([||])

    // Encountering a '{' means we are dealing with a nested list or the first
    // element is an empty object.
    | 123 ->
      stream.Read() |> ignore
      skipWhitespace stream
      if (stream.Peek() = 125) then
        stream.Read() |> ignore
        parseContainer()
      else
        advance fillBuffer
        match (stream.Peek()) with
        | 125 -> parseArrayElem (ParaValue.Array ([| narrowBuffer() |]))

        // An equals sign means we are parsing an object
        | 61 ->
            parseObject (bufferToString()) (fun (stream:PeekingStream) -> stream.Peek() = 125)
            |> parseArrayElem
        | _ -> // parse list
          skipWhitespace stream
          parseArrayElem (Common.parseArrayFirst (narrowBuffer()) parseArray)

    // A quote means a quoted list
    | 34 -> ParaValue.Array(parseArray(ResizeArray<_>()))

    // Else we are not quite sure what we are parsing, so we need more info
    | _ -> parseContainerContents()

  and parseObject key stopFn =
    // Read through the '='
    stream.Read() |> ignore
    let pairs = ResizeArray<_>()
    pairs.Add(key, advance parseValue)
    while not(stopFn stream) do
      // Beware of empty objects "{}" that don't have a key. If we encounter
      // them, just blow right by them.
      if (stream.Peek()) = 123 then
        stream.Read() |> ignore
        while (stream.Read()) <> 125 do
          ()
      else
        pairs.Add(parsePair())
      skipWhitespace stream
    ParaValue.Record (pairs.ToArray())

  and parseQuotes () =
    // Read through the quote
    stream.Read() |> ignore

    quotedFillBuffer()
    let date = parseDate stringBuffer stringBufferCount
    let result =
      if date.HasValue then
        ParaValue.Date (date.Value)
      else
        ParaValue.String (bufferToString())
    stringBufferCount <- 0
    result

  and parsePair () =
    let key = advance readString
    assert (stream.Peek() = 61)
    stream.Read() |> ignore
    key, advance parseValue

  member x.Parse () =
    // Before we too far into parsing the stream we need to check if we have a
    // header. If we do see a header, ignore it.
    let pairs = ResizeArray<_>()

    skipWhitespace stream
    while (stream.Peek() = 35) do
      skipComments ()
      skipWhitespace stream

    let first = readString()
    let result =
      match (stream.Peek()) with
      | 10 | 13 ->
        while (not stream.EndOfStream) do
          pairs.Add(parsePair())
        ParaValue.Record (pairs.ToArray())
      | _ ->
        skipWhitespace stream
        assert (stream.Peek() = 61)
        parseObject first (fun stream -> stream.EndOfStream)
    cache.Clear()
    strCache.Clear()
    result

[<RequireQualifiedAccess>]
type private BinaryToken =
| String
| Uint
| Int
| Token
| Float
| Bool
| OpenGroup
| EndGroup
| Equals

type private BinaryParaParser (stream:BinaryReader, lookup:IDictionary<int16, string>) =

  /// The current token that the binary parser is looking at
  let mutable tok = BinaryToken.Token

  /// Performance optimization: Instead of making `BinaryToken` a discriminated
  /// union, inline all the options as mutable variables. `BinaryToken` is after
  /// all, private. This trick made binary parsing 25% faster with 25% less
  /// memory usage, at the cost of making the code less functional and harder to
  /// read.
  let mutable stringToken = ""
  let mutable uint32Token : uint32 = uint32 0
  let mutable int32Token : int32 = int32 0
  let mutable tokenString = ""
  let mutable floatToken = 0.0
  let mutable boolToken = false

  /// Reads a string from the stream, which is two octets of length followed
  /// by windows 1252 encoded characters.
  let readString () = String(stream.ReadChars(stream.ReadUInt16() |> int))

  /// throws an exception with stream byte position for easier debugging
  let fail preamble token =
    let str =
      match token with
      | BinaryToken.String -> sprintf "String: %s" stringToken
      | BinaryToken.Uint -> sprintf "Uint: %d" uint32Token
      | BinaryToken.Int -> sprintf "Int: %d" int32Token
      | BinaryToken.Token -> sprintf "Token: %s" tokenString
      | BinaryToken.Float -> sprintf "Float: %f" floatToken
      | BinaryToken.Bool -> sprintf "Bool: %b" boolToken
      | BinaryToken.OpenGroup -> "Open Group"
      | BinaryToken.EndGroup -> "End Group"
      | BinaryToken.Equals -> "Equals"
    failwithf "%s: %s, Position %d" preamble str stream.BaseStream.Position

  /// Looks up the human friendly name for an id. If the name does not exist,
  /// use the id's string value as a substitute. Don't error out because it is
  /// unreasonable for the client to know all ids that exist and will ever
  /// exist.
  let lookupId id =
      match lookup.TryGetValue(id) with
      | (false, _) -> id.ToString()
      | (true, x) -> x

  /// An integer may be a date. This active pattern attempts to detect such
  /// occurrences, though it doesn't guarantee 100% accuracy, as numbers larger
  /// than 43,808,760 will be detected as dates.
  let (|HiddenDate|_|) value =
    // Performance optimization: instead of checking after Math.DivRem that the
    // years fall between [0, 10000), this branch ensures that `value` will lead
    // to years between [0, 10000)
    if value >= 43808760 && value < 131408760 then
      let (left, hours) = Math.DivRem(int(value), 24)
      let (left, days) = Math.DivRem(left, 365)
      let years = left - 5001
      let date =
        DateTime.MinValue
          .AddYears(years)
          .AddDays(float(if years = 0 then 0 else days + 1))
          .AddHours(float(hours))
      Some(date)
    else None

  let parseDouble () =
    let res = stream.ReadInt32() |> cut

    // The next integer is seemingly unused
    stream.ReadInt32() |> ignore
    res

  let parseToken token =
    match token with
    | 0x0014s -> uint32Token <- stream.ReadUInt32(); BinaryToken.Uint
    | 0x000cs -> int32Token <- stream.ReadInt32(); BinaryToken.Int
    | 0x000es -> boolToken <- stream.ReadByte() <> 0uy; BinaryToken.Bool
    | 0x000fs | 0x0017s -> stringToken <- readString(); BinaryToken.String
    | 0x000ds -> floatToken <- stream.ReadInt32() |> cut32; BinaryToken.Float
    | 0x0167s -> floatToken <- parseDouble(); BinaryToken.Float
    | 0x0003s -> BinaryToken.OpenGroup
    | 0x0004s -> BinaryToken.EndGroup
    | 0x0001s -> BinaryToken.Equals
    | 0x284bs -> boolToken <- true; BinaryToken.Bool
    | 0x284cs -> boolToken <- false; BinaryToken.Bool
    | x -> tokenString <- lookupId x; BinaryToken.Token

  /// Returns whether a given token is an EndGroup
  let endGroup = function | BinaryToken.EndGroup -> true | _ -> false

  /// If the given token is not an Equals throw an exception
  let ensureEquals = function
    | BinaryToken.Equals -> ()
    | x -> fail "Expected equals, but got" x

  /// If the given token can't be used as an identifier, throw an exception
  let ensureIdentifier = function
    | BinaryToken.Int -> int32Token.ToString()
    | BinaryToken.Uint -> uint32Token.ToString()
    | BinaryToken.String -> stringToken
    | BinaryToken.Token -> tokenString
    | x -> fail "Expected identifier, but got" x

  /// Advances the stream to the next token and returns the token
  let nextToken () = tok <- parseToken (stream.ReadInt16()); tok

  /// Occasionally there are isolated "{}" without identifiers. I believe these
  /// to be useless, so this function will skip instances.
  let skipEmptyObjects () =
    while (match tok with | BinaryToken.OpenGroup -> true | _ -> false) do
      match (nextToken()) with
      | BinaryToken.EndGroup -> nextToken() |> ignore
      | x ->  fail "Expected empty object, but got" x

  let toPara = function
    | HiddenDate(date) -> ParaValue.Date(date)
    | num -> ParaValue.Number(float(num))

  let rec parseTopObject () =
    let pairs = ResizeArray<_>()
    while stream.BaseStream.Position <> stream.BaseStream.Length do
      let key = nextToken() |> ensureIdentifier
      nextToken() |> ensureEquals
      nextToken() |> ignore
      pairs.Add((key, parseValue()))
      skipEmptyObjects()
    pairs.ToArray()

  and parseObject firstKey =
    let pairs = ResizeArray<_>()

    // Advance reader to the next token
    nextToken() |> ignore
    pairs.Add((firstKey, parseValue()))
    nextToken() |> ignore
    skipEmptyObjects()

    while not (endGroup tok) do
      let key = tok |> ensureIdentifier
      nextToken() |> ensureEquals
      nextToken() |> ignore
      pairs.Add((key, parseValue()))
      nextToken() |> ignore
      skipEmptyObjects()
    pairs.ToArray()

  and parseArray (values:ResizeArray<_>) =
    while not (endGroup tok) do
      values.Add(parseValue())
      nextToken() |> ignore
    values.ToArray()

  /// Transforms current token into a ParaValue
  and parseValue () =
    match tok with
    | BinaryToken.Int -> toPara int32Token
    | BinaryToken.Uint -> ParaValue.Number(float(uint32Token))
    | BinaryToken.Bool -> ParaValue.Bool(boolToken)
    | BinaryToken.String -> ParaValue.String(stringToken)
    | BinaryToken.Float -> ParaValue.Number(floatToken)
    | BinaryToken.OpenGroup -> parseSubgroup()
    | BinaryToken.Token -> ParaValue.String(tokenString)
    | x -> fail "Unexpected token" x

  /// Determines what type of object follows an OpenGroup token -- an array or
  /// object.
  and parseSubgroup () =
    match (nextToken()) with
    | BinaryToken.Uint ->
      let temp = uint32Token
      subber (temp.ToString()) (fun () -> float temp |> ParaValue.Number)
    | BinaryToken.Int ->
      let temp = int32Token
      subber (temp.ToString()) (fun () -> toPara temp)
    | BinaryToken.Float ->
      let temp = floatToken
      nextToken() |> ignore
      Common.parseArrayFirst (ParaValue.Number(temp)) parseArray
    | BinaryToken.String ->
      let temp = stringToken
      subber temp (fun () -> ParaValue.String temp)
    | BinaryToken.OpenGroup ->
      let firstKey = nextToken() |> ensureIdentifier
      nextToken() |> ensureEquals
      let first = ParaValue.Record(parseObject firstKey)
      nextToken() |> ignore
      Common.parseArrayFirst first parseArray
    | BinaryToken.Token ->
      nextToken() |> ensureEquals
      ParaValue.Record(parseObject tokenString)
    | BinaryToken.EndGroup -> ParaValue.Record [| |]
    | x -> fail "Unexpected token" x

  /// It's impossible to know just by reading the first token if we are dealing
  /// with an object or an array. The only way to know is to read the next
  /// token. Only if it is an equals are we dealing with an object.
  and subber (key: string) (paraFn:unit -> ParaValue) : ParaValue =
    match (nextToken()) with
    | BinaryToken.Equals -> ParaValue.Record(parseObject key)
    | BinaryToken.EndGroup -> ParaValue.Array([| paraFn() |])
    | _ ->
      let values = ResizeArray<_>()
      values.Add(paraFn())
      values.Add(parseValue())
      nextToken() |> ignore
      ParaValue.Array(parseArray values)

  member x.Parse (header:option<string>) =
    match header with
    | Some(txt) ->
      let (headerBuffer:char[]) = Array.zeroCreate txt.Length
      stream.Read(headerBuffer, 0, headerBuffer.Length) |> ignore
      if String(headerBuffer) <> txt then
        failwith "Expected header not encountered"
      ParaValue.Record(parseTopObject())
    | None -> ParaValue.Record(parseTopObject())

type ParaResult<'a> = Choice<'a,string>
type ParaValue<'a> = ParaValue -> ParaResult<'a> * ParaValue

[<AutoOpen>]
module ParaResultImpl =
  /// Represents the deserialized value or an error. Adapted from Don Syme's
  /// Github comment on a result type in F#:
  /// https://github.com/fsharp/FSharpLangDesign/issues/49#issuecomment-193795013
  let inline Ok x : ParaResult<'a> = Choice1Of2 x
  let inline Error<'a> x : ParaResult<'a> = Choice2Of2 x
  type Choice<'a, 'b> with
    static member Error<'a> (x:string) : ParaResult<'a> = Error<'a> x
    static member Ok<'a> (x:'a) : ParaResult<'a> = Ok x
  let  (|Ok|Error|) (result: ParaResult<'Ok>) = result

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParaResult =
  /// If the result is ok, extract the value and return the given function
  /// applied with the value, else continue propogating error 
  let bind (fn: 'a -> ParaResult<'b>) (m: ParaResult<'a>) : ParaResult<'b> =
    match m with
    | Ok(x) -> fn x
    | Error(x) -> Error(x)

  /// If the result is ok, extract the value and apply the given function
  /// on the value and wrap it in an result, else continue propogating error
  let map (f: 'a -> 'b) (m: ParaResult<'a>) : ParaResult<'b> =
    bind (f >> Ok) m 

  /// Get the underlying value or throw an exception
  let get = function
  | Ok(x) -> x
  | Error(x) -> failwith x

  /// If the option is present, return inner value applied to the function.
  /// else return a default value wrapped in a result.
  let defaultOpt (fn:ParaValue -> ParaResult<'a>) (def:'a) (opt:ParaValue option) =
    match Option.map fn opt with
    | Some(res) -> res
    | None -> Ok def

  /// Apply a function that returns a result to each element of an array.
  /// Aggregate the results into a vector and return the resulting vector
  /// or the error that occurred on one of the elements in the array
  let inline paraFold fn arr : ParaResult<ResizeArray<'a>> =
    let addElem (vec:List<'a>) elem = vec.Add(elem); Ok(vec)

    Array.map fn arr
    |> Array.fold(fun state x ->
      state |> bind (fun vec -> bind (addElem vec) x))
      (Ok (ResizeArray<'a>()))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParaValue =
  /// Extracts inner array or reports an error
  let asArray = function
  | ParaValue.Array arr -> Ok arr
  | x -> Error(sprintf "Expected array but received %O" x)

  /// Extracts inner key value array or reports an error
  let asRecord = function
  | ParaValue.Record props -> Ok props
  | x -> Error(sprintf "Expected record but received %O" x)

  /// Extracts inner boolean or reports an error. A boolean value is sometimes
  /// written as a number, so if a number is non-zero, it is considered true
  /// (think C)
  let asBool = function
  | ParaValue.Bool b -> Ok b
  | ParaValue.Number n -> Ok((int n) <> 0)
  | x -> Error(sprintf "Expected boolean but received %O" x)

  /// Extracts inner number or reports an error
  let asNumber = function
  | ParaValue.Number x -> Ok x 
  | x -> Error(sprintf "Expected number but received %O" x)

  /// Extracts the inner number and truncates any possible decimals
  let asInteger = asNumber >> ParaResult.map int

  /// Extracts inner string or reports an error
  let asString = function
  | ParaValue.String s -> Ok s
  | x -> Error(sprintf "Expected string but received %O" x)

  /// Extracts inner date or reports an error
  let asDate = function
  | ParaValue.Date s -> Ok s
  | x -> Error(sprintf "Expected date but received %O" x)

  /// Extracts inner hsv or reports an error
  let asHsv = function
  | ParaValue.Hsv (h, s, v) -> Ok (h, s, v)
  | x -> Error(sprintf "Expected hsv but received %O" x)

  /// Extracts inner rgb or reports an error
  let asRgb = function
  | ParaValue.Rgb (r, g, b) -> Ok (r, g, b)
  | x -> Error(sprintf "Expected rgb but received %O" x)

  /// When given an array/record that contains only a single instance. Run the
  /// deserialization function against that single element.
  let unfold fn = function
  | ParaValue.Array [| x |] -> fn x
  | ParaValue.Record [| key, value |] -> fn value
  | x -> fn x
  
  /// Assumes the passed in value is a record and attempts to search
  /// the record's properties for the given key. If a single value exists
  /// return the value else there is an error
  let get (key:string) (o:ParaValue) : ParaResult<ParaValue> =
    asRecord o
    |> ParaResult.bind (
      fun props ->
        match Array.filter (fst >> (=) key) props with
        | [| key, value |] -> Ok value
        | x -> Error(sprintf "Found not 1 but %d of %s" (Array.length x) key))

  /// Assumes the passed in value is a record and collects all properties
  /// with a given key.
  let getAll (key:string) (o:ParaValue) : ParaResult<ParaValue[]> =
    asRecord o
    |> ParaResult.map (Array.filter (fst >> (=) key) >> Array.map snd)

  /// Assumes the passed in value is a record and attempts to search
  /// the record's properties for the given key. If a single value exists
  /// return some value, if no properties were found, return None
  let tryGet (key:string) (o:ParaValue) : ParaResult<ParaValue option> =
    asRecord o
    |> ParaResult.bind (
      fun props ->
        match Array.filter (fst >> (=) key) props with
        | [| key, value |] -> Ok (Some value)
        | [| |] -> Ok None
        | x -> Error(sprintf "Found not 1 but %d of %s" (Array.length x) key))

  /// Return a new ParaValue resulting from applying the given function
  /// to each value in the data. Adapted from json4s.
  let map (fn:ParaValue -> ParaValue) (obj:ParaValue) : ParaValue =
    match obj with
    | ParaValue.Record props ->
      props |> Array.map (fun (k, v) -> (k, fn v)) |> ParaValue.Record
    | ParaValue.Array arr -> arr |> Array.map fn |> ParaValue.Array
    | x -> fn x

  /// Run each element of the given array through the function and if all elements
  /// result in ok, then reduce the array of results into a single result of an
  /// array. If an element is an error, the first error is returned.
  let inline reduce (fn:ParaValue -> ParaResult<'a>) (arr:ParaValue[]) : ParaResult<'a[]> =
    let res = ParaResult.paraFold fn arr
    ParaResult.map (fun (x: List<'a>) -> x.ToArray()) res

  /// Given a function to map a value to a result, apply this function on every
  /// value of a record, or array, or any other element (and the result would be
  /// an a single element array)
  let inline flatMap (fn:ParaValue -> ParaResult<'a>) (o:ParaValue) : ParaResult<'a[]> =
    match o with
    | ParaValue.Array arr -> reduce fn arr
    | ParaValue.Record props -> props |> Array.map snd |> reduce fn
    | x -> fn x |> ParaResult.map (fun y -> [| y |])

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

module ApplicativeParaValue =

  /// Wraps a result into a ParaValue<'a>
  let inline ofResult (result: ParaResult<'a>) : ParaValue<'a> =
    fun para -> result, para

  /// Creates an error from a given string
  let inline error (e: string) : ParaValue<'a> = ofResult(Error e)

  /// Creates a ParaValue<'a> from a value
  let inline init (a: 'a) : ParaValue<'a> = ofResult(Ok a)

  /// If a given result has a value, apply another function to the value
  let inline bind (m: ParaValue<'a>) (f: 'a -> ParaValue<'b>) : ParaValue<'b> =
    fun paravalue ->
      match m paravalue with
      | Ok a, paravalue -> (f a) paravalue
      | Error e, paravalue -> Error e, paravalue

  /// If the wrapped function is a success and the given result is a success
  /// the function is applied on the value. Otherwise the exisiting error
  /// messages are propagated.
  let inline apply (f: ParaValue<'a -> 'b>) (m: ParaValue<'a>) : ParaValue<'b> =
    bind f (fun f' ->
      bind m (f' >> init))

  /// Maps the underlying value. If the result is currently an error
  /// the map does not happen
  let inline map (f: 'a -> 'b) (m: ParaValue<'a>) : ParaValue<'b> =
    bind m (f >> init)

  /// Given a function that given a ParaValue will return a ParaResult, wrap
  /// it into a ParaValue<'a>
  let wrap (fn:ParaValue -> ParaResult<'a>) =
    fun paravalue -> fn paravalue, paravalue

[<AutoOpen>]
module Functional =
  open ParaValue
  open ApplicativeParaValue

  type FromParaDefaults = FromParaDefaults with
    static member inline FromPara (_: bool)  = wrap (unfold asBool)
    static member inline FromPara (_: int) = map int (wrap (unfold asNumber))
    static member inline FromPara (_: uint32) = map uint32 (wrap (unfold asNumber))
    static member inline FromPara (_: sbyte) = map sbyte (wrap (unfold asNumber))
    static member inline FromPara (_: byte) = map byte (wrap (unfold asNumber))
    static member inline FromPara (_: int16) = map int16 (wrap (unfold asNumber))
    static member inline FromPara (_: uint16) = map uint16 (wrap (unfold asNumber))
    static member inline FromPara (_: int64) = map int64 (wrap (unfold asNumber))
    static member inline FromPara (_: uint64) = map uint64 (wrap (unfold asNumber))
    static member inline FromPara (_: single) = map single (wrap (unfold asNumber))
    static member inline FromPara (_: float) = map float (wrap (unfold asNumber))
    static member inline FromPara (_: string) = wrap (unfold asString)

  let inline internal fromParaDefaults (a: ^a, _: ^b) =
    ((^a or ^b) : (static member FromPara: ^a -> ^a ParaValue) a)

  let inline fromPara x =
    fst (fromParaDefaults (Unchecked.defaultof<'a>, FromParaDefaults) x)

  let inline lister fn =
    map fn (wrap (ParaValue.asArray >> ParaResult.bind (ParaResult.paraFold fromPara)))

  type FromParaDefaults with
    static member inline FromPara (_: 'a option) = map Some (fun b -> fromPara b, b)
    static member inline FromPara (_: 'a[]) : ParaValue<'a[]> =
      lister (fun x -> x.ToArray())
    static member inline FromPara (_: 'a list) : ParaValue<'a list> =
      lister List.ofSeq

  /// Deserialize data into a type implementing `FromPara`. Throws an
  /// exception on failure
  let inline deserialize paraValue = ParaResult.get (fromPara paraValue)

  /// Extract and deserialize the value with a given key
  let inline pget (key:string) (o:ParaValue) : ParaResult<'a> =
    ParaValue.get key o |> ParaResult.bind fromPara

  /// Extract and deserialize all the values matching a given key
  /// into a result
  let inline pgetAll (key:string) (o:ParaValue) =
   let mfold (state:ParaResult<'a list>) (x:ParaResult<'a>) =
     state |> ParaResult.bind (fun lst -> ParaResult.map (fun y -> y :: lst) x)

   ParaValue.getAll key o
   |> ParaResult.bind (Array.map fromPara >>
     Array.fold mfold (Ok []) >>
     ParaResult.map (List.rev >> List.toArray))

  /// Extract and deserialize the value with a given key if the key is present
  let inline tryPget (key:string) (o:ParaValue) : ParaResult<'a option> =
    ParaValue.tryGet key o
    |> ParaResult.bind (function
      | Some(y) -> fromPara y
      | None -> Ok None)

[<AutoOpen>]
module ParaBuilder =
  open ParaResult
  type ParaBuilder () =
    member __.Bind (m1, m2) : ParaResult<_> = bind m2 m1
    member __.Combine (m1, m2) : ParaResult<_> = bind (fun () -> m2) m1
    member __.Delay (f) : ParaResult<_> = bind f (Ok ())
    member __.Return (x) : ParaResult<_> = Ok x
    member __.ReturnFrom (f) : ParaResult<_> = f
    member __.Zero () : ParaResult<_> = Ok ()
    member __.TryWith (body, handler) =
      try
        body()
      with
      | e -> handler e
    member __.TryFinally (body, compensation) =
      try
        body()
      finally
        compensation()
    member x.Using(d:#IDisposable, body) =
      let result = fun () -> body d
      x.TryFinally (result, fun () ->
        match d with
        | null -> ()
        | d -> d.Dispose())
    member x.While (guard, body) =
      if not <| guard () then
        x.Zero()
      else
        bind (fun () -> x.While(guard, body)) (body())
    member x.For(s:seq<_>, body) =
      x.Using(s.GetEnumerator(), fun enum ->
        x.While(enum.MoveNext, fun () -> body enum.Current))

  let para = ParaBuilder ()

type ParaValue with
  /// Parses the given stream assuming that it contains strictly plain text.
  static member LoadText (stream:Stream) =
    let parser = ParaParser (PeekingStream(stream))
    parser.Parse ()

  /// Parses the given stream filled with binary data with a token lookup and
  /// header
  static member LoadBinary (stream:Stream, lookup:IDictionary<int16, string>, header:option<string>) =
    use stream = new BinaryReader(stream, encoding)
    let parser = BinaryParaParser(stream, lookup)
    parser.Parse (header)

  /// Parses zip files or uncompressed files that can be plain text or binary
  /// encoded
  static member Load (file:string, binHeader:string, txtHeader:string, lookup:Lazy<IDictionary<int16, string>>) =
    use fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 0x8000)
    if (fs.ReadByte()) = 0x50 && (fs.ReadByte()) = 0x4b then
      use zf = new ZipFile(file)
      zf
      |> Seq.filter (fun x -> Path.GetExtension(x.FileName) <> "")
      |> Seq.exactlyOne
      |> fun x -> ParaValue.Load(x.OpenReader(), binHeader, txtHeader, lookup)
    else
      fs.Seek(0L, SeekOrigin.Begin) |> ignore
      ParaValue.Load(fs, binHeader, txtHeader, lookup)

  /// Given a stream that may be textually or binary encoded, the function
  /// parses it correctly when given the headers to look out for.
  static member Load (stream:Stream, binHeader:string, txtHeader:string, lookup:Lazy<IDictionary<int16, string>>) =
    if binHeader.Length <> txtHeader.Length then
      failwith "Headers should be the same length"
    let bt = Array.zeroCreate binHeader.Length
    let length = stream.Read(bt, 0, binHeader.Length)
    let header = encoding.GetString(bt, 0, length)
    match header with
    | x when x = binHeader -> ParaValue.LoadBinary(stream, (lookup.Force()), None)
    | x when x = txtHeader -> ParaValue.LoadText(stream)
    | x -> failwithf "Unexpected header: %s" x

  /// Parses the given file path assuming that it contains strictly plain text.
  static member LoadText (file:string) =
    use fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 0x8000)
    ParaValue.LoadText fs

  /// Parses the given string
  static member Parse (text:string) =
    let str = new MemoryStream(encoding.GetBytes(text))
    ParaValue.LoadText str

  /// Writes the given data to a stream
  static member Save (stream:Stream, data:ParaValue) =
    use stream = new StreamWriter(stream, encoding, 0x8000)

    let rec recWrite props =
      props |> Array.iter (fun ((prop:string), v) ->
        stream.Write(prop)
        stream.Write('=')
        write v)
    and write = function
      | ParaValue.Bool b -> stream.WriteLine(if b then "yes" else "no")
      | ParaValue.Date d -> stream.WriteLine(d.ToString("yyyy.M.d"))
      | ParaValue.Number n -> stream.WriteLine(n.ToString("0.000"))
      | ParaValue.String s -> stream.WriteLine("\"" + s + "\"")
      | ParaValue.Hsv(h, s, v) -> stream.WriteLine(sprintf "hsv { %.3f %.3f %.3f }" h s v)
      | ParaValue.Rgb(r, g, b) -> stream.WriteLine(sprintf "rgb { %i %i %i }" r g b)
      | ParaValue.Array a ->
        stream.Write('{')
        Array.iter write a
        stream.Write('}')
      | ParaValue.Record r ->
        stream.Write('{')
        recWrite r
        stream.Write('}')

    match data with
    | ParaValue.Record props -> recWrite props
    | _ -> failwith "Can't save a non-record"
