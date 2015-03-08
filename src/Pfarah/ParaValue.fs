/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
namespace Pfarah

open System
open System.IO
open System.Text
open System.Collections.Generic

[<RequireQualifiedAccess>]
type ParaValue =
  | Bool of bool
  | Number of float
  | Date of DateTime
  | String of string
  | Array of elements:ParaValue[]
  | Record of properties:(string * ParaValue)[]

type private ParaParser (stream:StreamReader) =
  /// The max token size of any string, as defined by paradox internal source code is 256
  let (stringBuffer:char[]) = Array.zeroCreate 256
  let mutable stringBufferCount = 0

  // Helper functions
  let isspace (c:int) = c = 10 || c = 13 || c = 9 || c = 32

  /// skippy
  let skipWhitespace (stream:StreamReader) =
    while (isspace (stream.Peek())) do
      stream.Read() |> ignore

  let tryDate (str:string) =
    match str.Split('.') with
    | [|y;m;d|] -> Some(new DateTime(int y, int m, int d))
    | [|y;m;d;h|] -> Some(new DateTime(int y, int m, int d, int h, 0, 0))
    | _ -> None

  let narrow str =
    match str with
    | "yes" -> ParaValue.Bool true
    | "no" -> ParaValue.Bool false
    | _ ->
      match Double.TryParse str with
      | (true, x) -> ParaValue.Number x
      | (false, _) ->
        match tryDate str with
        | Some(date) -> ParaValue.Date date
        | None -> ParaValue.String str

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
    | _ -> narrow (readString())

  and readString () =
    let mutable isDone = false
    while not isDone do
      let next = stream.Peek()
      isDone <- isspace next || next = 61 || next = -1 || next = 125
      if not (isDone) then
        stringBuffer.[stringBufferCount] <- (char (stream.Read()))
        stringBufferCount <- stringBufferCount + 1

    let result = new String(stringBuffer, 0, stringBufferCount)
    stringBufferCount <- 0
    result

  and quotedStringRead() =
    while stream.Peek() <> 34 do
      stringBuffer.[stringBufferCount] <- (char (stream.Read()))
      stringBufferCount <- stringBufferCount + 1

    // Read the trailing quote
    stream.Read() |> ignore
    let result = new String(stringBuffer, 0, stringBufferCount)
    stringBufferCount <- 0
    result

  and parseArray (vals:ResizeArray<_>) =
    while (stream.Peek() <> 125) do
      skipWhitespace stream
      vals.Add(parseValue())
      skipWhitespace stream

  and parseContainerContents() =
    let first = readString()
    skipWhitespace stream

    match (stream.Peek()) with
    | 125 -> // List of one
      let vals = ResizeArray<_>()
      vals.Add(narrow first)
      ParaValue.Array (vals |> Seq.toArray)
    | 61 -> parseObject first
    | _ -> // parse list
      skipWhitespace stream
      let vals = ResizeArray<_>()
      vals.Add(narrow first)
      parseArray vals
      ParaValue.Array (vals |> Seq.toArray)

  and parseContainer () =
    skipWhitespace stream
    match (stream.Peek()) with
    | 125 -> ParaValue.Record ([||])
    | 123 | 34 ->
      let vals = ResizeArray<_>()
      parseArray vals
      ParaValue.Array (vals |> Seq.toArray)
    | _ -> parseContainerContents()

  and parseObject key =
    // Read through the '='
    stream.Read() |> ignore
    let pairs = ResizeArray<_>()
    pairs.Add((key, parseValue()))
    skipWhitespace stream
    while stream.Peek() <> 125 do
      pairs.Add(parsePair())
      skipWhitespace stream
    ParaValue.Record (pairs |> Seq.toArray)

  and parseQuotes () =
    // Read through the quote
    stream.Read() |> ignore

    let q = quotedStringRead()
    match tryDate q with
    | Some(date) -> ParaValue.Date date
    | None -> ParaValue.String q

  and parsePair () =
    skipWhitespace stream
    if (stream.Peek()) = 123 then
      while (stream.Read()) <> 125 do
        ()
      parsePair()
    else
      let key = readString()
      skipWhitespace stream
      assert (stream.Peek() = 61)
      stream.Read() |> ignore
      skipWhitespace stream
      let result = key, parseValue()
      skipWhitespace stream
      result

  member x.Parse () = 
    let pairs = ResizeArray<_>()
    while (not stream.EndOfStream) do
      pairs.Add(parsePair())
    ParaValue.Record (pairs |> Seq.toArray)

type ParaValue with
  /// Parses the given stream
  static member Load (stream:Stream) =
    use stream = new StreamReader(stream, Encoding.GetEncoding(1252), false, 0x8000)
    let parser = ParaParser stream
    parser.Parse ()

  /// Parses the given file path, allowing other processes to read and write at the same time
  static member Load file =
    use fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 0x8000)
    ParaValue.Load fs

  /// Parses the given string
  static member Parse (text:string) =
    let str = new MemoryStream(Encoding.GetEncoding(1252).GetBytes(text))
    ParaValue.Load str