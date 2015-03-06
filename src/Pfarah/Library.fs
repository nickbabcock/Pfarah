/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Pfarah

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


let isspace (c:int) = c = 10 || c = 13 || c = 9 || c = 32

let skipWhitespace (stream:StreamReader) =
  while (isspace (stream.Peek())) do
    stream.Read() |> ignore

let tryDate (str:string) =
  match str.Split('.') with
  | [|y;m;d|] -> Some(new DateTime(int y, int m, int d))
  | [|y;m;d;h|] -> Some(new DateTime(int y, int m, int d, int h, 0, 0))
  | _ -> None

let isnum (c:char) =
  c >= '0' && c <= '9'

type ParaParser (stream:StreamReader) =
  let MaxTokenSize = 256
  let (stringBuffer:char[]) = Array.zeroCreate MaxTokenSize
  let mutable stringBufferCount = 0

  member self.readString () =
    let mutable isDone = false
    while isDone <> true do
      let next = stream.Peek()
      isDone <- isspace next || next = 61 || next = -1 || next = 125
      if not (isDone) then
        stringBuffer.[stringBufferCount] <- (char (stream.Read()))
        stringBufferCount <- stringBufferCount + 1

    let result = new String(stringBuffer, 0, stringBufferCount)
    stringBufferCount <- 0
    result

  member self.quotedStringRead() =
    while stream.Peek() <> 34 do
      stringBuffer.[stringBufferCount] <- (char (stream.Read()))
      stringBufferCount <- stringBufferCount + 1

    // Read the trailing quote
    stream.Read() |> ignore
    let result = new String(stringBuffer, 0, stringBufferCount)
    stringBufferCount <- 0
    result

  member self.parseArray (vals:ResizeArray<_>) =
    while (stream.Peek() <> 125) do
      skipWhitespace stream
      vals.Add(self.parseValue())
      skipWhitespace stream

  member self.ParseContainerContents() =
    let first = self.readString()
    skipWhitespace stream

    match (stream.Peek()) with
    | 125 -> // List of one
      let vals = ResizeArray<_>()
      vals.Add(ParaValue.String first)
      ParaValue.Array (vals |> Seq.toArray)
    | 61 -> self.parseObject first
    | _ -> // parse list
      skipWhitespace stream
      let vals = ResizeArray<_>()
      vals.Add(ParaValue.String first)
      self.parseArray vals
      ParaValue.Array (vals |> Seq.toArray)

  member self.ParseContainer () =
    skipWhitespace stream
    match (stream.Peek()) with
    | 125 -> ParaValue.Record ([||])
    | 34 ->
      let vals = ResizeArray<_>()
      self.parseArray vals
      ParaValue.Array (vals |> Seq.toArray)    
    | _ -> self.ParseContainerContents()

  member self.parseObject key =
    // Read through the '='
    stream.Read() |> ignore
    let pairs = ResizeArray<_>()
    pairs.Add((key, self.parseValue()))
    skipWhitespace stream
    while stream.Peek() <> 125 do
      pairs.Add(self.parsePair())
      skipWhitespace stream
    ParaValue.Record (pairs |> Seq.toArray)

  member self.ParseQuotes () =
    // Read through the quote
    stream.Read() |> ignore

    let q = self.quotedStringRead()
    match tryDate q with
    | Some(date) -> ParaValue.Date date
    | None -> ParaValue.String q

  member self.parseValue () =
    match stream.Peek() with
    | 34 -> self.ParseQuotes()
    | 123 -> 
      // Read through '{'
      stream.Read() |> ignore
      let result = self.ParseContainer()

      // Read through '}'
      assert (stream.Peek() = 125)
      stream.Read() |> ignore
      result
    | _ -> ParaValue.String (self.readString())

  member self.parsePair () =
    skipWhitespace stream
    let key = self.readString()
    skipWhitespace stream
    assert (stream.Peek() = 61)
    stream.Read() |> ignore
    skipWhitespace stream
    let result = key, self.parseValue()
    skipWhitespace stream
    result

  member self.Parse () = 
    let pairs = ResizeArray<_>()
    while (not stream.EndOfStream) do
      pairs.Add(self.parsePair())
    pairs |> Seq.toArray

let parse (stream:Stream) =
  use stream = new StreamReader(stream, Encoding.GetEncoding(1252), false, 0x8000)
  let parser = ParaParser stream
  parser.Parse ()

let parseFile file =
  use fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 0x8000)
  parse fs