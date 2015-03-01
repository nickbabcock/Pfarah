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

type PfData =
  | Pfbool of bool
  | Pfnumber of float
  | Pfdate of DateTime
  | Pfstring of string
  | Pflist of List<PfData>
  | PfObj of Dictionary<string, PfData>


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
  let mutable obj = new Dictionary<string, PfData>()

  member self.readString () =
    let mutable isDone = false
    while isDone <> true do
      let next = stream.Peek()
      isDone <- isspace next || next = 61 || next = -1
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
    let result = new String(stringBuffer, 0, stringBufferCount)
    stringBufferCount <- 0
    result

  member self.ParseContainer () =
    let first = self.readString()
    skipWhitespace stream
    

    match (stream.Peek()) with
    | 125 -> // List of one
      let mutable li = new List<PfData>()
      li.Add(Pfstring first)
      Pflist li
    | 61 -> // we're parsing an object
      PfObj (new Dictionary<string, PfData>())
    | _ -> // parse list
      Pflist (new List<PfData>())

  member self.Parse () =
    skipWhitespace stream
    let key = self.readString ()
    skipWhitespace stream

    assert (stream.Peek() = 61)
    stream.Read() |> ignore
    skipWhitespace stream
    let value =
      match stream.Peek() with
      | 34 ->
        // Read through the quote
        stream.Read() |> ignore

        let q = self.quotedStringRead()
        match tryDate q with
        | Some(date) -> Pfdate date
        | None -> Pfstring q
      | 123 ->
        skipWhitespace stream

        match stream.Peek() with
        | 125 -> // Empty object
          stream.Read() |> ignore
          PfObj (new Dictionary<string, PfData>(0))
        | 34 -> // Quoted date/string list
          Pfstring ""
        | _ -> self.ParseContainer()
      | _ ->
        Pfstring (self.readString())
    obj.Add(key, value)

  member self.Obj () = PfObj obj

let parse (stream:Stream) =
  use stream = new StreamReader(stream, Encoding.GetEncoding(1252), false, 0x8000)
  let parser = ParaParser stream
  parser.Parse ()
  parser.Obj ()  

let parseFile file =
  use fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 0x8000)
  parse fs