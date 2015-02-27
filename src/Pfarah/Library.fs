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

type PfData =
  | Pfbool of bool
  | Pfnumber of float
  | Pfdate of DateTime
  | Pfstring of string
  | Pflist of PfData list
  | PfObj of Map<string, PfData>

let isspace (c:int) = c = 10 || c = 13 || c = 9 || c = 32

let skipWhitespace (stream:StreamReader) =
  while (isspace (stream.Peek())) do
    stream.Read() |> ignore

let parse file () =
  use fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 0x8000)
  use stream = new StreamReader(fs, Encoding.GetEncoding(1252), false, 0x8000)
  skipWhitespace stream
  fs
