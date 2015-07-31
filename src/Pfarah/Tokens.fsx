#I "../../bin"
#r "Pfarah.dll"
open Pfarah
open Tokens
open System
open System.IO
open System.Collections.Generic

let txt, bin =
  match fsi.CommandLineArgs with
  | [| txt; bin |] -> txt, bin
  | x -> failwith "<text file> <binary file>"

let txtData = ParaValue.Load(txt, "EU4bin", "EU4txt", lazy(dict([])))
let binData = ParaValue.Load(bin, "EU4bin", "EU4txt", lazy(dict([])))

let printMappings (entry:KeyValuePair<string,string>) =
  printfn "0x%04x, %s" (Int32.Parse(entry.Value)) entry.Key

let d = Tokens.deduce txtData binData
Seq.iter printMappings d
