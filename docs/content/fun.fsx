(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

#r "Pfarah.dll"
open Pfarah
open Pfarah.Operators
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = ParaValue.LoadText("../../src/Pfarah.Benchmarks/achievements.txt")
let trans obj = obj / "possible" / "ironman"
let ironmen = fmap trans data
let requireIroman : (string * bool)[] =
  match ironmen with
  | ParaValue.Record props ->
    props |> Array.map (fun (k, v) -> (k, deserialize v))
  | x -> failwith "Unexpected"

let tags : string[] = deserialize (data /./ "possible" / "tag")
tags
|> Seq.groupBy id
|> Seq.map (fun (k, grp) -> (k, Seq.length grp))
|> Seq.sortByDescending snd
|> Seq.iter (fun (tag, length) -> printfn "%s: %d" tag length)