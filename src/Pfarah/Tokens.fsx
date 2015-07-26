#I "../../bin"
#r "Pfarah.dll"
open Pfarah
open System
open System.IO

let txt, bin =
  match fsi.CommandLineArgs with
  | [| txt; bin |] -> txt, bin
  | x -> failwith "<text file> <binary file>"

let txtData = ParaValue.Load(txt, "EU4bin", "EU4txt", lazy(dict([])))
let binData = ParaValue.Load(bin, "EU4bin", "EU4txt", lazy(dict([])))

let printMappings = Map.iter(fun name token -> printfn "0x%04x, %s" (Int32.Parse(token)) name)
let extractKeys x y =
  Seq.zip (x |> asRecord) (y |> asRecord)
  |> Seq.filter (fun ((xkey, xval), (ykey, yval)) -> xkey <> ykey)
  |> Seq.map (fun ((xkey, xval), (ykey, yval)) -> xkey, ykey)

let topMap = extractKeys txtData binData |> Map.ofSeq

let binTrade =
  binData
  |> tryFind topMap.["trade"]
  |> Option.get
 
let tradeMap = extractKeys txtData?trade binTrade |> Map.ofSeq
let binNodes = binTrade |> collect tradeMap.["node"]

Seq.zip (txtData?trade |> collect "node") (binNodes)
|> Seq.collect (fun (x, y) -> extractKeys x y)
|> Map.ofSeq
|> printMappings

Seq.zip (txtData?trade |> collect "node") (binNodes)
|> Seq.collect (fun (x, y) ->
  Seq.zip (x |> asRecord) (y |> asRecord)
  |> Seq.filter (fun ((xkey, xval), (ykey, yval)) -> xkey = ykey)
  |> Seq.map (fun ((xkey, xval), (ykey, yval)) -> xval, yval))
|> Seq.collect (fun (x, y) -> extractKeys x y)
|> Map.ofSeq
|> printMappings

topMap |> printMappings