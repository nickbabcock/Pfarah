#I "../../bin"
#r "Pfarah.dll"
open Pfarah
open System
open System.Diagnostics;

let startmem = Process.GetCurrentProcess().WorkingSet64

let filename = @"C:\temp\1.9observer_uncompressed.eu4"
let data = ParaValue.LoadText(filename)

GC.Collect()
let endmem = Process.GetCurrentProcess().WorkingSet64

let rec extractString = function
  | ParaValue.String(x) -> Some(Seq.singleton(x))
  | ParaValue.Record(x) ->
    let keys = x |> Seq.map fst
    let subkeys =
      x
      |> Seq.map (snd >> extractString)
      |> Seq.choose id
      |> Seq.collect id
    Some(Seq.append keys subkeys)
  | ParaValue.Array(x) -> Some(x |> Seq.map extractString |> Seq.choose id |> Seq.collect id)
  | _ -> None

let stringMemUsage (key:string, cnt:int) = (20 + 2 * key.Length) * (cnt - 1)
let results = data |> extractString |> Option.get |> Seq.countBy id |> Seq.sortBy (stringMemUsage >> (~-))
let savings = results |> Seq.map stringMemUsage |> Seq.sum
printfn "The top ten most memory expensive strings in the file"
results |> Seq.take 10 |> Seq.iter (fun (key, cnt) -> printfn "%s: %d" key cnt)

printfn ""
printfn "String interning will save: %d bytes" savings
printfn "Memory usage: before: %d after: %d" (endmem - startmem) (endmem - startmem - int64(savings))
