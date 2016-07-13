namespace Pfarah

open System

module Tokens =
  let isTokenKey xkey ykey =
    xkey <> ykey && Int32.TryParse(ykey) |> fst |> (=) true

  let extractKeys x y =
    let newx = ParaValue.asRecord x |> ParaResult.get
    let newy = ParaValue.asRecord y |> ParaResult.get
    Seq.zip newx newy
    |> Seq.filter (fun ((xkey, xval), (ykey, yval)) -> isTokenKey xkey ykey)
    |> Seq.map (fun ((xkey, xval), (ykey, yval)) -> xkey, ykey)

  let rec deduceInner txt bin =
    let subkeys =
      let newx = ParaValue.asRecord txt |> ParaResult.get
      let newy = ParaValue.asRecord bin |> ParaResult.get
      Seq.zip newx newy
      |> Seq.choose(fun ((xkey, xval), (ykey, yval)) ->
        match xval, yval with
        | ParaValue.Record x, ParaValue.Record y -> Some(xval, yval)
        | _ -> None)
      |> Seq.collect (fun (x, y) -> deduceInner x y)

    Seq.append subkeys (extractKeys txt bin)

  let deduce txt bin = deduceInner txt bin |> dict
