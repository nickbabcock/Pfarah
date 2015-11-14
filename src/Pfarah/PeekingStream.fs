namespace Pfarah

open System.IO

type PeekingStream(stream:Stream) =
  let mutable current = 0;
  let mutable eof = false

  let read () =
    let c = current
    if not eof then
      current <- stream.ReadByte()
      eof <- current = -1
    c

  do read() |> ignore

  member __.Peek() = current
  member __.Read() = read()
  member __.EndOfStream = eof

