namespace Pfarah

open System.IO

/// A PeekingStream is a simpler wrapper around a Stream object that allows
/// for efficient peeking. The peeking mechanism is somewhat similar to a
/// StreamReader, except that a PeekingStream keeps the next byte cached so
/// that repeating calls to Peek only result in simple return instead of an
/// array lookup. This class was inspired by RapidJson's [FileReadStream]
/// (https://github.com/miloyip/rapidjson/blob/b39a8982699aed269a8db2c0d1319bd7ab04485a/include/rapidjson/filereadstream.h)
type PeekingStream (stream:Stream) =
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

