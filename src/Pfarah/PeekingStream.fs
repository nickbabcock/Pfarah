namespace Pfarah

open System.IO

/// A PeekingStream is a simpler wrapper around a Stream object that allows
/// for efficient peeking. The peeking mechanism is somewhat similar to a
/// StreamReader, except that a PeekingStream keeps the next byte cached so
/// that repeating calls to Peek only result in simple return instead of an
/// array lookup. This class was inspired by RapidJson's [FileReadStream]
/// (https://github.com/miloyip/rapidjson/blob/b39a8982699aed269a8db2c0d1319bd7ab04485a/include/rapidjson/filereadstream.h)
type internal PeekingStream (stream:Stream) =
  let mutable current = 0;
  let mutable eof = false
  let (buf:byte[]) = Array.zeroCreate 0x8000
  let mutable readLen = 0
  let mutable readPos = 0

  /// Reads a single byte from the stream, returning -1 if no more bytes
  /// are available. 10% improvement to parsing speed when implemented
  /// rather than using `stream.ReadByte` (5be9cd4)
  let readByte () : int =
    if readPos < readLen then
      readPos <- readPos + 1
      int buf.[readPos]
    else if not eof then
      readLen <- stream.Read(buf, 0, 0x8000) - 1
      readPos <- 0
      if readLen < 0x8000 - 1 then
        eof <- true
      int buf.[readPos]
    else -1

  let read () =
    let c = current
    current <- readByte()
    c

  do read() |> ignore

  member __.Peek() = current
  member __.Read() = read()
  member __.EndOfStream = eof && readPos = readLen
