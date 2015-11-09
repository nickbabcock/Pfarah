namespace Pfarah

open System

module Utils =
  let inline private isNum (c:char) = c >= '0' && c <= '9'
  let inline private toNum (c:char) = int c - 48

  /// Converts an integer that represents a Q16.16 fixed-point number into its
  /// floating point representation. See:
  /// https://en.wikipedia.org/wiki/Q_(number_format)
  let inline cut (n:int) =
    Math.Floor(((float(n) * 2.0) / 65536.0) * 100000.0) / 100000.0

  /// Converts the smaller precision binary float from the integer it is stored
  /// as to a float. It makes sense to be encoded this way as the numbers are
  /// textually represented with three decimal points (#.000)
  let inline cut32 (n:int) = float(n) / 1000.0

  /// A highly optimized version of Double.TryParse that takes advantage of
  /// the fact that we know that the number format is (in regex form):
  /// <(\d+)\.(\d{3})?>. In profiling it was shown that Double.TryParse was a
  /// bottleneck at around 20-50% of the CPU time and after this function was
  /// written, the bottleneck completely disappeared.
  let tryDoubleParse (str:string) =
    let mutable whole = 0
    let mutable i = 0
    let mutable isDone = str.Length = 0
    let mutable sign = 1.0
    let mutable result = None

    // If the first character is a dash, then we know that the number is
    // negative
    if not isDone && str.Chars(i) = '-' then
      sign <- -1.0
      i <- i + 1
      isDone <- i = str.Length

    while not isDone do
      if isNum (str.Chars(i)) then
        whole <- (whole * 10) + toNum (str.Chars(i))
        i <- i + 1
        isDone <- i = str.Length
        if isDone then
          result <- Some (float whole * sign)

      // If the encountered character is a dot, then we know that the next
      // three characters represent the fractional part of the number. If the
      // next three characters aren't numbers then we aren't looking at a
      // number
      else if (str.Chars(i)) = '.' then
        if (i + 3 = str.Length - 1) && isNum (str.Chars(i + 1))
          && isNum(str.Chars(i + 2)) && isNum(str.Chars(i + 3)) then
          let fraction =
           float (toNum (str.Chars(i + 1)) * 100 +
                  toNum (str.Chars(i + 2)) * 10 +
                  toNum (str.Chars(i + 3))) / 1000.0
          result <- Some((float whole + fraction) * sign)
        else if (i + 5 = str.Length - 1) && isNum (str.Chars(i + 1))
              && isNum(str.Chars(i + 2)) && isNum(str.Chars(i + 3))
              && isNum(str.Chars(i + 4)) && isNum(str.Chars(i + 5)) then
          let fraction =
           float (toNum (str.Chars(i + 1)) * 10000 +
                  toNum (str.Chars(i + 2)) * 1000 +
                  toNum (str.Chars(i + 3)) * 100 +
                  toNum (str.Chars(i + 4)) * 10 +
                  toNum (str.Chars(i + 5))) / 100000.0
          result <- Some((float whole + fraction) * sign)
        isDone <- true
      else
        isDone <- true

    result

  /// Returns some datetime if the parameters can be used to create
  /// a valid date
  let private tryDate year month day hour =
    if year > 0 && year < 10000 &&
      month > 0 && month < 13 &&
      hour >= 0 && hour < 24 &&
      day > 0 && day <= DateTime.DaysInMonth(year, month) then
      Some(new DateTime(year, month, day, hour, 0, 0))
    else
      None

  /// Attempts to convert the string to a date time. Returns some datetime if
  /// successful
  let tryDateParse (str:string) =
    // imperatively check to see if the string contains only numbers and
    // periods as this cuts the function time in half compared to the
    // functional equivalent (Seq.forall)
    let mutable canBe = true
    let mutable i = 0
    while canBe && i < str.Length do
      canBe <- isNum (str.Chars(i)) || (str.Chars(i)) = '.'
      i <- i + 1

    if canBe then
      match str.Split('.') with
      | [|y;m;d|] -> tryDate (int y) (int m) (int d) 0
      | [|y;m;d;h|] -> tryDate (int y) (int m) (int d) (int h)
      | _ -> None
    else
      None

  /// Partial active pattern that will return a date if matched
  let (|ParaDate|_|) = tryDateParse

  /// Partial active pattern that will return a float if matched
  let (|ParaNumber|_|) = tryDoubleParse

  /// Specialized method on creating strings. Takes advantage of the fact 25%
  /// of strings in a savegame come from just 5 different string, so we pre-
  /// emptively check the buffer to see if this is a string we have already
  /// seen.
  let inline getString (buffer:char[]) (bufferLength:int) : string =
    match bufferLength with
    | 0 -> ""
    | 2 when buffer.[0] = 'i' && buffer.[1] = 'd' -> "id"
    | 4 when buffer.[0] = 'n' && buffer.[1] = 'a' &&
              buffer.[2] = 'm' && buffer.[3] = 'e' -> "name"
    | 4 when buffer.[0] = 't' && buffer.[1] = 'y' &&
              buffer.[2] = 'p' && buffer.[3] = 'e' -> "type"
    | 5 when buffer.[0] = 'f' && buffer.[1] = 'l' &&
              buffer.[2] = 'a' && buffer.[3] = 'g' &&
              buffer.[4] = 's' -> "flags"
    | _ -> String(buffer, 0, bufferLength)
