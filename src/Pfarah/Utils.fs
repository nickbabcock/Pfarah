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

  /// Attempts to convert the string to a date time. Returns some datetime if
  /// successful
  let tryDateParse (str:string) =
    if str |> Seq.forall (fun c -> isNum c || c = '.') then
      match str.Split('.') with
      | [|y;m;d|] -> Some(new DateTime(int y, int m, int d))
      | [|y;m;d;h|] -> Some(new DateTime(int y, int m, int d, int h, 0, 0))
      | _ -> None
    else
      None

  /// Partial active pattern that will return a date if matched
  let (|ParaDate|_|) = tryDateParse

  /// Partial active pattern that will return a float if matched
  let (|ParaNumber|_|) = tryDoubleParse
