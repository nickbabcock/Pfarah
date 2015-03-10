namespace Pfarah

module DoubleParse =
  let inline private isNum (c:char) = c >= '0' && c <= '9'
  let inline private toNum (c:char) = int c - 48
  
  /// A highly optimized version of Double.TryParse that takes advantage of
  /// the fact that we know that the number format is (in regex form):
  /// <(\d+)\.(\d{3})?>. In profiling it was shown that Double.TryParse was a
  /// bottleneck at around 20-50% of the CPU time and after this function was
  /// written, the bottleneck completely disappeared.
  let tryParse (str:string) =
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
        isDone <- true
      else
        isDone <- true

    result
