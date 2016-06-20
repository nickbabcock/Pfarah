# Benchmarking

Pfarah is fast. Pfarah takes advantage of the shape of the data to supply
hyper-optimized functions that far outstrip the .NET's own libraries. It used
to be the case that determining if "1.000" was a number, a datetime, or just a
plain string consumed most of the CPU time when parsing. After writing
optimized functions, the previous bottleneck is barely a blip.

I would like to state that for 99% of use cases, .NET's own libraries are
sufficiently fast as they can handle any format and culture. It's only when one
can make certain assumptions about the data can optimizations be utilized.

Using [BenchmarkDotNet](https://github.com/PerfDotNet/BenchmarkDotNet), the
following breaks down the performance difference between Pfarah's
hyper-optimized functions (prefixed with `pfarah`) and .NET libraries (prefixed
with `bcl`). I'll call out performance characteristics of both the happy path
(eg. parsing strings that are dates/numbers) and parsing the sad path (eg.
parsing strings that are no dates/numbers). Often the sad path is even more
important than the happy path when parsing a file with very few numbers and
dates.

```ini

BenchmarkDotNet=v0.9.7.0
OS=Microsoft Windows NT 6.2.9200.0
Processor=Intel(R) Core(TM) i7-6700K CPU 4.00GHz, ProcessorCount=8
Frequency=3914058 ticks, Resolution=255.4893 ns, Timer=UNKNOWN
HostCLR=MS.NET 4.0.30319.42000, Arch=32-bit RELEASE
JitModules=clrjit-v4.6.1080.0

Type=ParseDateTime  Mode=Throughput

```

## Parsing Doubles

            Method | Payload |     Median |    StdDev |
------------------ |-------- |----------- |---------- |
 **pfarahParseDouble** | **0.98765** | **13.9992 ns** | **0.5843 ns** |
    bclParseDouble | 0.98765 | 77.4219 ns | 1.7125 ns |
 **pfarahParseDouble** |   **1.000** | **10.3546 ns** | **0.4040 ns** |
    bclParseDouble |   1.000 | 68.8776 ns | 2.1476 ns |
 **pfarahParseDouble** |    **15.3** | **11.2105 ns** | **0.4590 ns** |
    bclParseDouble |    15.3 | 68.0926 ns | 2.0549 ns |
 **pfarahParseDouble** |      **50** |  **6.0571 ns** | **0.3556 ns** |
    bclParseDouble |      50 | 63.4717 ns | 0.5042 ns |
 **pfarahParseDouble** |     **abc** |  **4.0015 ns** | **1.5356 ns** |
    bclParseDouble |     abc | 63.4639 ns | 9.4690 ns |

- Happy path: More than 10x improvement for simple numbers (50). As numbers
  grow complex (more numbers, decimals) performance improvement drops to 5x.
- Sad path: 15x improvement for data that obviously isn't a number (abc)

## Parsing DateTimes

              Method |     Payload |      Median |     StdDev |
-------------------- |------------ |------------ |----------- |
 **pfarahParseDateTime** | **1942.5.2.14** | **111.5436 ns** |  **2.4011 ns** |
         bclDateTime | 1942.5.2.14 | 328.4756 ns | 23.3696 ns |
 **pfarahParseDateTime** |    **2015.8.1** |  **89.0527 ns** |  **3.5095 ns** |
         bclDateTime |    2015.8.1 | 331.6652 ns | 20.3652 ns |
 **pfarahParseDateTime** |   **99999.8.1** |  **58.8484 ns** | **10.9077 ns** |
         bclDateTime |   99999.8.1 | 273.6202 ns |  2.6354 ns |
 **pfarahParseDateTime** |         **abc** |   **4.7968 ns** |  **0.3181 ns** |
         bclDateTime |         abc | 209.5747 ns | 19.2509 ns |

- Happy path: 3-4x improvement depending on if the hour part of a date is omitted.
- Sad path: > 40x improvement for data that obviously isn't a date (abc), but if the date is closer to an actual date, then performance improvement drops to 5x.
