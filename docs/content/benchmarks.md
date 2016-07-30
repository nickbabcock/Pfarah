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

![pfara-benchmark](/Pfarah/img/doubles-parsing.png)

- Happy path: More than 10x improvement for simple numbers (50). As numbers
  grow complex (more numbers, decimals) performance improvement drops to 5x.
- Sad path: 15x improvement for data that obviously isn't a number (abc)

## Parsing DateTimes

![pfara-benchmark](/Pfarah/img/parse-dates.png)

- Happy path: 3-4x improvement depending on if the hour part of a date is omitted.
- Sad path: > 40x improvement for data that obviously isn't a date (abc), but if the date is closer to an actual date, then performance improvement drops to 5x.

Squirrelling away the code used to generate the plots from the csv output from BenchmarkDotnet

```R
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)

bench_plot <- function(fp, title) {
    # Converts "2.313 ns" to 2.313
    toNs <- function(x) {
        split <- str_split(x, " ")
        num <- as.numeric(split[[1]][[1]])
        factor <- switch(split[[1]][[2]], ns=1, us=1000, ms=1000*1000)
        num * factor
    }
    vtoNs <- Vectorize(toNs)

    read_csv2(fp) %>%
        select(Method, Payload, Median, StdDev) %>%
        mutate(Median = vtoNs(Median), StdDev = vtoNs(StdDev),
               Method = ifelse(str_detect(Method, "bcl"), "BCL", "Pfarah")) %>%
        ggplot(mapping = aes(Payload, Median, fill = Method)) +
        geom_bar(stat = 'identity', position='dodge') +
        coord_flip() + ylab("Median (ns)") + ggtitle(title)
}

bench_plot("ParseDateTime-report.csv",
    "Time Attempting to Parse Dates (Smaller Bar = Faster)")
bench_plot("ParseDouble-report.csv",
    "Time Attempting to Parse Doubles (Smaller Bar = Faster)")
```