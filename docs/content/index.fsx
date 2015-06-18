(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
# Pfarah

Pfarah is an extremely fast and easy to use solution for parsing files created
by Clausewitz engine from Paradox Interactive

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      Pfarah can be <a href="https://nuget.org/packages/Pfarah">installed from NuGet</a>:
      <pre>PM> Install-Package Pfarah</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

## Getting Started

*)

#r "Pfarah.dll"
open Pfarah

// Parse strings. Great for discovering how to work with Pfarah or the sample
// data
let obj = ParaValue.Parse "foo=bar"
obj?foo |> asString

// Load files. Great for performance
let obj2 = ParaValue.LoadText @"C:\somefilepath"

(**

## Documentation


 * [Tutorial](tutorial.html) contains a great walkthrough of the API

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library.

## Note on Memory Usage

While Pfarah tries to be memory conscious (parsing based on streams and not
whole strings), the resulting data structure may be considered a memory hug. A
60MB file parsed by Pfarah will consume about 300MB of memory while the parsed
document data structure hangs around. The data structure allows for easy
quering on the data at the  cost of memory. Thus, it is recommended that as
soon as the file is parsed to keep around only what is needed and cast the
data to the expected formats (such as an integer), which will consume less
space. This will allow the garbage collector to clean up the intermediary
300MB data structure.

## Contributing and copyright

The project is hosted on [GitHub][gh] where you can [report issues][issues],
fork  the project and submit pull requests. You might also want to read the
[library design notes][readme] to understand how it works.

The library is available under the MIT license. For more information see the
[License file][license] in the GitHub repository.

  [content]: https://github.com/fsprojects/Pfarah/tree/master/docs/content
  [gh]: https://github.com/fsprojects/Pfarah
  [issues]: https://github.com/fsprojects/Pfarah/issues
  [readme]: https://github.com/fsprojects/Pfarah/blob/master/README.md
  [license]: https://github.com/fsprojects/Pfarah/blob/master/LICENSE.txt
*)
