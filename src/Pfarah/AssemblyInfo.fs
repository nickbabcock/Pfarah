﻿namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Pfarah")>]
[<assembly: AssemblyProductAttribute("Pfarah")>]
[<assembly: AssemblyDescriptionAttribute("Parses files generated by the Clausewitz engine")>]
[<assembly: AssemblyVersionAttribute("0.2.0")>]
[<assembly: AssemblyFileVersionAttribute("0.2.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.2.0"