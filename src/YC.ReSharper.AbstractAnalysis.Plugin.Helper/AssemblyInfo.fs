namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("YC.ReSharper.AbstractAnalysis.Plugin.Helper")>]
[<assembly: AssemblyProductAttribute("YaccConstructor")>]
[<assembly: AssemblyDescriptionAttribute("Platform for parser generators and other grammarware research and development.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
