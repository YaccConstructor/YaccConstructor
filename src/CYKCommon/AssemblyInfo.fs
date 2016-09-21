namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("CYKCommon")>]
[<assembly: AssemblyProductAttribute("YaccConstructor")>]
[<assembly: AssemblyDescriptionAttribute("Platform for parser generators and other grammarware research and development.")>]
[<assembly: AssemblyVersionAttribute("0.1.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0.0"
    let [<Literal>] InformationalVersion = "0.1.0.0"
