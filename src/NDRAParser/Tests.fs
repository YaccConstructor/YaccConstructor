open Yard.Generators.NDRA
open System.IO

[<EntryPoint>]
let main _ =
    let basePath = "../../../../Tests/NDRA"

    let myParser = new Parser(Path.Combine(basePath, "A.yrd"))

    printfn "%A" (myParser.Parse [|"("; "NUM"; "+"; "NUM"; ")"; "*"; "NUM"; "*"; "("; "NUM"; "*"; "NUM"; ")"|])

    0
