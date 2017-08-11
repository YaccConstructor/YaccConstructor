#I @"../../Bin/Release/v40"

#r @"YC.Common.dll"
#r @"YC.GLLGenerator.dll"
#r @"YC.YardFrontend.dll"

open Yard.Generators.GLL
open Yard.Frontends.YardFrontend

module GLLParserSimpleTests =
    let fe = new YardFrontend()
    let gen = new GLL()
    let yards = [|"Epsilon"; "Longest"; "InfEpsilon"; "Order"; "Cond"; "Attrs"
                 ; "Calc"; "Counter"; "Cycle"; "LongCycle"; "LolCalc"; "Omit"
                 ; "ComplexRightNull"; "Expr"; "First"; "List"; "SimpleRightNull"
                 |] |> Seq.ofArray

    let generate() = 
       yards 
       |> Seq.map (fun x -> gen.Generate(fe.ParseGrammar (x + ".yrd")
                            , true, "-pos int -token int -module GLL.Parse" + x + " -o "
                            + x + ".yrd.fs"))
       |> ignore
       gen.Generate(fe.ParseGrammar "Eps.yrd", true, "-pos int -token int -module GLL.Eps -o Eps.yrd.fs") |> ignore
       gen.Generate(fe.ParseGrammar "Eps2.yrd", true, "-pos int -token int -module GLL.Eps2 -o Eps2.yrd.fs") |> ignore
       gen.Generate(fe.ParseGrammar "ListEps.yrd", true, "-pos int -token int -module GLL.ListEps -o ListEps.yrd.fs") |> ignore
       gen.Generate(fe.ParseGrammar "Brackets.yrd", true, "-pos int -token int -module GLL.Brackets -o Brackets.yrd.fs") |> ignore
       gen.Generate(fe.ParseGrammar "BadLeftRecursion.yrd", true, "-pos int -token int -module GLL.BadLeftRecursion -o BadLeftRecursion.yrd.fs") |> ignore
       gen.Generate(fe.ParseGrammar "SimpleLeftRecursion.yrd", true, "-pos int -token int -module GLL.SimpleLeftRecursion -o SimpleLeftRecursion.yrd.fs ") |> ignore
       gen.Generate(fe.ParseGrammar "SimpleRightRecursion.yrd", true, "-pos int -token int -module GLL.SimpleRightRecursion -o SimpleRightRecursion.yrd.fs") |> ignore
       gen.Generate(fe.ParseGrammar "SimpleAmb.yrd", true, "-pos int -token int -module GLL.SimpleAmb -o SimpleAmb.yrd.fs") |> ignore
       gen.Generate(fe.ParseGrammar "Mixed.yrd", true, " -pos int -token int -module GLL.Mixed -o Mixed.yrd.fs") |> ignore
       gen.Generate(fe.ParseGrammar "PrettySimpleCalc.yrd", true, "-pos int -token int -module GLL.PrettySimpleCalc -o PrettySimpleCalc.yrd.fs ") |> ignore

GLLParserSimpleTests.generate()
