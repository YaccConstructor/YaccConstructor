#I @"..\..\Bin\Release\v40"

#r @"YC.Common.dll"
#r @"YC.RNGLR.dll"
#r @"YC.YardFrontend.dll"

open Yard.Generators.RNGLR
open Yard.Frontends.YardFrontend

module ControlFlowGraphTest =
    let fe = new YardFrontend()
    let gen = new RNGLR()
    let strs = [|"ExtendedCalc", "If", "Simple", "Let"|] |> Seq.ofArray |> Seq.cast<string>

    let generate() = 
        strs
        |> Seq.map (fun x -> gen.Generate(fe.ParseGrammar (x + ".yrd"), true, "-pos int -module " + x + "Test.Parser -translate false -light true -abstract true -o " + x + ".yrd.fs"))
        |> ignore


ControlFlowGraphTest.generate()