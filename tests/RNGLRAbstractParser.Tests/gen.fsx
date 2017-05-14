#I @"../../Bin/Release/v40"

#r @"YC.Common.dll"
#r @"YC.RNGLR.dll"
#r @"YC.YardFrontend.dll"

open Yard.Generators.RNGLR
open Yard.Frontends.YardFrontend

module RNGLRAbstractParserTests =
    let gen = new RNGLR()
    let fe = new YardFrontend()
    let strs = [|"AandB", "SimpleCalc", "Calc", "EpsilonKiller", "simpleCalc_with_Nterms", "simpleCalc_with_Nterms_2"
                , "simpleCalc_with_Nterms_3", "simpleCalc_with_Nterms_4", "PrettySimpleCalc", "NotAmbigousSimpleCalc"
                , "NotAmbigousSimpleCalcWith2Ops", "Stars", "Stars2", "Eps", "List", "FirstEps", "CroppedBrackets", "Brackets"
                , "Brackets1", "StrangeBrackets", "ErrorSupport"|] |> Seq.ofArray |> Seq.cast<string>
    
    let generate() = 
        strs 
        |> Seq.map (
            fun x -> let il = fe.ParseGrammar (x + ".yrd")
                     gen.Generate(il, true, "-pos int -token int -module RNGLR." + x + " -translate false -o " + x 
                         + ".yrd.fs" + " -abstract true") |> ignore
            ) |> ignore

RNGLRAbstractParserTests.generate()