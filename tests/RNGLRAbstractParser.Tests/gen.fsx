#I @"../../Bin/Release/v40"

#r @"YC.Common.dll"
#r @"YC.RNGLR.dll"
#r @"YC.YardFrontend.dll"

open Yard.Generators.RNGLR
open Yard.Frontends.YardFrontend

module RNGLRAbstractParserTests =
    let gen = new RNGLR()
    let fe = new YardFrontend()
    let strs = [|"AandB"; "SimpleCalc"; "EpsilonKiller"; "simpleCalc_with_Nterms"; "simpleCalc_with_Nterms_2"
                ; "simpleCalc_with_Nterms_3"; "simpleCalc_with_Nterms_4"; "PrettySimpleCalc"; "NotAmbigousSimpleCalc"
                ; "NotAmbigousSimpleCalcWith2Ops"; "Stars"; "CroppedBrackets"; "Brackets"
                ; "StrangeBrackets"|]
    
    let generate() = 
        for x in strs do
            printf "Gr: %s \n"  x
            let mutable il = fe.ParseGrammar (x + ".yrd")
            gen.Generate(il, true, "-pos int -token int -module RNGLR." + x + " -translate false -o " + x 
                         + ".yrd.fs" + " -abstract true")

RNGLRAbstractParserTests.generate()