#I @"../../Bin/Release/v40"

#r @"YC.Common.dll"
#r @"YC.RNGLR.dll"
#r @"YC.YardFrontend.dll"
#r @"YC.Conversions.dll"

open Yard.Generators.RNGLR
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions

module RNGLRAbstractParserTests =
    let gen = new RNGLR()
    let fe = new YardFrontend()
    let inAlt = new ExpandInnerAlt.ExpandInnerAlt()
    let defAC = new AddDefaultAC.AddDefaultAC()
    let ebnf = new ExpandEbnfStrict.ExpandEbnf()
    let meta = new ExpandMeta.ExpandMeta()

    let strs = [|"AandB"; "SimpleCalc"; "EpsilonKiller"; "simpleCalc_with_Nterms"; "simpleCalc_with_Nterms_2"
                ; "simpleCalc_with_Nterms_3"; "simpleCalc_with_Nterms_4"; "PrettySimpleCalc"; "NotAmbigousSimpleCalc"
                ; "NotAmbigousSimpleCalcWith2Ops"; "Stars"; "CroppedBrackets"; "Brackets"
                ; "StrangeBrackets"|]

    let generate() = 
        let mutable il = fe.ParseGrammar("AandB.yrd")
        for x in strs do
            printf "\nGr: %s \n"  x
            il <- fe.ParseGrammar (x + ".yrd")
            gen.Generate(il, true, "-pos int -token int -module RNGLR." + x + " -translate false -o " + x 
                         + ".yrd.fs" + " -abstract true")
        
        printf "\nGr: Calc \n"  
        il <- fe.ParseGrammar "Calc.yrd"
        il <- {il with grammar = ebnf.ConvertGrammar il.grammar}
        il <- {il with grammar = meta.ConvertGrammar il.grammar}
        gen.Generate(il, true, "-pos int -token int -module RNGLR.Calc -translate false -o Calc.yrd.fs -abstract true")

        printf "\nGr: Stars2 \n"  
        il <- fe.ParseGrammar "Stars2.yrd"
        il <- {il with grammar = inAlt.ConvertGrammar il.grammar}
        il <- {il with grammar = defAC.ConvertGrammar il.grammar}
        gen.Generate(il, true, "-pos int -token int -module RNGLR.Stars2 -translate false -o Stars2.yrd.fs -abstract true")

        printf "\nGr: Eps \n"  
        il <- fe.ParseGrammar "Eps.yrd"
        il <- {il with grammar = ebnf.ConvertGrammar il.grammar}
        il <- {il with grammar = defAC.ConvertGrammar il.grammar}
        gen.Generate(il, true, "-pos int -token int -module RNGLR.Eps -translate false -o Eps.yrd.fs -abstract true")

        printf "\nGr: List \n"  
        il <- fe.ParseGrammar "List.yrd"
        il <- {il with grammar = ebnf.ConvertGrammar il.grammar}
        il <- {il with grammar = meta.ConvertGrammar il.grammar}
        il <- {il with grammar = defAC.ConvertGrammar il.grammar}
        gen.Generate(il, true, "-pos int -token int -module RNGLR.List -translate false -o List.yrd.fs -abstract true")

        printf "\nGr: FirstEps \n"  
        il <- fe.ParseGrammar "FirstEps.yrd"
        il <- {il with grammar = ebnf.ConvertGrammar il.grammar}
        il <- {il with grammar = defAC.ConvertGrammar il.grammar}
        gen.Generate(il, true, "-pos int -token int -module RNGLR.FirstEps -translate false -o FirstEps.yrd.fs -abstract true")

        printf "\nGr: ErrorSupport \n"  
        il <- fe.ParseGrammar "ErrorSupport.yrd"
        il <- {il with grammar = ebnf.ConvertGrammar il.grammar}
        il <- {il with grammar = inAlt.ConvertGrammar il.grammar}
        il <- {il with grammar = defAC.ConvertGrammar il.grammar}
        gen.Generate(il, true, "-pos int -token int -module RNGLR.ErrorSupport -translate false -o ErrorSupport.yrd.fs -abstract true")

RNGLRAbstractParserTests.generate()