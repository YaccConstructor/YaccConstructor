
#I @"../../Bin/Release/v40"

#r @"YC.Common.dll"
#r @"YC.RNGLR.dll"
#r @"YC.YardFrontend.dll"
#r @"YC.Conversions.dll"

open Yard.Generators.RNGLR
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta
open Yard.Core.Conversions.ExpandEbnfStrict

module RNGLRAbstractParserTests =
    let gen = new RNGLR()
    let fe = new YardFrontend()
    let meta = new ExpandMeta()
    let ebnf = new ExpandEbnf()
    let withConv = [|"Epsilon", "Longest", "InfEpsilon"|] |> Seq.cast<string>
    let woTranslate = [|"Order", "Cond", "Attrs", "Calc", "Counter", "Cycle", "LongCycle"
                        , "Resolvers", "LolCalc", "Omit"|] |> Seq.cast<string>
    let withTranslate = [|"ComplexRightNull", "Expr", "First", "List", "SimpleRightNull"|] 
                        |> Seq.cast<string>
    
    let generate() = 
        withConv
        |> Seq.map (
            fun x -> 
                     let il = fe.ParseGrammar (x + ".yrd")
                     {il with grammar = ebnf.ConvertGrammar(il.grammar)} |> ignore
                     {il with grammar = meta.ConvertGrammar(il.grammar)} |> ignore
                     gen.Generate(il, true, "-pos int -token int -module RNGLR.Parse" + x + " -o " + x 
                         + ".yrd.fs") |> ignore
            ) |> ignore

        woTranslate
        |> Seq.map (
            fun x -> let il = fe.ParseGrammar (x + ".yrd")
                     gen.Generate(il, true, "-pos int -token int -module RNGLR.Parse" + x + " -o " + x 
                         + ".yrd.fs") |> ignore
            ) |> ignore

        withTranslate
        |> Seq.map (
            fun x -> let il = fe.ParseGrammar (x + ".yrd")
                     gen.Generate(il, true, "-pos int -token int -module RNGLR.Parse" + x + " -translate false -o " + x 
                         + ".yrd.fs") |> ignore
            ) |> ignore

        let eps = fe.ParseGrammar "Eps.yrd"
        {eps with grammar = ebnf.ConvertGrammar(eps.grammar)} |> ignore
        {eps with grammar = meta.ConvertGrammar(eps.grammar)} |> ignore
        gen.Generate(eps, true, "-pos int -token int -module RNGLR.Eps -translate false -table LR -o Eps.yrd.fs")
        |> ignore

        let eps2 = fe.ParseGrammar "Eps2.yrd"
        {eps2 with grammar = ebnf.ConvertGrammar(eps2.grammar)} |> ignore
        {eps2 with grammar = meta.ConvertGrammar(eps2.grammar)} |> ignore
        gen.Generate(eps2, true, "-pos int -token int -module RNGLR.Eps2 -translate false -table LR -o Eps2.yrd.fs")
        |> ignore

        let listEps = fe.ParseGrammar "ListEps.yrd"
        {listEps with grammar = ebnf.ConvertGrammar(listEps.grammar)} |> ignore
        {listEps with grammar = meta.ConvertGrammar(listEps.grammar)} |> ignore
        gen.Generate(listEps, true, "-pos int -token int -module RNGLR.ListEps -translate false -table LR -o ListEps.yrd.fs")
        |> ignore

        let brackets = fe.ParseGrammar "Brackets.yrd"
        {brackets with grammar = ebnf.ConvertGrammar(brackets.grammar)} |> ignore
        {brackets with grammar = meta.ConvertGrammar(brackets.grammar)} |> ignore
        gen.Generate(brackets, true, "-pos int -token int -module RNGLR.Brackets -translate false -table LR -o Brackets.yrd.fs")
        |> ignore

        let _brackets = fe.ParseGrammar "_Brackets.yrd"
        {_brackets with grammar = ebnf.ConvertGrammar(_brackets.grammar)} |> ignore
        {_brackets with grammar = meta.ConvertGrammar(_brackets.grammar)} |> ignore
        gen.Generate(_brackets, true, "-pos int -token int -module RNGLR._Brackets -translate false -table LR -o _Brackets.yrd.fs")
        |> ignore

RNGLRAbstractParserTests.generate()