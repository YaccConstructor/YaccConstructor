
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
    let withConv = [|"Epsilon"; "Longest"; "InfEpsilon"|]
    let woTranslate = [|"Order"; "Cond"; "Attrs"; "Calc"; "Counter"; "Cycle"; "LongCycle"
                        ; "Resolvers"; "LolCalc"|]
    let withTranslate = [|"ComplexRightNull"; "Expr"; "First"; "List"; "SimpleRightNull"|]
                        
    
    let generate() = 
        for x in withConv do
            printf "GR: %s \n" x
            let mutable il = fe.ParseGrammar (x + ".yrd")
            il <- {il with grammar = ebnf.ConvertGrammar(il.grammar)}
            il <- {il with grammar = meta.ConvertGrammar(il.grammar)}
            gen.Generate(il, true, "-pos int -token int -module RNGLR.Parse" + x + " -o " + x 
                + ".yrd.fs")
            |> ignore

        for x in woTranslate do
            printf "Gr: %s \n" x
            let mutable il = fe.ParseGrammar (x + ".yrd")
            gen.Generate(il, true, "-pos int -token int -module RNGLR.Parse" + x + " -o " + x 
                + ".yrd.fs")
            |> ignore

        for x in withTranslate do
            printf "Gr: %s \n" x
            let mutable il = fe.ParseGrammar (x + ".yrd")
            gen.Generate(il, true, "-pos int -token int -module RNGLR.Parse" + x + " -translate false -o " + x 
                + ".yrd.fs")
            |> ignore

        let mutable omit = fe.ParseGrammar "Omit.yrd"
        omit <- {omit with grammar = ebnf.ConvertGrammar(omit.grammar)}
        omit <- {omit with grammar = meta.ConvertGrammar(omit.grammar)}
        gen.Generate(omit, true, "-pos int -token int -module RNGLR.ParseOmit -o Omit.yrd.fs")
        |> ignore

        let mutable eps = fe.ParseGrammar "Eps.yrd"
        eps <- {eps with grammar = ebnf.ConvertGrammar(eps.grammar)}
        eps <- {eps with grammar = meta.ConvertGrammar(eps.grammar)}
        gen.Generate(eps, true, "-pos int -token int -module RNGLR.Eps -translate false -table LR -o Eps.yrd.fs")
        |> ignore

        let mutable eps2 = fe.ParseGrammar "Eps2.yrd"
        eps2 <- {eps2 with grammar = ebnf.ConvertGrammar(eps2.grammar)}
        eps2 <- {eps2 with grammar = meta.ConvertGrammar(eps2.grammar)}
        gen.Generate(eps2, true, "-pos int -token int -module RNGLR.Eps2 -translate false -table LR -o Eps2.yrd.fs")
        |> ignore

        let mutable listEps = fe.ParseGrammar "ListEps.yrd"
        listEps <- {listEps with grammar = ebnf.ConvertGrammar(listEps.grammar)}
        listEps <- {listEps with grammar = meta.ConvertGrammar(listEps.grammar)}
        gen.Generate(listEps, true, "-pos int -token int -module RNGLR.ListEps -translate false -table LR -o ListEps.yrd.fs")
        |> ignore

        let mutable brackets = fe.ParseGrammar "Brackets.yrd"
        brackets <- {brackets with grammar = ebnf.ConvertGrammar(brackets.grammar)}
        brackets <- {brackets with grammar = meta.ConvertGrammar(brackets.grammar)}
        gen.Generate(brackets, true, "-pos int -token int -module RNGLR.Brackets -translate false -table LR -o Brackets.yrd.fs")
        |> ignore

        let mutable _brackets = fe.ParseGrammar "_Brackets.yrd"
        _brackets <- {_brackets with grammar = ebnf.ConvertGrammar(_brackets.grammar)}
        _brackets <- {_brackets with grammar = meta.ConvertGrammar(_brackets.grammar)}
        gen.Generate(_brackets, true, "-pos int -token int -module RNGLR._Brackets -translate false -table LR -o _Brackets.yrd.fs")
        |> ignore

RNGLRAbstractParserTests.generate()