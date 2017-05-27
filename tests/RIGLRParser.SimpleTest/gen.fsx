#I @"..\..\Bin\Release\v40"

#r @"YC.RIGLRGenerator.dll"
#r @"YC.Common.dll"
#r @"YC.YardFrontend.dll"
#r @"YC.Conversions.dll"

open Yard.Generators.RIGLRGenerator
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta
open Yard.Core.Conversions.ExpandEbnfStrict

module RIGLRParserSimpleTest =
    let gen = new RIGLR()
    let fe = new YardFrontend()
    let meta = new ExpandMeta()
    let ebnf = new ExpandEbnf()
    let strs = [|"Chaos", "Expr", "Brackets", "Grammar7"|] |> Seq.cast<string>

    let generate () =
        strs
        |> Seq.map (fun x -> let il = fe.ParseGrammar (x + ".yrd")
                             {il with grammar = ebnf.ConvertGrammar(il.grammar)} |> ignore
                             {il with grammar = meta.ConvertGrammar(il.grammar)} |> ignore
                             gen.Generate(il, true, "-token int -module RIGLR." + x) |> ignore) |> ignore

RIGLRParserSimpleTest.generate()
