#I @"..\..\Bin\Release\v40"

#r @"YC.Common.dll"
#r @"YC.GLLGenerator.dll"
#r @"YC.YardFrontend.dll"
#r @"YC.Conversions.dll"

open Yard.Generators.GLL
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta 

module GLLAbstractParserSimpleTests =
    let fe = new YardFrontend()
    let gen = new GLL()
    let meta = new ExpandMeta()

    let generate() = 
        let g = fe.ParseGrammar "BioCFG.yrd"
        {g with grammar = meta.ConvertGrammar(g.grammar)}
        gen.Generate(g, true, "-pos int -token int -module GLL.BioCFG  -o BioCFG.yrd.fs")

GLLAbstractParserSimpleTests.generate()
