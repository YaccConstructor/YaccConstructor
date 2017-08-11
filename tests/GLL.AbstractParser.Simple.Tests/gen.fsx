#I @"../../Bin/Release/v40"

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
        let ilFirst = fe.ParseGrammar "GPPerf1.yrd"
        {ilFirst with grammar = meta.ConvertGrammar(ilFirst.grammar)}
        let ilSecond = fe.ParseGrammar "GPPerf2.yrd"
        {ilSecond with grammar = meta.ConvertGrammar(ilSecond.grammar)}
        gen.Generate(ilFirst, true, "-pos int -token int -module GLL.GPPerf1  -o GPPerf1.yrd.fs") |> ignore
        gen.Generate(ilSecond, true, "-pos int -token int -module GLL.GPPerf2  -o GPPerf2.yrd.fs")

GLLAbstractParserSimpleTests.generate()
