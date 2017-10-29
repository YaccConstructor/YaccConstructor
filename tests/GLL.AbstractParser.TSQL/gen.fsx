#I @"../../Bin/Release/v40"

#r @"YC.Common.dll"
#r @"YC.GLLGenerator.dll"
#r @"YC.YardFrontend.dll"
#r @"YC.Conversions.dll"

open Yard.Generators.GLL
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta
open Yard.Core.Conversions.Linearize

module GLLAbstractParserTSQLGen =
    let fe = new YardFrontend()
    let gen = new GLL()
    let meta = new ExpandMeta()
    let line = new Linearize()

    let generate() =
        let mutable il = fe.ParseGrammar "../../src/YC.GrammarZOO/SQL/TSQL/mssql_abstract.yrd"
        il <- {il with grammar = il.grammar |> line.ConvertGrammar |> meta.ConvertGrammar}
        gen.Generate(il, true, "-pos int -token int -module GLL.MSParser -o GLLMsParser.yrd.fs") |> ignore

GLLAbstractParserTSQLGen.generate()
