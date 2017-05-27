#I @"..\..\Bin\Release\v40"

#r @"YC.Common.dll"
#r @"YC.GLLGenerator.dll"
#r @"YC.YardFrontend.dll"

open Yard.Generators.GLL
open Yard.Frontends.YardFrontend

module BioSearch =
    let fe = new YardFrontend()
    let gen = new GLL()

    let generate() = 
        let ilFirst = fe.ParseGrammar "..\YC.GrammarZOO\Bio\16s\R16S_1_18.yrd"
        let ilSecond = fe.ParseGrammar "..\YC.GrammarZOO\Bio\16s\R16S_1_18.yrd"
        gen.Generate(ilFirst, true, "-token unit -module GLL.R16S_19_27 -o R16S_19_27.fs") |> ignore
        gen.Generate(ilSecond, true, "-token unit -module GLL.R16S_19_27 -o R16S_19_27.fs")

BioSearch.generate()