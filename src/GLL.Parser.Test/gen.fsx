#I @"..\..\Bin\Release\v40"

#r @"YC.Common.dll"
#r @"YC.GLLGenerator.dll"
#r @"YC.YardFrontend.dll"

open Yard.Generators.GLL
open Yard.Frontends.YardFrontend

module GLLParserTest =
    let fe = new YardFrontend()
    let gen = new GLL()

    let generate() = 
        let il = fe.ParseGrammar "test.yrd"
        gen.Generate(il, true, "-token unit -module GLL.test -o test.fs") |> ignore

GLLParserTest.generate()
