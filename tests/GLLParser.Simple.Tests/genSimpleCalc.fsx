#I @"../../Bin/Release/v40"

#r @"YC.RNGLR.dll"
#r @"YC.Common.dll"
#r @"YC.YardFrontend.dll"

open Yard.Generators.RNGLR
open Yard.Frontends.YardFrontend

module GLLParserSimpleTestsCalc =
    let gen = new RNGLR()
    let fe = new YardFrontend()

    let generate () =
        let il = fe.ParseGrammar "SimpleCalcWithErrors.yrd"
        gen.Generate(il, true, "-pos int -token string -module SimpleCalcWithErrors -translate true -bindSrc false -o SimpleCalcWithErrors.yrd.fs -abstract true") |> ignore
        let ilWOErrors = fe.ParseGrammar "SimpleCalcWithoutErrors.yrd"
        gen.Generate(ilWOErrors, true, "-pos int -token string -module SimpleCalcWithoutErrors -translate true -bindSrc false -o SimpleCalcWithoutErrors.yrd.fs -abstract true")

GLLParserSimpleTestsCalc.generate()
