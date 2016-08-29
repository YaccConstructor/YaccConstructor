module RIGLRParser.SimpleTest

open LexCommon
open Yard.Generators.RIGLR.Parser

open NUnit.Framework

open RIGLR.grammar7_7

let dir = @"..\..\..\data\RIGLR\"
let outDir = @"..\.."

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let runTest parser input fileName intToString leftSide tokToNum tokenData = 
    let path = dir + input
    let r = run path parser
    match r with
        | Error _ ->
            printfn "Error"
            Assert.Fail("Parsing finished with error!")
        | Success tree ->            
            tree.AstToDot intToString  tokToNum tokenData leftSide (outDir + fileName)                             
            Assert.Pass()

[<TestFixture>]
type ``Tests`` () =
    
    [<Test>]
    member this.``test`` () =
        let parser = buildAst
        runTest parser "grammar7_7.txt" "sppf.dot" numToString leftSide tokenToNumber (Some tokenData)