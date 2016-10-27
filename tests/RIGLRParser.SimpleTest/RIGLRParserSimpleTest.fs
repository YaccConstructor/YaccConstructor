module RIGLRParser.SimpleTest

open LexCommon
open Yard.Generators.RIGLR.Parser

open NUnit.Framework

open RIGLR.Grammar7
open RIGLR.Chaos
open RIGLR.Expr
open RIGLR.Brackets

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
    member this.``example`` () =
        let parser = RIGLR.Grammar7.buildAst
        runTest parser "Grammar7.txt" "sppfEx.dot" RIGLR.Grammar7.numToString RIGLR.Grammar7.leftSide RIGLR.Grammar7.tokenToNumber (Some RIGLR.Grammar7.tokenData)

    [<Test>] 
    member this.``chaos``() =
        let parser = RIGLR.Chaos.buildAst
        runTest parser "chaos.txt" "sppfC.dot" RIGLR.Chaos.numToString RIGLR.Chaos.leftSide RIGLR.Chaos.tokenToNumber (Some RIGLR.Chaos.tokenData)

    [<Test>]
    member this.``expr``() =
        let parser = RIGLR.Expr.buildAst
        runTest parser "Expr.txt" "sppfE.dot" RIGLR.Expr.numToString RIGLR.Expr.leftSide RIGLR.Expr.tokenToNumber (Some RIGLR.Expr.tokenData)

    [<Test>]
    member this.``brackets`` () =
        let parser = RIGLR.Brackets.buildAst
        runTest parser "lbr_a_rbr.txt" "sppfB.dot" RIGLR.Brackets.numToString RIGLR.Brackets.leftSide RIGLR.Brackets.tokenToNumber (Some RIGLR.Brackets.tokenData)        