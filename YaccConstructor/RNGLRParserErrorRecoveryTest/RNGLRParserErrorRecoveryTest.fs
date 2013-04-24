// Learn more about F# at http://fsharp.net

module RNGLRParserErrorRecoveryTest

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open NUnit.Framework
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../../Tests/RNGLRRecovery/"
let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let inline translate (f : TranslateArguments<_,_> -> 'b -> 'c) (ast : 'b) =
    let args = {
        tokenToRange = fun _ -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }
    f args ast

[<TestFixture>]
type ``RNGLR error recovery tests`` () =

    [<Test>]
    member test.``Calc test non-amb``() =
        let parser = RNGLR.ParseCalcErrorNonAmb.buildAst
        let path = dir + "CalcRecoveryBrace1.txt"

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            debugs.drawGSSDot "res.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst() 
            RNGLR.ParseCalcErrorNonAmb.defaultAstToDot mAst "calcTestNonAmb.dot"
                                  
    [<Test>]
    member test.``Calc test amb``() = 
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "CalcRecoveryBrace1.txt"                                    

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            debugs.drawGSSDot "res.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst()
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "calcTestAmb.dot"
                                                                         
    [<Test>]
    member test.``Trivial test``() =                                                                                
        let parser = RNGLR.ParseTrivialRecovery.buildAst                                                                         
        let path = dir + "RecoveryTrivial.txt"

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            debugs.drawGSSDot "res.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst()
            RNGLR.ParseTrivialRecovery.defaultAstToDot mAst "trivial.dot"


//[<EntryPoint>]
(*(new ``RNGLR error recovery tests``()).``Trivial test`` ()
0*)

[<EntryPoint>]
(new ``RNGLR error recovery tests``()).``Calc test amb``()
(new ``RNGLR error recovery tests``()).``Calc test non-amb``()
0