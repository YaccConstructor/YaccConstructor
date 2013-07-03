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

    //[<Test>]
    member test.``Calc error after right brace``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc error after right brace.txt"

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            //debugs.drawGSSDot "res.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst() 
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc error after right brace.dot"
            
    [<Test>]
    member test.``Calc error inside in braces``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc error inside in braces.txt"

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            //debugs.drawGSSDot "res.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst() 
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc error inside in braces.dot"                          

    [<Test>]
    member test.``Calc error inside in braces2``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc error inside in braces2.txt"

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            //debugs.drawGSSDot "Calc error_in_middle3.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst() 
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc error inside in braces2.dot"

    [<Test>]
    member test.``Calc missing left brace``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc missing left brace.txt"

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            debugs.drawGSSDot "Calc missing left brace.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst() 
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc missing left brace.dot"

    [<Test>]
    member test.``Calc missing operand``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc missing operand.txt"

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            debugs.drawGSSDot "Calc missing operand.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst() 
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc missing operand.dot"

    [<Test>]
    member test.``Calc missing operator``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc missing operator.txt"

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            //debugs.drawGSSDot "Calc error_in_middle6.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst() 
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc missing operator.dot"

    //[<Test>]
    member test.``Calc missing right brace``() = 
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc missing right brace.txt"                                    

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst()
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc missing right brace.dot"

    [<Test>]
    member test.``Calc no operand in the end``() = 
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc no operand in the end.txt"                                    

        match run path parser with
        | Parser.Error (num, tok, err, debugs) -> 
            //debugs.drawGSSDot "CCalc no operand in the end.dot"
            printErr (num, tok, err)
        | Parser.Success mAst -> 
            mAst.PrintAst()
            RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc no operand in the end.dot"

    [<Test>]
    member test.``Many reductions before error``() = 
        let parser = RNGLR.ParseManyReductions.buildAst
        let path = dir + "Many reductions before error.txt"

        match run path parser with 
        | Parser.Error (num, tok, err, debugs) ->
            printErr (num, tok, err)
        | Parser.Success mAst ->
            mAst.PrintAst()
            RNGLR.ParseManyReductions.defaultAstToDot mAst "Many reductions.dot"
    
    [<Test>]
    member test.``Many reductions before error2``()=
        let parser = RNGLR.ParseManyReductions2.buildAst
        let path = dir + "Many reductions before error2.txt"

        match run path parser with 
        | Parser.Error (num, tok, err, debugs) ->
            printErr (num, tok, err)
        | Parser.Success mAst ->
            mAst.PrintAst()
            RNGLR.ParseManyReductions2.defaultAstToDot mAst "Many reductions2.dot"

(*[<EntryPoint>]
(new ``RNGLR error recovery tests``()).``Many reductions before error``()*)

[<EntryPoint>]
(new ``RNGLR error recovery tests``()).``Calc missing left brace``()