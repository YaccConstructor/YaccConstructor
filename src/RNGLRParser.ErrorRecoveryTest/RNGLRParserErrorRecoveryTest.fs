module RNGLRParserErrorRecoveryTest

open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
open NUnit.Framework

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../Tests/RNGLRRecovery/"
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

let needPrint = false

[<TestFixture>]
type ``RNGLR error recovery tests`` () =

    //[<Test>]
    member test.``Calc error after right brace``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc error after right brace.txt"
        
        match run path parser with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            if needPrint 
            then 
                mAst.PrintAst() 
                RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc error after right brace.dot"
            
    [<Test>]
    member test.``Calc error inside in braces``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc error inside in braces.txt"

        match run path parser with
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            if needPrint 
            then
                mAst.PrintAst()  
                RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc error inside in braces.dot"                          

    [<Test>]
    member test.``Calc error inside in braces2``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc error inside in braces2.txt"

        match run path parser with
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            if needPrint 
            then 
                mAst.PrintAst() 
                RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc error inside in braces2.dot"

    [<Test>]
    member test.``Calc missing left brace``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc missing left brace.txt"

        match run path parser with
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _ ,_) ->
            if needPrint 
            then 
                mAst.PrintAst() 
                RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc missing left brace.dot"

    [<Test>]
    member test.``Calc missing operand``() =
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc missing operand.txt"

        match run path parser with
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _ ,_) ->
            if needPrint 
            then 
                mAst.PrintAst() 
                RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc missing operand.dot"

    //[<Test>]
    member test.``Calc missing right brace``() = 
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc missing right brace.txt"                                    

        match run path parser with
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            if needPrint 
            then 
                mAst.PrintAst()
                RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc missing right brace.dot"

    [<Test>]
    member test.``Calc no operand in the end``() = 
        let parser = RNGLR.ParseCalcErrorAmb.buildAst
        let path = dir + "Calc no operand in the end.txt"                                    

        match run path parser with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            if needPrint 
            then 
                mAst.PrintAst()
                RNGLR.ParseCalcErrorAmb.defaultAstToDot mAst "Calc no operand in the end.dot"
        
    [<Test>]
    member test.``Many reductions before error``() = 
        let parser = RNGLR.ParseManyReductions.buildAst
        let path = dir + "Many reductions before error.txt"

        match run path parser with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            if needPrint 
            then 
                mAst.PrintAst()
                RNGLR.ParseManyReductions.defaultAstToDot mAst "Many reductions.dot"

    [<Test>]
    member test.``Eps error in the end``() = 
        let parser = RNGLR.ParseErrorToEps.buildAst
        let path = dir + "Eps error in the end.txt"

        match run path parser with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            if needPrint 
            then
                mAst.PrintAst() 
                RNGLR.ParseErrorToEps.defaultAstToDot mAst "Eps error in the end.dot"
    
    [<Test>]
    member test.``Ambiguous``() = 
        let parser = RNGLR.ParseAmbiguous.buildAst
        let path = dir + "Ambiguous.txt"

        match run path parser with
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            if needPrint 
            then 
                mAst.PrintAst()
                RNGLR.ParseAmbiguous.defaultAstToDot mAst "Ambiguous.dot"

[<TestFixture>]
type ``RNGLR error recovery tests Semantics`` () =
    
    [<Test>]
    member test.``PrintErrorInfo``() = 
        let parser = RNGLR.ParsePrintErrorInfo.buildAst
        let path = dir + "PrintErrorInfo.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->
           if needPrint 
           then 
               mAst.PrintAst()
               RNGLR.ParsePrintErrorInfo.defaultAstToDot mAst "PrintErrorInfo.dot"
           
           let res = translate RNGLR.ParsePrintErrorInfo.translate mAst errors
           Assert.AreEqual([-3], res)
            
    //[<Test>]
    member test.``PrintErrorInfoEOF``() = 
        let parser = RNGLR.ParsePrintErrorInfoEOF.buildAst
        let path = dir + "PrintErrorInfoEOF.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->
           if needPrint 
           then 
               mAst.PrintAst()
               RNGLR.ParsePrintErrorInfoEOF.defaultAstToDot mAst "PrintErrorInfo.dot"
           
           let res = translate RNGLR.ParsePrintErrorInfo.translate mAst errors
           Assert.AreEqual([-1], res)

    [<Test>]
    member test.``Primitive translate``() = 
        let parser = RNGLR.ParsePrimitiveErrorTranslate.buildAst
        let path = dir + "Primitive translate.txt"

        match run path parser with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->
            if needPrint 
            then 
                mAst.PrintAst()
                RNGLR.ParsePrimitiveErrorTranslate.defaultAstToDot mAst "Eps error in the end.dot"
            
            let res = translate RNGLR.ParsePrimitiveErrorTranslate.translate mAst errors
            Assert.AreEqual([-1], res)

    //[<Test>]
    member test.``Error to epsilon translate``() = 
        let parser = RNGLR.ParsePrimitiveErrorTranslate.buildAst
        let path = dir + "Primitive translate2.txt"

        match run path parser with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->
            if needPrint 
            then 
                mAst.PrintAst()
                RNGLR.ParsePrimitiveErrorTranslate.defaultAstToDot mAst "Error to epsilon translate.dot"
            
            let res = translate RNGLR.ParsePrimitiveErrorTranslate.translate mAst errors
            Assert.AreEqual([-1], res)

(*[<EntryPoint>]
(new ``RNGLR error recovery tests``()).Ambiguous()*)

(*[<EntryPoint>]
(new ``RNGLR error recovery tests``()).``Calc error inside in braces2``()*)

(*[<EntryPoint>]
(new ``RNGLR error recovery tests``()).``PrintErrorInfo``()*)
