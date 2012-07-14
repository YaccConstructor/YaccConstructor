// Learn more about F# at http://fsharp.net

module RNGLRParserSimpleTest

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open NUnit.Framework
open Yard.Generators
open LexCommon
open FsYaccCycle

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens, tokens

let dir = @"../../../../Tests/RNGLR/"

[<TestFixture>]
type ``RNGLR parser tests with simple lexer`` () =

    [<Test>]
    member test.``First grammar test``() =
        let parser = RNGLR.ParseFirst.buildAst
        let path = dir + "first/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst.PrintAst()

    [<Test>]
    member test.``List test``() =
        let parser = RNGLR.ParseList.buildAst
        let path = dir + "list/input.txt"

        match run path parser with
        | Parser.Error (num, message), _ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst.PrintAst()

    [<Test>]
    member test.``Simple Right Null test``() =
        let parser = RNGLR.ParseSimpleRightNull.buildAst
        let path = dir + "simpleRightNull/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst.PrintAst()

    [<Test>]
    member test.``Complex Right Null test``() =
        let parser = RNGLR.ParseComplexRightNull.buildAst
        let path = dir + "complexRightNull/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ ->
            mAst.PrintAst()
            RNGLR.ParseComplexRightNull.defaultAstToDot mAst "ast.dot"
        

    [<Test>]
    member test.``Expression test``() =
        let parser = RNGLR.ParseExpr.buildAst
        let path = dir + "expr/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst.PrintAst()

    [<Test>]
    member test.``Counter test - simple for translator``() =
        let parser = RNGLR.ParseCounter.buildAst
        let path = dir + "counter/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,tokens ->
            mAst.PrintAst()
            printfn "Result: %A" (RNGLR.ParseCounter.translate mAst)

    [<Test>]
    member test.``Calc test - simple for translator``() =
        let parser = RNGLR.ParseCalc.buildAst
        let path = dir + "calc/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,tokens ->
            mAst.PrintAst()
            printfn "Result: %A" (RNGLR.ParseCalc.translate mAst)

    [<Test>]
    member test.``Translate with Attributes``() =
        let parser = RNGLR.ParseAttrs.buildAst
        let path = dir + "attrs/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,tokens ->
            mAst.PrintAst()
            printfn "Result: %A" (RNGLR.ParseAttrs.translate mAst 3 : int list)

    [<Test>]
    member test.``AST, containing cycles``() =
        let parser = RNGLR.ParseCycle.buildAst
        let path = dir + "cycle/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,tokens ->
            //mAst.PrintAst
            printf "OK\n"
            RNGLR.ParseCycle.defaultAstToDot mAst "cyclesBefore.dot"
            mAst.EliminateCycles()
            RNGLR.ParseCycle.defaultAstToDot mAst "cyclesAfter.dot"
            printfn "Result: %A" (RNGLR.ParseCycle.translate mAst)
