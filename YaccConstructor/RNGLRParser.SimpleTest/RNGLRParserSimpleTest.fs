// Learn more about F# at http://fsharp.net

module RNGLRParserSimpleTest

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open NUnit.Framework
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../../Tests/RNGLR/"
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
type ``RNGLR parser tests with simple lexer`` () =

    [<Test>]
    member test.``First grammar test``() =
        let parser = RNGLR.ParseFirst.buildAst
        let path = dir + "first.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst -> mAst.PrintAst()

    [<Test>]
    member test.``List test``() =
        let parser = RNGLR.ParseList.buildAst
        let path = dir + "list.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst -> mAst.PrintAst()

    [<Test>]
    member test.``Simple Right Null test``() =
        let parser = RNGLR.ParseSimpleRightNull.buildAst
        let path = dir + "simpleRightNull.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst -> mAst.PrintAst()

    [<Test>]
    member test.``Complex Right Null test``() =
        let parser = RNGLR.ParseComplexRightNull.buildAst
        let path = dir + "complexRightNull.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            mAst.PrintAst()
            RNGLR.ParseComplexRightNull.defaultAstToDot mAst "ast.dot"
        

    [<Test>]
    member test.``Expression test``() =
        let parser = RNGLR.ParseExpr.buildAst
        let path = dir + "expr.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            mAst.ChooseLongestMatch()
            mAst.PrintAst()

    [<Test>]
    member test.``Counter test - simple for translator``() =
        let parser = RNGLR.ParseCounter.buildAst
        let path = dir + "counter.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            mAst.PrintAst()
            let res = translate RNGLR.ParseCounter.translate mAst
            printfn "Result: %A" res
            Assert.AreEqual([5], res)


    [<Test>]
    member test.``Calc test - simple for translator``() =
        let parser = RNGLR.ParseCalc.buildAst
        let path = dir + "calc.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            let res = translate RNGLR.ParseCalc.translate mAst
            printfn "Result: %A" res
            Assert.AreEqual(List.replicate 8 105, res)

    [<Test>]
    member test.``Translate with Attributes``() =
        let parser = RNGLR.ParseAttrs.buildAst
        let path = dir + "attrs.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            mAst.PrintAst()
            let res = translate RNGLR.ParseAttrs.translate mAst 3 : int list
            printfn "Result: %A" res
            Assert.AreEqual([48], res)

    [<Test>]
    member test.``AST, containing cycles``() =
        let parser = RNGLR.ParseCycle.buildAst
        let path = dir + "cycle.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            //mAst.PrintAst
            printf "OK\n"
            RNGLR.ParseCycle.defaultAstToDot mAst "cyclesBefore.dot"
            //mAst.EliminateCycles()
            mAst.collectWarnings (fun _ -> 0,0)
            |> ResizeArray.iter (printfn "%A")
            mAst.ChooseLongestMatch()
            RNGLR.ParseCycle.defaultAstToDot mAst "cyclesAfter.dot"
            let res = translate RNGLR.ParseCycle.translate mAst
            printfn "Result: %A" res
            Assert.AreEqual([0], res)

    [<Test>]
    member test.``AST, containing long cycles``() =
        let parser = RNGLR.ParseLongCycle.buildAst
        let path = dir + "LongCycle.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            //mAst.PrintAst
            printf "OK\n"
            RNGLR.ParseLongCycle.defaultAstToDot mAst "LongCyclesBefore.dot"
            //mAst.EliminateCycles()
            mAst.collectWarnings (fun _ -> 0,0)
            |> ResizeArray.iter (printfn "%A")
            mAst.ChooseLongestMatch()
            let res = translate RNGLR.ParseLongCycle.translate mAst
            printfn "Result: %A" res
            Assert.AreEqual([1], res)

    [<Test>]
    member test.``Parse empty string``() =
        let parser = RNGLR.ParseEpsilon.buildAst
        let path = dir + "Epsilon.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            RNGLR.ParseEpsilon.defaultAstToDot mAst "epsilon.dot"
            let res = translate RNGLR.ParseEpsilon.translate mAst
            Assert.AreEqual([3], res)

    [<Test>]
    member test.``If Then Else``() =
        let parser = RNGLR.ParseCond.buildAst
        let path = dir + "Cond.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            mAst.PrintAst()
            RNGLR.ParseCond.defaultAstToDot mAst "ast.dot"
            let res = translate RNGLR.ParseCond.translate mAst
            printfn "Result: %A" res
            Assert.AreEqual([22; 40], res)

    [<Test>]
    member test.``Resolvers``() =
        let parser = RNGLR.ParseResolvers.buildAst
        let path = dir + "Resolvers.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            RNGLR.ParseResolvers.defaultAstToDot mAst "resolvers.dot"
            let res = translate RNGLR.ParseResolvers.translate mAst
            printfn "Result: %A" res
            Assert.AreEqual([List.replicate 5 1], res)

    [<Test>]
    member test.``Calculation order``() =
        let parser = RNGLR.ParseOrder.buildAst
        let path = dir + "Order.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            RNGLR.ParseOrder.res := []
            let _ = translate RNGLR.ParseOrder.translate mAst
            let res = List.rev !RNGLR.ParseOrder.res
            printfn "Result: %A" res
            Assert.AreEqual([1..8], res)

    [<Test>]
    member test.``Longest match``() =
        let parser = RNGLR.ParseLongest.buildAst
        let path = dir + "Longest.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst ->
            RNGLR.ParseLongest.defaultAstToDot mAst "longest.dot"
            mAst.ChooseLongestMatch()
            let res = translate RNGLR.ParseLongest.translate mAst
            printfn "Result: %A" res
            Assert.AreEqual([5,0], res)

(*[<EntryPoint>]
(new ``RNGLR parser tests with simple lexer``()).``AST, containing long cycles``()*)