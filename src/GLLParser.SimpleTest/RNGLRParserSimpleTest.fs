// Learn more about F# at http://fsharp.net

module GLLParserSimpleTest

open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
open NUnit.Framework
open Yard.Generators
open LexCommon
open Microsoft.FSharp.Collections

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../Tests/GLL/"
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
type ``GLL parser tests with simple lexer`` () =
    [<Test>]
    member test.``SIMPLE epsilon``() =
        let parser = GLL.SimpleAmb.buildAst
        let path = dir + "AA.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, errors) ->
            GLL.SimpleEpsilon.defaultAstToDot mAst "SimpleEpsilon.dot"
           // mAst.ChooseLongestMatch()
//            let res = translate GLL.ParseLongest.translate mAst errors
//            printfn "Result: %A" res
//            Assert.AreEqual([5,0], res)

    [<Test>]
    member test.``Omit``() =
        let parser = GLL.ParseOmit.buildAst
        let path = dir + "Omit.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, errors) ->
            let res = translate GLL.ParseOmit.translate mAst errors
            printfn "%A" res
            Assert.AreEqual([[1; 3]], res)

//    [<Test>]
//    member test.``First grammar test``() =
//        let parser = GLL.ParseFirst.buildAst
//        let path = dir + "first.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _) ->
//            mAst.PrintAst()
//
//    [<Test>]
//    member test.``List test``() =
//        let parser = GLL.ParseList.buildAst
//        let path = dir + "list.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _) ->
//            mAst.PrintAst()
//
//    [<Test>]
//    member test.``Simple Right Null test``() =
//        let parser = GLL.ParseSimpleRightNull.buildAst
//        let path = dir + "simpleRightNull.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _) -> mAst.PrintAst()
//
//    [<Test>]
//    member test.``Complex Right Null test``() =
//        let parser = GLL.ParseComplexRightNull.buildAst
//        let path = dir + "complexRightNull.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _) ->
//            mAst.PrintAst()
//            GLL.ParseComplexRightNull.defaultAstToDot mAst "ast.dot"
//        
//
//    //[<Test>]
//    //member test.``Expression test``() =
//     //   let parser = GLL.ParseExpr.buildAst
//      //  let path = dir + "expr.txt"
/////
// //       match run path parser with
//   //     | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//     //   | Parser.Success (mAst, _) ->
//       //     mAst.ChooseLongestMatch()
//         //   mAst.PrintAst()
//
//    [<Test>]
//    member test.``Counter test - simple for translator``() =
//        let parser = GLL.ParseCounter.buildAst
//        let path = dir + "counter.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            mAst.PrintAst()
//            let res = translate GLL.ParseCounter.translate mAst errors
//            printfn "Result: %A" res
//            Assert.AreEqual([5], res)
//
//
//    [<Test>]
//    member test.``Calc test - simple for translator``() =
//        let parser = GLL.ParseCalc.buildAst
//        let path = dir + "calc.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            let res = translate GLL.ParseCalc.translate mAst errors
//            printfn "Result: %A" res
//            Assert.AreEqual(List.replicate 8 105, res)
//
////    [<Test>]
////    member test.``Lol Calc. To test priority``() =
////        let parser = GLL.ParseLolCalc.buildAst
////        let path = dir + "LolCalc.txt"//
////
//  //      match run path parser with
//    //    | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
// //       | Parser.Success (mAst, errors) ->
// //           let res = translate GLL.ParseLolCalc.translate mAst errors
// //           GLL.ParseLolCalc.defaultAstToDot mAst "lolCalc.dot"
// //           printfn "Result: %A" res
// //           Assert.AreEqual(List.replicate 2 45, res)
//
//    [<Test>]
//    member test.``Translate with Attributes``() =
//        let parser = GLL.ParseAttrs.buildAst
//        let path = dir + "attrs.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            mAst.PrintAst()
//            let res = translate GLL.ParseAttrs.translate mAst errors 3 : int list
//            printfn "Result: %A" res
//            Assert.AreEqual([48], res)
//
//    [<Test>]
//    member test.``AST, containing cycles``() =
//        let parser = GLL.ParseCycle.buildAst
//        let path = dir + "cycle.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            //mAst.PrintAst
//            printf "OK\n"
//            GLL.ParseCycle.defaultAstToDot mAst "cyclesBefore.dot"
//            //mAst.EliminateCycles()
//            mAst.collectWarnings (fun _ -> 0,0)
//            |> ResizeArray.iter (printfn "%A")
//            mAst.ChooseLongestMatch()
//            GLL.ParseCycle.defaultAstToDot mAst "cyclesAfter.dot"
//            let res = translate GLL.ParseCycle.translate mAst errors
//            printfn "Result: %A" res
//            Assert.AreEqual([0], res)
//
//    [<Test>]
//    member test.``AST, containing long cycles``() =
//        let parser = GLL.ParseLongCycle.buildAst
//        let path = dir + "LongCycle.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            //mAst.PrintAst
//            printf "OK\n"
//            GLL.ParseLongCycle.defaultAstToDot mAst "LongCyclesBefore.dot"
//            //mAst.EliminateCycles()
//            mAst.collectWarnings (fun _ -> 0,0)
//            |> ResizeArray.iter (printfn "%A")
//            mAst.ChooseLongestMatch()
//            let res = translate GLL.ParseLongCycle.translate mAst errors
//            printfn "Result: %A" res
//            Assert.AreEqual([1], res)
//
//    [<Test>]
//    member test.``Parse empty string``() =
//        let parser = GLL.ParseEpsilon.buildAst
//        let path = dir + "Epsilon.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            GLL.ParseEpsilon.defaultAstToDot mAst "epsilon.dot"
//            let res = translate GLL.ParseEpsilon.translate mAst errors
//            Assert.AreEqual([3], res)
//
//    [<Test>]
//    member test.``If Then Else``() =
//        let parser = GLL.ParseCond.buildAst
//        let path = dir + "Cond.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            mAst.PrintAst()
//            GLL.ParseCond.defaultAstToDot mAst "ast.dot"
//            let res = translate GLL.ParseCond.translate mAst errors
//            printfn "Result: %A" res
//            Assert.AreEqual([22(*; 40*)], res)
//
//    [<Test>]
//    member test.``Resolvers``() =
//        let parser = GLL.ParseResolvers.buildAst
//        let path = dir + "Resolvers.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            GLL.ParseResolvers.defaultAstToDot mAst "resolvers.dot"
//            let res = translate GLL.ParseResolvers.translate mAst errors
//            printfn "Result: %A" res
//            Assert.AreEqual([List.replicate 5 1], res)
//
//    [<Test>]
//    member test.``Calculation order``() =
//        let parser = GLL.ParseOrder.buildAst
//        let path = dir + "Order.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            GLL.ParseOrder.res := []
//            let _ = translate GLL.ParseOrder.translate mAst errors
//            let res = List.rev !GLL.ParseOrder.res
//            printfn "Result: %A" res
//            Assert.AreEqual([1..8], res)
//
//    [<Test>]
//    member test.``Longest match``() =
//        let parser = GLL.ParseLongest.buildAst
//        let path = dir + "Longest.txt"
//
//        match run path parser with
//        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, errors) ->
//            GLL.ParseLongest.defaultAstToDot mAst "longest.dot"
//            mAst.ChooseLongestMatch()
//            let res = translate GLL.ParseLongest.translate mAst errors
//            printfn "Result: %A" res
//            Assert.AreEqual([5,0], res)
//
[<EntryPoint>]
(new ``GLL parser tests with simple lexer``()).``SIMPLE epsilon``()