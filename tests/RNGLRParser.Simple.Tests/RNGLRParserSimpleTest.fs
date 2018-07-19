module RNGLRParserSimpleTest

open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
open NUnit.Framework
open Microsoft.FSharp.Collections
open System.IO

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = Path.Combine(__SOURCE_DIRECTORY__, "..", "data", "RNGLR") + Path.DirectorySeparatorChar.ToString()
let getPath file = System.IO.Path.Combine(dir, file)

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

    let translateAndCheck toDot translateFunction (expected : List<_>) (ast : Tree<_>) file errors = 
#if DEBUG
        ast.PrintAst()
#endif        
        let res = translate translateFunction ast errors
        printfn "Result: %A" res
        Assert.AreEqual(expected, res)

    let printAst (ast:Tree<_>) file errors =
#if DEBUG
        ast.PrintAst()
#else
        ()
#endif

    let runTest parser file processSuccess = 
        let path = getPath file

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) -> processSuccess mAst file errors

    
    [<Test>]
    member test.``Omit``() =
        let parser = RNGLR.ParseOmit.buildAst
        let path = getPath "Omit.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->
            let res = translate RNGLR.ParseOmit.translate mAst errors
            printfn "%A" res
            Assert.AreEqual([[1; 3]], res)
            
            (*
    [<Test>]
    member test.``First grammar test``() =
        runTest RNGLR.ParseFirst.buildAst "First.txt" printAst
        *)

    [<Test>]
    member test.``List test``() =
        runTest RNGLR.ParseList.buildAst "List.txt" printAst

        (*
    [<Test>]
    member test.``Simple Right Null test``() =
        runTest RNGLR.ParseSimpleRightNull.buildAst "SimpleRightNull.txt" printAst

    [<Test>]
    member test.``Complex Right Null test``() =
        runTest RNGLR.ParseComplexRightNull.buildAst "ComplexRightNull.txt" printAst 
        *)

    [<Test>]
    member test.``Counter test - simple for translator``() =
        runTest RNGLR.ParseCounter.buildAst "Counter.txt" (translateAndCheck RNGLR.ParseCounter.defaultAstToDot RNGLR.ParseCounter.translate [5])

    [<Test>]
    member test.``Calc test - simple for translator``() =
        runTest RNGLR.ParseCalc.buildAst "Calc.txt" (translateAndCheck RNGLR.ParseCalc.defaultAstToDot RNGLR.ParseCalc.translate <| List.replicate 8 105)

    [<Test>]
    member test.``Translate with Attributes``() =
        let processSuccess (mAst : Tree<_>) file errors = 
            //mAst.PrintAst()
            let res = (translate RNGLR.ParseAttrs.translate mAst errors) 3 : int list
            printfn "Result: %A" res
            Assert.AreEqual([48], res)
        runTest RNGLR.ParseAttrs.buildAst "Attrs.txt" processSuccess

    [<Test>]
    member test.``Parse empty string``() =
        runTest RNGLR.ParseEpsilon.buildAst "Epsilon.txt" (translateAndCheck RNGLR.ParseEpsilon.defaultAstToDot RNGLR.ParseEpsilon.translate [3])

    [<Test>]
    member test.``If Then Else``() =
        runTest RNGLR.ParseCond.buildAst "Cond.txt" (translateAndCheck RNGLR.ParseCond.defaultAstToDot RNGLR.ParseCond.translate [22])

    [<Test>]
    member test.``Resolvers``() =
        runTest RNGLR.ParseResolvers.buildAst "Resolvers.txt" (translateAndCheck RNGLR.ParseResolvers.defaultAstToDot RNGLR.ParseResolvers.translate [List.replicate 5 1])

    [<Test>]
    member test.``Calculation order``() =
        let parser = RNGLR.ParseOrder.buildAst
        let path = getPath "Order.txt"

        match run path parser with
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->
            RNGLR.ParseOrder.res := []
            let _ = translate RNGLR.ParseOrder.translate mAst errors
            let res = List.rev !RNGLR.ParseOrder.res
            printfn "Result: %A" res
            Assert.AreEqual([1..8], res)

    [<Test>]
    member test.``Longest match``() =
        let parser = RNGLR.ParseLongest.buildAst
        let path = getPath "Longest.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->
            RNGLR.ParseLongest.defaultAstToDot mAst "longest.dot"
            mAst.ChooseLongestMatch()
            let res = translate RNGLR.ParseLongest.translate mAst errors
            printfn "Result: %A" res
            Assert.AreEqual([5,0], res)

    [<Test>]
    member test.``AST, containing cycles``() =
        let parser = RNGLR.ParseCycle.buildAst
        let path = getPath "Cycle.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->            
            printf "OK\n"
            RNGLR.ParseCycle.defaultAstToDot mAst "cyclesBefore.dot"
            mAst.collectWarnings (fun _ -> 0,0)
            |> ResizeArray.iter (printfn "%A")
            mAst.ChooseLongestMatch()
            RNGLR.ParseCycle.defaultAstToDot mAst "cyclesAfter.dot"
            let res = translate RNGLR.ParseCycle.translate mAst errors
            printfn "Result: %A" res
            Assert.AreEqual([0], res)

    [<Test>]
    member test.``AST, containing long cycles``() =
        let parser = RNGLR.ParseLongCycle.buildAst
        let path = getPath "LongCycle.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, errors) ->            
            printf "OK\n"
            RNGLR.ParseLongCycle.defaultAstToDot mAst "LongCyclesBefore.dot"
            mAst.collectWarnings (fun _ -> 0,0)
            |> ResizeArray.iter (printfn "%A")
            mAst.ChooseLongestMatch()
            let res = translate RNGLR.ParseLongCycle.translate mAst errors
            printfn "Result: %A" res
            Assert.AreEqual([1], res)

            (*
    [<Test>]
    member test.``Expression test``() =
        let parser = RNGLR.ParseExpr.buildAst
        let path = dir + "expr.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _) ->
            mAst.ChooseLongestMatch()
            mAst.PrintAst()
            *)

//    [<Test>]
//    member test.``Lol Calc. To test priority``() =
//        let parser = RNGLR.ParseLolCalc.buildAst
//        let path = dir + "LolCalc.txt"//
//
  //      match run path parser with
    //    | Parser.Error (num, tok, err,_, _) -> printErr (num, tok, err)
 //       | Parser.Success (mAst, errors) ->
 //           let res = translate RNGLR.ParseLolCalc.translate mAst errors
 //           RNGLR.ParseLolCalc.defaultAstToDot mAst "lolCalc.dot"
 //           printfn "Result: %A" res
 //           Assert.AreEqual(List.replicate 2 45, res)

    [<Test>]
    member test.``Eps``() =
        runTest RNGLR.ParseEps.buildAst "Eps.txt" printAst

    [<Test>]
    member test.``Eps2``() =
        runTest RNGLR.ParseEps2.buildAst "Eps2.txt" printAst

    [<Test>]
    member test.``ListEps``() =
        runTest RNGLR.ParseListEps.buildAst "ListEps.txt" printAst

        (*
    [<Test>]
    member test.Brackets() =
        runTest RNGLR.ParseBrackets.buildAst "Brackets.txt" printAst
        *)


    [<Test>]
    member test._Brackets() =
        runTest RNGLR.Parse_Brackets.buildAst "_Brackets.txt" printAst


//[<EntryPoint>]
let f x =
    if System.IO.Directory.Exists "dot" 
    then 
        System.IO.Directory.GetFiles "dot" |> Seq.iter System.IO.File.Delete
    else System.IO.Directory.CreateDirectory "dot" |> ignore
    let t = new ``RNGLR parser tests with simple lexer`` ()
    t.``Translate with Attributes`` ()

    0
