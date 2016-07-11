module GLLParserSimpleTest

open Yard.Generators.GLL
open Yard.Generators.GLL.Parser 
open NUnit.Framework
open Yard.Generators
open LexCommon
open Microsoft.FSharp.Collections

open GLL.BadLeftRecursion
open GLL.Brackets
open GLL.Eps
open GLL.Eps2
open GLL.ListEps
open GLL.Mixed
open GLL.ParseAttrs
//open GLL.ParseCalc
open GLL.ParseComplexRightNull
open GLL.ParseCond
open GLL.ParseCounter
open GLL.ParseCycle
open GLL.ParseEpsilon
open GLL.ParseExpr
open GLL.ParseFirst
open GLL.ParseInfEpsilon
open GLL.ParseList
//open GLL.ParseLolCalc
open GLL.ParseLongCycle
open GLL.ParseLongest
open GLL.ParseOmit
open GLL.ParseOrder
open GLL.ParseSimpleRightNull
open GLL.SimpleLeftRecursion
open GLL.SimpleAmb
open GLL.SimpleRightRecursion
open GLL.PrettySimpleCalc

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../Tests/GLL/"
let outDir = @"../../../src/GLLParser.SimpleTest/"

let runTest parser input (intToString : int -> string) (fileName : string) nodesCount edgesCount termsCount ambiguityCount tokToNum tokenData = 
    let path = dir + input
    let r = run path parser
    match r with
        | Error _ ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            let n, e, t, a = tree.CountCounters 
            printfn "%d %d %d %d" n e t a
            tree.AstToDot intToString  tokToNum tokenData (outDir + fileName)
            Assert.AreEqual(nodesCount, n, "Nodes count mismatch")
            Assert.AreEqual(edgesCount, e, "Edges count mismatch")
            Assert.AreEqual(termsCount, t, "Terms count mismatch")
            Assert.AreEqual(ambiguityCount, a, "Ambiguities count mismatch")
            //tree.AstToDot intToString (outDir + outFileName)
            tree.AstToDot intToString  tokToNum tokenData (outDir + fileName)
            Assert.Pass()
            
            

[<TestFixture>]
type ``GLL parser tests with simple lexer`` () =

    [<Test>]
    member test.``Bad left rec``() =
        let parser = GLL.BadLeftRecursion.buildAst
        runTest parser "BBB.txt" GLL.BadLeftRecursion.numToString "BadLeftRec.dot" 33 45 5 1 GLL.BadLeftRecursion.tokenToNumber GLL.BadLeftRecursion.tokenData

    [<Test>]
    member test.``SimpleAmb``() =
        let parser = GLL.SimpleAmb.buildAst
        runTest parser "SimpleAmb.txt" GLL.SimpleAmb.numToString "SimpleAmb.dot" 18 21 5 1 GLL.SimpleAmb.tokenToNumber GLL.SimpleAmb.tokenData


    [<Test>]
    member test.``Simple left rec``() =
        let parser = GLL.SimpleLeftRecursion.buildAst
        runTest parser "BBB.txt" GLL.SimpleLeftRecursion.numToString "SimpleLeftRec.dot" 19 21 5 0 GLL.SimpleLeftRecursion.tokenToNumber GLL.SimpleLeftRecursion.tokenData

    [<Test>]
    member test.``Attrs``() =
        let parser = GLL.ParseAttrs.buildAst
        runTest parser "Attrs.txt" GLL.ParseAttrs.numToString "Attrs.dot" 29 33 7 0 GLL.ParseAttrs.tokenToNumber GLL.ParseAttrs.tokenData

    [<Test>]
    member test.``Brackets``() =
        let parser = GLL.Brackets.buildAst
        runTest parser "Brackets.txt" GLL.Brackets.numToString "Brackets.dot" 46 54 11 1 GLL.Brackets.tokenToNumber GLL.Brackets.tokenData

    [<Test>]
    member test.``Simple right rec``() =
        let parser = GLL.SimpleRightRecursion.buildAst
        runTest parser "BBB.txt" GLL.SimpleRightRecursion.numToString "SimpleRightRec.dot" 15 15 5 0 GLL.SimpleRightRecursion.tokenToNumber GLL.SimpleRightRecursion.tokenData

    [<Test>]
    member test.``Complex right null``() =
        let parser = GLL.ParseComplexRightNull.buildAst
        runTest parser "ComplexRightNull.txt" GLL.ParseComplexRightNull.numToString "ComplexRightNull.dot" 35 42 8 1 GLL.ParseComplexRightNull.tokenToNumber GLL.ParseComplexRightNull.tokenData

    [<Test>]
    member test.``Counter``() =
        let parser = GLL.ParseCounter.buildAst
        runTest parser "Counter.txt" GLL.ParseCounter.numToString "Counter.dot" 21 21 7 0 GLL.ParseCounter.tokenToNumber GLL.ParseCounter.tokenData

    [<Test>]
    member test.``Cycle``() =
        let parser = GLL.ParseCycle.buildAst
        runTest parser "Cycle.txt" GLL.ParseCycle.numToString "Cycle.dot" 15 18 4 1 GLL.ParseCycle.tokenToNumber GLL.ParseCycle.tokenData

    [<Test>]
    member test.``Eps``() =
        let parser = GLL.Eps.buildAst
        runTest parser "Eps.txt" GLL.Eps.numToString "Eps.dot" 19 21 5 0 GLL.Eps.tokenToNumber GLL.Eps.tokenData

    [<Test>]
    member test.``Eps2``() =
        let parser = GLL.Eps2.buildAst
        runTest parser "Eps2.txt" GLL.Eps2.numToString "Eps2.dot" 26 30 6 0 GLL.Eps2.tokenToNumber GLL.Eps2.tokenData

    [<Test>]
    member test.``Epsilon``() =
        let parser = GLL.ParseEpsilon.buildAst
        runTest parser "Epsilon.txt" GLL.ParseEpsilon.numToString "Epsilon.dot" 21 24 5 0 GLL.ParseEpsilon.tokenToNumber GLL.ParseEpsilon.tokenData

    [<Test>]
    member test.``Expr``() =
        let parser = GLL.ParseExpr.buildAst
        runTest parser "Expr.txt" GLL.ParseExpr.numToString "Expr.dot" 36 45 7 1 GLL.ParseExpr.tokenToNumber GLL.ParseExpr.tokenData

    [<Test>]
    member test.``First``() =
        let parser = GLL.ParseFirst.buildAst
        runTest parser "First.txt" GLL.ParseFirst.numToString "First.dot" 21 21 7 0 GLL.ParseFirst.tokenToNumber GLL.ParseFirst.tokenData

//////////////////create input for this test//////////////////
    [<Test>]
    member test.``InfEpsilon``() =
        let parser = GLL.ParseInfEpsilon.buildAst
        runTest parser "Epsilon.txt" GLL.ParseInfEpsilon.numToString "InfEpsilon.dot" 17 21 4 1 GLL.ParseInfEpsilon.tokenToNumber GLL.ParseInfEpsilon.tokenData

    [<Test>]
    member test.``List``() =
        let parser = GLL.ParseList.buildAst
        runTest parser "List.txt" GLL.ParseList.numToString "List.dot" 51 60 11 0 GLL.ParseList.tokenToNumber GLL.ParseList.tokenData

    [<Test>]
    member test.``ListEps``() =
        let parser = GLL.ListEps.buildAst
        runTest parser "ListEps.txt" GLL.ListEps.numToString "ListEps.dot" 33 39 7 0 GLL.ListEps.tokenToNumber GLL.ListEps.tokenData

    [<Test>]
    member test.``Order``() =
        let parser = GLL.ParseOrder.buildAst
        runTest parser "Order.txt" GLL.ParseOrder.numToString "Order.dot" 58 72 10 0 GLL.ParseOrder.tokenToNumber GLL.ParseOrder.tokenData

//    [<Test>]
//    member test.``Lol calc``() =
//        let parser = GLL.ParseLolCalc.buildAst
//        runTest parser "LolCalc.txt" GLL.ParseLolCalc.numToString "LolCalc.dot"

//    [<Test>]
//    member test.``Calc``() =
//        let parser = GLL.ParseCalc.buildAst
//        runTest parser "Calc.txt" GLL.ParseCalc.numToString "Calc.dot"

    [<Test>]
    member test.``Long cycle``() =
        let parser = GLL.ParseLongCycle.buildAst
        runTest parser "LongCycle.txt" GLL.ParseLongCycle.numToString "LongCycle.dot" 14 18 3 1 GLL.ParseLongCycle.tokenToNumber GLL.ParseLongCycle.tokenData
         
    [<Test>]
    member test.``Longest``() =
        let parser = GLL.ParseLongest.buildAst
        runTest parser "Longest.txt" GLL.ParseLongest.numToString "Longest.dot" 91 123 14 1 GLL.ParseLongest.tokenToNumber GLL.ParseLongest.tokenData

    [<Test>]
    member test.``Mixed``() =
        let parser = GLL.Mixed.buildAst
        runTest parser "Mixed.txt" GLL.Mixed.numToString "Mixed.dot" 24 27 6 0 GLL.Mixed.tokenToNumber GLL.Mixed.tokenData

    [<Test>]
    member test.``Omit``() =
        let parser = GLL.ParseOmit.buildAst
        runTest parser "Omit.txt" GLL.ParseOmit.numToString "Omit.dot" 26 30 6 0 GLL.ParseOmit.tokenToNumber GLL.ParseOmit.tokenData

    [<Test>]
    member test.``Simple right null``() =
        let parser = GLL.ParseSimpleRightNull.buildAst
        runTest parser "SimpleRightNull.txt" GLL.ParseSimpleRightNull.numToString "SimpleRightNull.dot" 22 24 6 0 GLL.ParseSimpleRightNull.tokenToNumber GLL.ParseSimpleRightNull.tokenData

    [<Test>]
    member test.``Cond``() =
        let parser = GLL.ParseCond.buildAst
        runTest parser "Cond.txt" GLL.ParseCond.numToString "Cond.dot" 44 57 7 1 GLL.ParseCond.tokenToNumber GLL.ParseCond.tokenData

    [<Test>]
    member test.``Pretty simple calc seq input``() =
        let parser = GLL.PrettySimpleCalc.buildAst
        runTest parser "PrettyCalc1.txt" GLL.PrettySimpleCalc.numToString "PrettySimpleCalc1.dot" 21 24 5 0 GLL.PrettySimpleCalc.tokenToNumber GLL.PrettySimpleCalc.tokenData

   