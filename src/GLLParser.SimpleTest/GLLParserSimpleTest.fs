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

let runTest parser input (intToString : int -> string) (outFileName : string) nodesCount edgesCount termsCount ambiguityCount = 
    let path = dir + input
    let r = run path parser
    match r with
        | Error _ ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            let n, e, t, a = tree.CountCounters 
            printfn "%d %d %d %d" n e t a
            Assert.AreEqual(nodesCount, n, "Nodes count mismatch")
            Assert.AreEqual(edgesCount, e, "Edges count mismatch")
            Assert.AreEqual(termsCount, t, "Terms count mismatch")
            Assert.AreEqual(ambiguityCount, a, "Ambiguities count mismatch")
            //tree.AstToDot intToString (outDir + outFileName)
            Assert.Pass()
            
            

[<TestFixture>]
type ``GLL parser tests with simple lexer`` () =

    [<Test>]
    member test.``Bad left rec``() =
        let parser = GLL.BadLeftRecursion.buildAst
        runTest parser "BBB.txt" GLL.BadLeftRecursion.numToString "BadLeftRec.dot" 30 42 4 1

    [<Test>]
    member test.``SimpleAmb``() =
        let parser = GLL.SimpleAmb.buildAst
        runTest parser "SimpleAmb.txt" GLL.SimpleAmb.numToString "SimpleAmb.dot" 15 18 4 1


    [<Test>]
    member test.``Simple left rec``() =
        let parser = GLL.SimpleLeftRecursion.buildAst
        runTest parser "BBB.txt" GLL.SimpleLeftRecursion.numToString "SimpleLeftRec.dot" 16 18 4 0

    [<Test>]
    member test.``Attrs``() =
        let parser = GLL.ParseAttrs.buildAst
        runTest parser "Attrs.txt" GLL.ParseAttrs.numToString "Attrs.dot" 26 30 6 0

    [<Test>]
    member test.``Brackets``() =
        let parser = GLL.Brackets.buildAst
        runTest parser "Brackets.txt" GLL.Brackets.numToString "Brackets.dot" 43 51 10 1

    [<Test>]
    member test.``Simple right rec``() =
        let parser = GLL.SimpleRightRecursion.buildAst
        runTest parser "BBB.txt" GLL.SimpleRightRecursion.numToString "SimpleRightRec.dot" 12 12 4 0

    [<Test>]
    member test.``Complex right null``() =
        let parser = GLL.ParseComplexRightNull.buildAst
        runTest parser "ComplexRightNull.txt" GLL.ParseComplexRightNull.numToString "ComplexRightNull.dot" 32 39 7 1

    [<Test>]
    member test.``Counter``() =
        let parser = GLL.ParseCounter.buildAst
        runTest parser "Counter.txt" GLL.ParseCounter.numToString "Counter.dot" 18 18 6 0

    [<Test>]
    member test.``Cycle``() =
        let parser = GLL.ParseCycle.buildAst
        runTest parser "Cycle.txt" GLL.ParseCycle.numToString "Cycle.dot" 12 15 3 1

    [<Test>]
    member test.``Eps``() =
        let parser = GLL.Eps.buildAst
        runTest parser "Eps.txt" GLL.Eps.numToString "Eps.dot" 16 18 4 0

    [<Test>]
    member test.``Eps2``() =
        let parser = GLL.Eps2.buildAst
        runTest parser "Eps2.txt" GLL.Eps2.numToString "Eps2.dot" 23 27 5 0

    [<Test>]
    member test.``Epsilon``() =
        let parser = GLL.ParseEpsilon.buildAst
        runTest parser "Epsilon.txt" GLL.ParseEpsilon.numToString "Epsilon.dot" 18 21 4 0

    [<Test>]
    member test.``Expr``() =
        let parser = GLL.ParseExpr.buildAst
        runTest parser "Expr.txt" GLL.ParseExpr.numToString "Expr.dot" 33 42 6 1

    [<Test>]
    member test.``First``() =
        let parser = GLL.ParseFirst.buildAst
        runTest parser "First.txt" GLL.ParseFirst.numToString "First.dot" 18 18 6 0

//////////////////create input for this test//////////////////
    [<Test>]
    member test.``InfEpsilon``() =
        let parser = GLL.ParseInfEpsilon.buildAst
        runTest parser "Epsilon.txt" GLL.ParseInfEpsilon.numToString "InfEpsilon.dot" 14 18 3 1

    [<Test>]
    member test.``List``() =
        let parser = GLL.ParseList.buildAst
        runTest parser "List.txt" GLL.ParseList.numToString "List.dot" 48 57 10 0

    [<Test>]
    member test.``ListEps``() =
        let parser = GLL.ListEps.buildAst
        runTest parser "ListEps.txt" GLL.ListEps.numToString "ListEps.dot"

    [<Test>]
    member test.``Order``() =
        let parser = GLL.ParseOrder.buildAst
        runTest parser "Order.txt" GLL.ParseOrder.numToString "Order.dot"

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
        runTest parser "LongCycle.txt" GLL.ParseLongCycle.numToString "LongCycle.dot" 11 15 2 1
         
    [<Test>]
    member test.``Longest``() =
        let parser = GLL.ParseLongest.buildAst
        runTest parser "Longest.txt" GLL.ParseLongest.numToString "Longest.dot" 88 120 13 1

    [<Test>]
    member test.``Mixed``() =
        let parser = GLL.Mixed.buildAst
        runTest parser "Mixed.txt" GLL.Mixed.numToString "Mixed.dot" 21 24 5 0

    [<Test>]
    member test.``Omit``() =
        let parser = GLL.ParseOmit.buildAst
        runTest parser "Omit.txt" GLL.ParseOmit.numToString "Omit.dot" 23 27 5 0

    [<Test>]
    member test.``Simple right null``() =
        let parser = GLL.ParseSimpleRightNull.buildAst
        runTest parser "SimpleRightNull.txt" GLL.ParseSimpleRightNull.numToString "SimpleRightNull.dot" 19 21 5 0

    [<Test>]
    member test.``Cond``() =
        let parser = GLL.ParseCond.buildAst
        runTest parser "Cond.txt" GLL.ParseCond.numToString "Cond.dot" 41 54 6 1

    [<Test>]
    member test.``Pretty simple calc seq input``() =
        let parser = GLL.PrettySimpleCalc.buildAst
        runTest parser "PrettyCalc1.txt" GLL.PrettySimpleCalc.numToString "PrettySimpleCalc1.dot" 18 21 4 0

   