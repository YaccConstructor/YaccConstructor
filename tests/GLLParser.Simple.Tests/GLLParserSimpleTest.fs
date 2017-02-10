module GLLParserSimpleTest

open AbstractAnalysis.Common

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
//open GLL.ListEps
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
open Yard.Generators.GLL.ParserCommon
open System.Collections.Generic
//let run path astBuilder =
//    let tokens = LexCommon.tokens(path)
//    astBuilder tokens

let dir = @"../../../data/GLL/"
let dirFiles = @"C:/Code/YaccConstructor/tests/GLL.AbstractParser.Simple.Tests/"
//let dir = @"C:/Code/YaccConstructor/tests/data/GLL/"
let outDir = @"../../../src/GLLParser.SimpleTest/"

let getTokens path =
    System.IO.File.ReadAllText(dir + path)
        .Split([|' '|])
        |> Array.filter ((<>) "")

let getLinearInput path (tokenToInt : Dictionary<string,int>) = 
    new LinearInput(
            getTokens path
            |> Array.map (fun x -> tokenToInt.[x] * 1<token>))



//let runTest parser input (intToString : int -> string) (fileName : string) nodesCount edgesCount termsCount ambiguityCount tokToNum tokenData = 
//    let path = dir + input
//    let r = run path parser
//    match r with
//        | Error _ ->
//            printfn "Error"
//            Assert.Fail("Parsing finished with error!")
//        | Success tree ->
//            let n, e, t, a = tree.CountCounters 
//            printfn "%d %d %d %d" n e t a
//            tree.AstToDot intToString  tokToNum tokenData (outDir + fileName)
//            Assert.AreEqual(nodesCount, n, "Nodes count mismatch")
//            Assert.AreEqual(edgesCount, e, "Edges count mismatch")
//            Assert.AreEqual(termsCount, t, "Terms count mismatch")
//            Assert.AreEqual(ambiguityCount, a, "Ambiguities count mismatch")
//            //tree.AstToDot intToString (outDir + outFileName)
//            tree.AstToDot intToString  tokToNum tokenData (outDir + fileName)
//            Assert.Pass()

let isParsed parserSource input = 
    Yard.Generators.GLL.AbstractParserWithoutTree.isParsed parserSource input

let shouldBeTrue res = 
    Assert.AreEqual(res, true, "Nodes count mismatch")        
         
let getParcerSource grammarFile =    
    YaccConstructor.API.generate (dirFiles + grammarFile)
                                 "YardFrontend" "GLLGenerator" 
                                 None
                                 ["ExpandMeta"]
                                 [] :?> ParserSourceGLL
[<TestFixture>]
type ``GLL parser tests with simple lexer`` () =

    [<Test>]
    member test.``Bad left rec``() =
        let parser = getParcerSource "BadLeftRecursion.yrd"
        let input  = getLinearInput "BBB.txt" GLL.BadLeftRecursion.stringToNumber
        let res = isParsed parser input

        shouldBeTrue res

    [<Test>]
    member test.``SimpleAmb``() =
        let parser = GLL.SimpleAmb.parserSource
        let input  = getLinearInput "SimpleAmb.txt" GLL.SimpleAmb.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Simple left rec``() =
        let parser = GLL.SimpleLeftRecursion.parserSource
        let input  = getLinearInput "BBB.txt" GLL.SimpleLeftRecursion.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Attrs``() =
        let parser = GLL.ParseAttrs.parserSource
        let input  = getLinearInput "Attrs.txt" GLL.ParseAttrs.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Brackets``() =
        let parser = GLL.Brackets.parserSource
        let input  = getLinearInput "Brackets.txt" GLL.Brackets.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Simple right rec``() =
        let parser = GLL.SimpleRightRecursion.parserSource
        let input  = getLinearInput "BBB.txt" GLL.SimpleRightRecursion.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Complex right null``() =
        let parser = GLL.ParseComplexRightNull.parserSource
        let input  = getLinearInput "ComplexRightNull.txt" GLL.ParseComplexRightNull.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Counter``() =
        let parser = GLL.ParseCounter.parserSource
        let input  = getLinearInput "Counter.txt" GLL.ParseCounter.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Cycle``() =
        let parser = GLL.ParseCycle.parserSource
        let input  = getLinearInput "Cycle.txt" GLL.ParseCycle.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Eps``() =
        let parser = GLL.Eps.parserSource
        let input  = getLinearInput "Eps.txt" GLL.Eps.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Eps2``() =
        let parser = GLL.Eps2.parserSource
        let input  = getLinearInput "Eps2.txt" GLL.Eps2.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Epsilon``() =
        let parser = GLL.ParseEpsilon.parserSource
        let input  = getLinearInput "Epsilon.txt" GLL.ParseEpsilon.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Expr``() =
        let parser = GLL.ParseExpr.parserSource
        let input  = getLinearInput "Expr.txt" GLL.ParseExpr.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``First``() =
        let parser = GLL.ParseFirst.parserSource
        let input  = getLinearInput "First.txt" GLL.ParseFirst.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

//////////////////create input for this test//////////////////
    [<Test>]
    member test.``InfEpsilon``() =
        let parser = GLL.ParseInfEpsilon.parserSource
        let input  = getLinearInput "Epsilon.txt" GLL.ParseInfEpsilon.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``List``() =
        let parser = GLL.ParseList.parserSource
        let input  = getLinearInput "List.txt" GLL.ParseList.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``ListEps``() =
        let parser = GLL.ListEps.parserSource
        let input  = getLinearInput "ListEps.txt" GLL.ListEps.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Order``() =
        let parser = GLL.ParseOrder.parserSource
        let input  = getLinearInput "Order.txt" GLL.ParseOrder.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Lol calc``() =
        let parser = GLL.ParseLolCalc.parserSource
        let input  = getLinearInput "LolCalc.txt" GLL.ParseLolCalc.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

//    [<Test>]
//    member test.``Calc``() =
//        let parser = GLL.ParseCalc.parserSource
//        let input  = getLinearInput "Calc.txt" GLL.ParseCalc.stringToNumber
//        let res = isParsed parser input
//        shouldBeTrue res

    [<Test>]
    member test.``Long cycle``() =
        let parser = GLL.ParseLongCycle.parserSource
        let input  = getLinearInput "LongCycle.txt" GLL.ParseLongCycle.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res
         
    [<Test>]
    member test.``Longest``() =
        let parser = GLL.ParseLongest.parserSource
        let input  = getLinearInput "Longest.txt" GLL.ParseLongest.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Mixed``() =
        let parser = GLL.Mixed.parserSource
        let input  = getLinearInput "Mixed.txt" GLL.Mixed.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Omit``() =
        let parser = GLL.ParseOmit.parserSource
        let input  = getLinearInput "Omit.txt" GLL.ParseOmit.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Simple right null``() =
        let parser = GLL.ParseSimpleRightNull.parserSource
        let input  = getLinearInput "SimpleRightNull.txt" GLL.ParseSimpleRightNull.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Cond``() =
        let parser = GLL.ParseCond.parserSource
        let input  = getLinearInput "Cond.txt" GLL.ParseCond.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res

    [<Test>]
    member test.``Pretty simple calc seq input``() =
        let parser = GLL.PrettySimpleCalc.parserSource
        let input  = getLinearInput "PrettyCalc1.txt" GLL.PrettySimpleCalc.stringToNumber
        let res = isParsed parser input
        shouldBeTrue res
   