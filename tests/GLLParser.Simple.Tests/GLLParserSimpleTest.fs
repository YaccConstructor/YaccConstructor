module GLLParserSimpleTest

open AbstractAnalysis.Common

open Yard.Generators.GLL
//open Yard.Generators.GLL.Parser 
open NUnit.Framework
open Yard.Generators
open LexCommon
open Microsoft.FSharp.Collections

open Yard.Generators.GLL.ParserCommon
open System.Collections.Generic
//let run path astBuilder =
//    let tokens = LexCommon.tokens(path)
//    astBuilder tokens

let inputFilesPath = @"../../../data/GLL/"
let grammarFilesPath = @"C:/Code/YaccConstructor/tests/GLLParser.Simple.Tests/"
//let inputFilesPath = @"C:/Code/YaccConstructor/tests/data/GLL/"
//let outDir = @"../../../src/GLLParser.SimpleTest/"

let getTokens path =
    System.IO.File.ReadAllText(inputFilesPath + path)
        .Split([|' '|])
        |> Array.filter ((<>) "")

let getLinearInput path (stringToToken : string -> int<token>) = 
    new LinearInput(
            getTokens path
            |> Array.map stringToToken
            )

let isParsed parserSource input = 
    Yard.Generators.GLL.AbstractParser.isParsed parserSource input

let shouldBeTrue res = 
    Assert.AreEqual(res, true, "Not parsed")        

         
let getParserSource grammarFile =    
    YaccConstructor.API.generate (grammarFilesPath + grammarFile)
                                 "YardFrontend" "GLLGenerator" 
                                 None
                                 ["ExpandMeta"]
                                 [] :?> ParserSourceGLL

let runTest grammarFile inputFile =
    let parser = getParserSource grammarFile
    let input  = getLinearInput inputFile parser.StringToToken
    let res = isParsed parser input
    shouldBeTrue res

[<TestFixture>]
type ``GLL parser tests with simple lexer`` () =

    [<Test>]
    member test.``Bad left rec``() =
        runTest "BadLeftRecursion.yrd" "BBB.txt"

    [<Test>]
    member test.``SimpleAmb``() =
        runTest "SimpleAmb.yrd" "SimpleAmb.txt"

    [<Test>]
    member test.``Simple left rec``() =
        runTest "SimpleLeftRecursion.yrd" "BBB.txt"

    [<Test>]
    member test.``Attrs``() =
        runTest "Attrs.yrd" "Attrs.txt"

    [<Test>]
    member test.``Brackets``() =
        runTest "Brackets.yrd" "Brackets.txt"

    [<Test>]
    member test.``Simple right rec``() =
        runTest "SimpleRightRecursion.yrd" "BBB.txt"

    [<Test>]
    member test.``Complex right null``() =
        runTest "ComplexRightNull.yrd" "ComplexRightNull.txt"

    [<Test>]
    member test.``Counter``() =
        runTest "Counter.yrd" "Counter.txt"

    [<Test>]
    member test.``Cycle``() =
        runTest "Cycle.yrd" "Cycle.txt"

    [<Test>]
    member test.``Eps``() =
        runTest "Eps.yrd" "Eps.txt"

    [<Test>]
    member test.``Eps2``() =
        runTest "Eps2.yrd" "Eps2.txt"

    [<Test>]
    member test.``Epsilon``() =
        runTest "Epsilon.yrd" "Epsilon.txt"

    [<Test>]
    member test.``Expr``() =
        runTest "Expr.yrd" "Expr.txt"

    [<Test>]
    member test.``First``() =
        runTest "First.yrd" "First.txt"

//////////////////create input for this test//////////////////
    [<Test>]
    member test.``InfEpsilon``() =
        runTest "InfEpsilon.yrd" "Epsilon.txt"

    [<Test>]
    member test.``List``() =
        runTest "List.yrd" "List.txt"

    [<Test>]
    member test.``ListEps``() =
        runTest "ListEps.yrd" "ListEps.txt"

    [<Test>]
    member test.``Order``() =
        runTest "Order.yrd" "Order.txt"

    [<Test>]
    member test.``Lol calc``() =
        runTest "LolCalc.yrd" "LolCalc.txt"

//    [<Test>]
//    member test.``Calc``() =
//        runTest ParseCalc.parserSource
//        "Calc.txt" GLL.ParseCalc.stringToNumber
//        let res = isParsed parser input
//        shouldBeTrue res

    [<Test>]
    member test.``Long cycle``() =
        runTest "LongCycle.yrd" "LongCycle.txt"
         
    [<Test>]
    member test.``Longest``() =
        runTest "Longest.yrd" "Longest.txt"

    [<Test>]
    member test.``Mixed``() =
        runTest "Mixed.yrd" "Mixed.txt"

    [<Test>]
    member test.``Omit``() =
        runTest "Omit.yrd" "Omit.txt"

    [<Test>]
    member test.``Simple right null``() =
        runTest "SimpleRightNull.yrd" "SimpleRightNull.txt"

    [<Test>]
    member test.``Cond``() =
        runTest "Cond.yrd" "Cond.txt"

    [<Test>]
    member test.``Pretty simple calc seq input``() =
        runTest "PrettySimpleCalc.yrd" "PrettyCalc1.txt"
   