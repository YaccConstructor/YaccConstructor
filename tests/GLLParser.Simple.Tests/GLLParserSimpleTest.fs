module GLLParserSimpleTest

open AbstractAnalysis.Common

open Yard.Generators.GLL
//open Yard.Generators.GLL.Parser 
open NUnit.Framework
open Yard.Generators
open LexCommon
open Microsoft.FSharp.Collections

open YaccConstructor.API
open Yard.Generators.GLL.ParserCommon
open System.Collections.Generic
open Yard.Generators.GLL.AbstractParser
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Core.Conversions.ExpandMeta

//let run path astBuilder =
//    let tokens = LexCommon.tokens(path)
//    astBuilder tokens

let inputFilesPath = (__SOURCE_DIRECTORY__ + @"\..\data\GLL\")

let grammarFilesPath = __SOURCE_DIRECTORY__ + @"\"

let getTokens path =
    System.IO.File.ReadAllText(inputFilesPath + path)
        .Split([|' '|])
        |> Array.filter ((<>) "")

let getLinearInput path (stringToToken : string -> int<token>) = 
    new LinearInput(
            getTokens path
            |> Array.map stringToToken
            )
let getLinearInputWithAllStartingPos path (stringToToken : string -> int<token>) = 
    let tokens = 
        getTokens path
            |> Array.map stringToToken
    let startPoss = 
        tokens
        |> Array.mapi(fun i x -> i* 1<positionInInput>)
    new LinearInput(startPoss,
            getTokens path
            |> Array.map stringToToken
            )

let isParsed parserSource input = 
    Yard.Generators.GLL.AbstractParser.isParsed parserSource input

let shouldBeTrue res = 
    Assert.AreEqual(res, true, "Not parsed")        

         
let getParserSource grammarFile = 
    let fe = new YardFrontend()
    let gen = new GLL()
    let conv = seq{yield new ExpandMeta()}
    generate (grammarFilesPath + grammarFile)
             fe gen 
             None
             conv
             [|""|]
             //[ "ExpandEbnf"; "ExpandMeta"; "ExpandInnerAlt"; "AddDefaultAC"; "Linearize"]
             [] :?> ParserSourceGLL

let runTest grammarFile inputFile =
    let parser = getParserSource grammarFile
    let input  = getLinearInput inputFile parser.StringToToken
    let res = isParsed parser input
    shouldBeTrue res

let checkAst grammarFile inputFile nodesCount edgesCount termsCount ambiguityCount = 
    let parser = getParserSource grammarFile
    let input  = getLinearInput inputFile parser.StringToToken
    let tree = buildAst parser input
    printfn "%A" tree
    tree.AstToDot (grammarFilesPath + inputFile + ".dot")
    let n, e, t, amb = tree.CountCounters
    printfn "%d %d %d %d" n e t amb
    Assert.AreEqual(nodesCount, n, sprintf "Nodes expected:%i, found:%i. %i %i %i" nodesCount n e t amb)
    Assert.AreEqual(edgesCount, e, sprintf "Edges expected:%i, found:%i." edgesCount e)
    Assert.AreEqual(termsCount, t, sprintf "Terms expected:%i, found:%i." termsCount t) 
    Assert.AreEqual(ambiguityCount, amb, sprintf "Ambiguities expected:%i, found:%i." ambiguityCount amb)
    Assert.Pass()

let checkIntervals grammarFile inputFile (intervals : _[]) = 
    let parser = getParserSource grammarFile
    let input  = getLinearInputWithAllStartingPos inputFile parser.StringToToken
    let ranges = getAllRangesForStartState parser input
    printfn "%A" ranges
    let result = 
        ranges
        |> Seq.fold(fun res x -> intervals |> Array.contains x && res) true
    Assert.True(result)
    Assert.Pass()


[<TestFixture>]
type ``GLL parser tests with simple lexer`` () =

//    [<Test>]
//    member test.``Strange Error``() =
//        //runTest "BadLeftRecursion.yrd" "BBB.txt"
//        checkAst "code.yrd" "code.txt"
//            19 24 3 1
    [<Test>]
    member test.``Brackets2``() =
        //runTest "BadLeftRecursion.yrd" "BBB.txt"
        checkIntervals "Brackets2.yrd" "Brackets2.txt"
            [|(0<positionInInput>, 7<positionInInput>);
              (1<positionInInput>, 6<positionInInput>);
              (2<positionInInput>, 5<positionInInput>);
              (3<positionInInput>, 4<positionInInput>);|]

    [<Test>]
    member test.``Bad left rec``() =
        //runTest "BadLeftRecursion.yrd" "BBB.txt"
        checkAst "BadLeftRecursion.yrd" "BBB.txt"
            19 24 3 1

    [<Test>]
    member test.``SimpleAmb``() =
        checkAst "SimpleAmb.yrd" "SimpleAmb.txt"
            10 11 3 1

    [<Test>]
    member test.``SimpleAmb2``() =
        checkAst "SimpleAmb2.yrd" "SimpleAmb.txt"
            12 13 3 1

    [<Test>]
    member test.``Simple left rec``() =
        checkAst "SimpleLeftRecursion.yrd" "BBB.txt"
            9 8 3 0
    [<Test>]
    member test.``Attrs``() =
        checkAst "Attrs.yrd" "Attrs.txt"
            15 14 5 0

    [<Test>]
    member test.``Brackets``() =
        checkAst "Brackets.yrd" "Brackets.txt"
            34 36 9 1
    [<Test>]
    member test.``Simple right rec``() =
        checkAst "SimpleRightRecursion.yrd" "BBB.txt"
            15 15 3 0
    [<Test>]
    member test.``Complex right null``() =
        checkAst "ComplexRightNull.yrd" "ComplexRightNull.txt"
            32 37 5 1
    [<Test>]
    member test.``Counter``() =
        checkAst "Counter.yrd" "Counter.txt"
            35 40 5 0
    [<Test>]
    member test.``Cycle``() =
        checkAst "Cycle.yrd" "Cycle.txt"
            7 7 2 1
    [<Test>]
    member test.``Eps``() =
        checkAst "Eps.yrd" "Eps.txt"
            10 9 2 0
    [<Test>]
    member test.``Eps2``() =
        checkAst "Eps2.yrd" "Eps2.txt"
            10 9 2 0
//    [<Test>]
//    member test.``Epsilon``() =
//        checkAst "Epsilon.yrd" "Epsilon.txt"
//            15 14 5 0
    [<Test>]
    member test.``Expr``() =
        checkAst "Expr.yrd" "Expr.txt"
            24 27 5 1
    [<Test>]
    member test.``First``() =
        checkAst "First.yrd" "First.txt"
            15 14 5 0
//////////////////create input for this test//////////////////
    [<Test>]
    member test.``InfEpsilon``() =
        checkAst "InfEpsilon.yrd" "Epsilon.txt"
            3 2 1 0
    [<Test>]
    member test.``List``() =
        checkAst "List.yrd" "List.txt"
            37 36 9 0
    [<Test>]
    member test.``ListEps``() =
        checkAst "ListEps.yrd" "ListEps.txt"
            24 24 5 1
    [<Test>]
    member test.``Order``() =
        checkAst "Order.yrd" "Order.txt"
            38 37 8 0
//    [<Test>]
//    member test.``Lol calc``() =
//        checkAst "LolCalc.yrd" "LolCalc.txt"
//            15 14 5 0
//    [<Test>]
//    member test.``Calc``() =
//        runTest ParseCalc.parserSource
//        "Calc.txt" GLL.ParseCalc.stringToNumber
//        let res = isParsed parser input
//        shouldBeTrue res

    [<Test>]
    member test.``Long cycle``() =
        checkAst "LongCycle.yrd" "LongCycle.txt"
             8 8 1 1
    [<Test>]
    member test.``Longest``() =
        checkAst "Longest.yrd" "Longest.txt"
            24 25 6 0
    [<Test>]
    member test.``Mixed``() =
        checkAst "Mixed.yrd" "Mixed.txt"
            16 16 4 0
    [<Test>]
    member test.``Omit``() =
        checkAst "Omit.yrd" "Omit.txt"
            22 20 4 0
    [<Test>]
    member test.``Simple right null``() =
        checkAst "SimpleRightNull.yrd" "SimpleRightNull.txt"
            27 28 5 0
    [<Test>]
    member test.``Cond``() =
        checkAst "Cond.yrd" "Cond.txt"
            42 47 5 1
    [<Test>]
    member test.``Pretty simple calc seq input``() =
        checkAst "PrettySimpleCalc.yrd" "PrettyCalc1.txt"
            15 14 3 0