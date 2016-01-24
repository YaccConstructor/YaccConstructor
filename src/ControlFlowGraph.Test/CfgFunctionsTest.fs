module CfgFunctionsTest

open NUnit.Framework
open QuickGraph

open AbstractAnalysis.Common

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures

open Yard.Generators.RNGLR.AbstractParser

let createEdge source target label = new ParserEdge<_>(source, target, label)

let needPrint = false

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

[<TestFixture>]
type ``Find undefined variables`` () =
    
    let buildAbstractAst = Test.ExtendedCalcParser.buildAstAbstract
    let tokenToNumber = Test.ExtendedCalcParser.tokenToNumber
    let leftSides = Test.ExtendedCalcParser.leftSide
    let indToString = Test.ExtendedCalcParser.numToString
    let tokenData = Test.ExtendedCalcParser.tokenData

    let semicolonNumber = tokenToNumber <| Test.ExtendedCalcParser.SEMICOLON 0
    let assignNumber = tokenToNumber <| Test.ExtendedCalcParser.ASSIGN 0

    let nodeToType = dict["assign", Assignment;]
        
    let keywordToInt = dict [
                                Keyword.SEMICOLON, semicolonNumber;
                                Keyword.ASSIGN, assignNumber;
                            ]
        
    let varsNumbers = 
        [Test.ExtendedCalcParser.X 0; Test.ExtendedCalcParser.Y 0; Test.ExtendedCalcParser.Z 0]
        |> List.map tokenToNumber

    let isVariable tok = varsNumbers |> List.exists ((=) tok) 

    let tokToRealName = tokenToNumber >> indToString
        
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt, isVariable)

    let runTest qGraph expected printNames = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            if needPrint
            then
                let astName = fst printNames
                Test.ExtendedCalcParser.defaultAstToDot mAst astName

            let cfg = ControlFlow (mAst, parserSource, langSource, tokToRealName)
            
            if needPrint
            then
                let cfgName = snd printNames
                cfg.PrintToDot cfgName
            
            let errorList = cfg.FindUndefVariable()
            
            if needPrint
            then
                printfn "%A" errorList
                printfn "Expected: %d. Actual: %d." expected errorList.Length 
            
            Assert.AreEqual(expected, errorList.Length)

    [<Test>]
    member test.``Elementary``() = 
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 1)
                createEdge 2 3 (Test.ExtendedCalcParser.Z 2)
                createEdge 3 4 (Test.ExtendedCalcParser.SEMICOLON 3)
                createEdge 4 5 (Test.ExtendedCalcParser.Y 4)
                createEdge 5 6 (Test.ExtendedCalcParser.ASSIGN 5)
                createEdge 6 7 (Test.ExtendedCalcParser.X 6)
                createEdge 7 8 (Test.ExtendedCalcParser.SEMICOLON 7)
                createEdge 8 9 (Test.ExtendedCalcParser.RNGLR_EOF 8)
            ] |> ignore

        let expected = 1
        let printNames = 
            "`cfg undefined variables ast elementary.dot", 
            "`cfg undefined variables cfg elementary.dot"
        runTest qGraph expected printNames

    [<Test>]
    member test.``X = X``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 1)
                createEdge 2 3 (Test.ExtendedCalcParser.X 2)
                createEdge 3 4 (Test.ExtendedCalcParser.SEMICOLON 3)
                createEdge 4 5 (Test.ExtendedCalcParser.RNGLR_EOF 4)
            ] |> ignore

        let expected = 1
        let printNames = 
            "`cfg undefined variables ast X = X.dot", 
            "`cfg undefined variables cfg X = X.dot"
        runTest qGraph expected printNames

    [<Test>]
    member test.``Undef: ambiguous``() =
        let qGraph = new ParserInputGraph<_>(0, 18)
        //          -> Y = 2;
        // X = 1;                -> X = Y * Z;
        //          -> Z = 3;
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 1)
                createEdge 2 3 (Test.ExtendedCalcParser.NUMBER 2)
                createEdge 3 4 (Test.ExtendedCalcParser.SEMICOLON 3)
                createEdge 4 5 (Test.ExtendedCalcParser.Y 4)
                createEdge 5 6 (Test.ExtendedCalcParser.ASSIGN 5)
                createEdge 6 7 (Test.ExtendedCalcParser.NUMBER 6)
                createEdge 7 8 (Test.ExtendedCalcParser.SEMICOLON 7)
                createEdge 4 9 (Test.ExtendedCalcParser.Z 8)
                createEdge 9 10 (Test.ExtendedCalcParser.ASSIGN 9)
                createEdge 10 11 (Test.ExtendedCalcParser.NUMBER 10)
                createEdge 11 8 (Test.ExtendedCalcParser.SEMICOLON 11)
                createEdge 8 12 (Test.ExtendedCalcParser.X 8)
                createEdge 12 13 (Test.ExtendedCalcParser.ASSIGN 12)
                createEdge 13 14 (Test.ExtendedCalcParser.Y 13)
                createEdge 14 15 (Test.ExtendedCalcParser.MULT 14)
                createEdge 15 16 (Test.ExtendedCalcParser.Z 15)
                createEdge 16 17 (Test.ExtendedCalcParser.SEMICOLON 16)
                createEdge 17 18 (Test.ExtendedCalcParser.RNGLR_EOF 17)
            ] |> ignore

        let expected = 2
        let printNames = 
            "`cfg undefined variables ast ambiguous1.dot", 
            "`cfg undefined variables cfg ambiguous1.dot"
        runTest qGraph expected printNames
            
    [<Test>]
    member test.``Undef: ambiguous 2``() =
        let qGraph = new ParserInputGraph<_>(0, 15)
        //        ---> Y = 2; ---> 
        // X = 1; ---------------> X = Y * Z;
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 1)
                createEdge 2 3 (Test.ExtendedCalcParser.NUMBER 2)
                createEdge 3 4 (Test.ExtendedCalcParser.SEMICOLON 3)
                createEdge 4 5 (Test.ExtendedCalcParser.Y 4)
                createEdge 5 6 (Test.ExtendedCalcParser.ASSIGN 5)
                createEdge 6 7 (Test.ExtendedCalcParser.NUMBER 6)
                createEdge 7 8 (Test.ExtendedCalcParser.SEMICOLON 7)
                createEdge 4 9 (Test.ExtendedCalcParser.X 8)
                createEdge 8 9 (Test.ExtendedCalcParser.X 8)
                createEdge 9 10 (Test.ExtendedCalcParser.ASSIGN 9)
                createEdge 10 11 (Test.ExtendedCalcParser.Y 10)
                createEdge 11 12 (Test.ExtendedCalcParser.MULT 11)
                createEdge 12 13 (Test.ExtendedCalcParser.Z 12)
                createEdge 13 14 (Test.ExtendedCalcParser.SEMICOLON 13)
                createEdge 14 15 (Test.ExtendedCalcParser.RNGLR_EOF 14)
            ] |> ignore

        let expected = 2
        let printNames = 
            "`cfg undefined variables ast ambiguous2.dot", 
            "`cfg undefined variables cfg ambiguous2.dot"
        runTest qGraph expected printNames