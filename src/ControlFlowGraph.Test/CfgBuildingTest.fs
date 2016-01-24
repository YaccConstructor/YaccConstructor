module CfgBuildingTest

open Microsoft.FSharp.Collections
open NUnit.Framework
open QuickGraph

open AbstractAnalysis.Common

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures
open ControlFlowGraph.TestHelper

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AbstractParser

open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open YC.FST.GraphBasedFst
open YC.Tests.Helper

let needPrint = false

let createEdge source target label = new ParserEdge<_>(source, target, label)

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let tokenToPos (tokenData : _ -> obj) token = 
    let t = tokenData token
    match t with
    | :? int as i -> [i] |> Seq.ofList
    | _ -> failwith ""

let baseInputGraphsPath = "../../../Tests/ControlFlowGraph/"

//let private quickGraphToFST (lexerInputGraph : AdjacencyGraph<int, TaggedEdge<_,string>>)= 
//    let initialStates = ResizeArray.singleton 0
//    let finishStates = ResizeArray.singleton <| lexerInputGraph.VertexCount - 1
//
//    let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)
//    let transform = 
//        let count = ref -1
//        fun x ->
//            incr count
//            match x with
//            | Smbl(y, _) -> (x, (Smbl !count))
//            | _ -> (x, Eps)
//
//    let transitions = new ResizeArray<_>()
//    lexerInputGraph.Edges
//    |> Seq.iter(fun edge -> transitions.Add(edge.Source, (edge.Tag, edge.Tag), edge.Target))
//
//    let appr = new Appr<_>(initialStates, finishStates, transitions)
//    let fsa = appr.ApprToFSA()
//    FST<_, int>.FSAtoFST(fsa, transform, smblEOF)
//
//let createParserInputGraph name =     
//    let fstInput = quickGraphToFST <| loadDotToQG baseInputGraphsPath name
//
//    let eof = Test.ExtendedCalcParser.RNGLR_EOF <| new FSA<_>()
//    let lexResult = YC.RNGLR.ExtendedCalcLexer.tokenize eof fstInput
//
//    match lexResult with
//    | Success parserInput -> parserInput
//    | Error e -> 
//        Assert.Fail("Lexer error")
//        failwithf "Lexer error"

[<TestFixture>]
type ``Simple cases``() =
    let buildAbstractAst = Test.ExtendedCalcParser.buildAstAbstract
    let tokenToNumber = Test.ExtendedCalcParser.tokenToNumber
    let leftSides = Test.ExtendedCalcParser.leftSide
    let indToString = Test.ExtendedCalcParser.numToString
    let tokenData = Test.ExtendedCalcParser.tokenData

    let semicolon = Test.ExtendedCalcParser.SEMICOLON 0
    let semicolonNumber = tokenToNumber semicolon
    let xNumber = tokenToNumber <| Test.ExtendedCalcParser.X 0
    let yNumber = tokenToNumber <| Test.ExtendedCalcParser.Y 0
    let zNumber = tokenToNumber <| Test.ExtendedCalcParser.Z 0

    let nodeToType = dict ["assign", Assignment;]
        
    let keywordToInt = dict [Keyword.SEMICOLON, semicolonNumber;]

    let tokToRealString tok = tok |> tokenToNumber |> indToString
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let runTest graph expectedBlocksCount expectedNodesCount printNames checks = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error(num, tok, err) -> printErr(num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success(mAst) ->
            if needPrint
            then
                let astName = fst printNames
                Test.ExtendedCalcParser.defaultAstToDot mAst astName

            let cfg = ControlFlow (mAst, parserSource, langSource, tokToRealString)
            
            if needPrint
            then
                let cfgName = snd printNames
                cfg.PrintToDot cfgName
            
            Assert.IsTrue(cfg.Entry.Parents.IsEmpty, "Entry node has parent node!")
            Assert.IsTrue(cfg.Exit.Children.IsEmpty, "Exit node has child node!")
            
            Assert.AreEqual(expectedBlocksCount, cfg.Blocks.Length, "Blocks count isn't equal expected one")
            Assert.AreEqual(expectedNodesCount, cfg.Nodes.Length, "Intermediate nodes count isn't equal expected one")

            let checkCondition condition = 
                let res = condition cfg.Blocks
                Assert.True(res, "Incorrect cfg was built")

            checks
            |> Seq.iter checkCondition
            
    [<Test>]
    member test.``Elementary test``() =
        let qGraph = new ParserInputGraph<_>(0, 13)
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
                createEdge 8 9 (Test.ExtendedCalcParser.Z 8)
                createEdge 9 10 (Test.ExtendedCalcParser.ASSIGN 9)
                createEdge 10 11 (Test.ExtendedCalcParser.NUMBER 10)
                createEdge 11 12 (Test.ExtendedCalcParser.SEMICOLON 11)
                createEdge 12 13 (Test.ExtendedCalcParser.RNGLR_EOF 12)
            ] |> ignore
        //let qGraph = createParserInputGraph "Seq.dot"
        //qGraph.PrintToDot "see.dot" tokToRealString
        let nodeToChildren = dict [xNumber, [yNumber]; yNumber, [zNumber]; zNumber, [];]
        let myChildrenCheck = checkChildren tokenToNumber nodeToChildren

        let nodeToParents = dict [xNumber, []; yNumber, [xNumber]; zNumber, [yNumber];]
        let myParentsCheck = checkParent tokenToNumber nodeToParents

        let myConds = [myChildrenCheck; myParentsCheck]
        
        let expectedNodes = 4
        let expectedBlocks = 3
        let printNames = "`elementary ast.dot", "`elementary cfg.dot"
        runTest qGraph expectedBlocks expectedNodes printNames myConds

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = new ParserInputGraph<_>(0, 16)
        //          -> Y = 2;
        // X = 1;                -> X = 4;
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
                createEdge 12 13 (Test.ExtendedCalcParser.ASSIGN 9)
                createEdge 13 14 (Test.ExtendedCalcParser.NUMBER 10)
                createEdge 14 15 (Test.ExtendedCalcParser.SEMICOLON 11)
                createEdge 15 16 (Test.ExtendedCalcParser.RNGLR_EOF 12)
            ] |> ignore

        let expectedNodes = 4
        let expectedBlocks = 4

        let blockToChildren = dict [yNumber, [xNumber]; zNumber, [xNumber];]
        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict [yNumber, [xNumber]; zNumber, [xNumber];]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents';]

        let printNames = "`ambiguous ast.dot", "`ambiguous cfg.dot"
        runTest qGraph expectedBlocks expectedNodes printNames myChecks

    [<Test>]
    member this.``Ambiguous2 test``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 0 1 (Test.ExtendedCalcParser.Y 1)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 2)
                createEdge 2 3 (Test.ExtendedCalcParser.NUMBER 3)
                createEdge 3 4 (Test.ExtendedCalcParser.SEMICOLON 4)
                createEdge 4 5 (Test.ExtendedCalcParser.RNGLR_EOF 5)
            ] |> ignore

        let expectedNodes = 2
        let expectedBlocks = 1

        let blockToChildren = dict [yNumber, []; zNumber, [];]
        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict [yNumber, []; zNumber, [];]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']

        let printNames = "`ambiguous2 ast.dot", "`ambiguous2 cfg.dot"
        runTest qGraph expectedBlocks expectedNodes  printNames myChecks

[<TestFixture>]
type ``If statements`` () =
    let buildAbstractAst = Test.IfParser.buildAstAbstract
    let tokenToNumber = Test.IfParser.tokenToNumber
    let leftSides = Test.IfParser.leftSide
    let indToString = Test.IfParser.numToString
    let tokenData = Test.IfParser.tokenData

    let semicolonNumber = tokenToNumber <| Test.IfParser.SEMICOLON 0
    let ifNumber = tokenToNumber <| Test.IfParser.IF 0
    let thenNumber = tokenToNumber <| Test.IfParser.THEN 0
    let elseNumber = tokenToNumber <| Test.IfParser.ELSE 0
    let endIfNumber = tokenToNumber <| Test.IfParser.ENDIF 0
    
    let nodeToType = dict[
                                "simple_statement", Assignment;
                                "if_statement", IfStatement;
                          ]
    

    let tokToRealString tok = tok |> tokenToNumber |> indToString

    let keywordToInt = dict [
                                    Keyword.SEMICOLON, semicolonNumber;
                                    Keyword.IF, ifNumber;
                                    Keyword.THEN, thenNumber;
                                    Keyword.ELSE, elseNumber;
                                    Keyword.ENDIF, endIfNumber; 
                            ]

    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let runTest graph expectedBlocksCount expectedNodesCount printNames = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            
            if needPrint
            then
                let astName = fst printNames
                Test.IfParser.defaultAstToDot mAst astName
            
            let cfg = ControlFlow (mAst, parserSource, langSource, tokToRealString)
            
            if needPrint
            then
                let cfgName = snd printNames
                cfg.PrintToDot cfgName
            
            Assert.IsTrue(cfg.Entry.Parents.IsEmpty, "Entry node has parent node!")
            Assert.IsTrue(cfg.Exit.Children.IsEmpty, "Exit node has child node!")

            Assert.AreEqual(expectedBlocksCount, cfg.Blocks.Length, "Blocks count isn't equal expected one")
            Assert.AreEqual(expectedNodesCount, cfg.Nodes.Length, "Intermediate nodes count isn't equal expected one")


    [<Test>]
    member test.``Simple If test``() =
        let qGraph = new ParserInputGraph<_>(0, 12)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.IfParser.IF 0)
                createEdge 1 2 (Test.IfParser.A 1)
                createEdge 2 3 (Test.IfParser.THEN 2)
                createEdge 3 4 (Test.IfParser.B 3)
                createEdge 4 5 (Test.IfParser.SEMICOLON 4)
                createEdge 5 6 (Test.IfParser.ELSE 5)
                createEdge 6 7 (Test.IfParser.C 6)
                createEdge 7 8 (Test.IfParser.SEMICOLON 7)
                createEdge 8 9 (Test.IfParser.ENDIF 8)
                createEdge 9 10 (Test.IfParser.D 9)
                createEdge 10 11 (Test.IfParser.SEMICOLON 10)
                createEdge 11 12 (Test.IfParser.RNGLR_EOF 11)
            ] |> ignore

        let printNames = "`simple if ast.dot", "`simple if cfg.dot"
        let expectedBlocksCount = 4
        let expectedNodesCount = 5
        runTest qGraph expectedBlocksCount expectedNodesCount printNames

    [<Test>]
    member test.``Big If test``() =
        let qGraph = new ParserInputGraph<_>(0, 16)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.IfParser.IF 0)
                createEdge 1 2 (Test.IfParser.A 1)
                createEdge 2 3 (Test.IfParser.THEN 2)
                createEdge 3 4 (Test.IfParser.B 3)
                createEdge 4 5 (Test.IfParser.SEMICOLON 4)
                createEdge 5 6 (Test.IfParser.C 5)
                createEdge 6 7 (Test.IfParser.SEMICOLON 6)
                createEdge 7 8 (Test.IfParser.ELSE 7)
                createEdge 8 9 (Test.IfParser.D 8)
                createEdge 9 10 (Test.IfParser.SEMICOLON 9)
                createEdge 10 11 (Test.IfParser.E 10)
                createEdge 11 12 (Test.IfParser.SEMICOLON 11)
                createEdge 12 13 (Test.IfParser.ENDIF 12)
                createEdge 13 14 (Test.IfParser.F 13)
                createEdge 14 15 (Test.IfParser.SEMICOLON 14)
                createEdge 15 16 (Test.IfParser.RNGLR_EOF 15)
            ] |> ignore

        let printNames = "`big if ast.dot", "`big if cfg.dot"
        let expectedBlocksCount = 6
        let expectedNodesCount = 7
        runTest qGraph expectedBlocksCount expectedNodesCount printNames

    [<Test>]
    member test.``If without else test``() =
        let qGraph = new ParserInputGraph<_>(0, 11)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.IfParser.IF 0)
                createEdge 1 2 (Test.IfParser.A 1)
                createEdge 2 3 (Test.IfParser.THEN 2)
                createEdge 3 4 (Test.IfParser.B 3)
                createEdge 4 5 (Test.IfParser.SEMICOLON 4)
                createEdge 5 6 (Test.IfParser.C 5)
                createEdge 6 7 (Test.IfParser.SEMICOLON 6)
                createEdge 7 8 (Test.IfParser.ENDIF 7)
                createEdge 8 9 (Test.IfParser.D 8)
                createEdge 9 10 (Test.IfParser.SEMICOLON 9)
                createEdge 10 11 (Test.IfParser.RNGLR_EOF 10)
            ] |> ignore

        let printNames = "`if-without-else ast.dot", "`if-without-else cfg.dot"
        let expectedBlocksCount = 4
        let expectedNodesCount = 5
        runTest qGraph expectedBlocksCount expectedNodesCount printNames
            
    [<Test>]
    member test.``Inner if``() =
        let qGraph = new ParserInputGraph<_>(0, 24)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1  (Test.IfParser.IF 0)
                createEdge 1 2  (Test.IfParser.A 1)
                createEdge 2 3  (Test.IfParser.THEN 2)
                createEdge 3 4  (Test.IfParser.IF 3)
                createEdge 4 5  (Test.IfParser.B 4)
                createEdge 5 6  (Test.IfParser.THEN 5)
                createEdge 6 7  (Test.IfParser.C 6)
                createEdge 7 8  (Test.IfParser.SEMICOLON 7)
                createEdge 8 9  (Test.IfParser.ELSE 8)
                createEdge 9 10 (Test.IfParser.D 9)
                createEdge 10 11(Test.IfParser.SEMICOLON 10)
                createEdge 11 12(Test.IfParser.ENDIF 11)
                createEdge 12 13(Test.IfParser.ELSE 12)
                createEdge 13 14(Test.IfParser.IF 13)
                createEdge 14 15(Test.IfParser.E 14)
                createEdge 15 16(Test.IfParser.THEN 15)
                createEdge 16 17(Test.IfParser.F 16)
                createEdge 17 18(Test.IfParser.SEMICOLON 17)
                createEdge 18 19(Test.IfParser.ELSE 18)
                createEdge 19 20(Test.IfParser.G 19)
                createEdge 20 21(Test.IfParser.SEMICOLON 20)
                createEdge 21 22(Test.IfParser.ENDIF 21)
                createEdge 22 23(Test.IfParser.ENDIF 22)
                createEdge 23 24(Test.IfParser.RNGLR_EOF 23)
            ] |> ignore

        let printNames = "`inner if ast.dot", "`inner if cfg.dot"
        let expectedBlocksCount = 7
        let expectedNodesCount = 8
        runTest qGraph expectedBlocksCount expectedNodesCount printNames

[<TestFixture>]
type ``Cycles``() = 
    let buildAbstractAst = Test.SimpleParser.buildAstAbstract
    let tokenToNumber = Test.SimpleParser.tokenToNumber
    let leftSides = Test.SimpleParser.leftSide
    let indToString = Test.SimpleParser.numToString
    let tokenData = Test.SimpleParser.tokenData

    let semicolon = Test.SimpleParser.SEMICOLON 0
    let semicolonNumber = tokenToNumber semicolon
    let nodeToType = dict["assign", Assignment;]

    let aNumber = tokenToNumber <| Test.SimpleParser.A 0
    let bNumber = tokenToNumber <| Test.SimpleParser.B 0
    let cNumber = tokenToNumber <| Test.SimpleParser.C 0
        
    let keywordToInt = dict [Keyword.SEMICOLON, semicolonNumber;]

    let tokToRealString tok = tok |> tokenToNumber |> indToString
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let runTest graph expectedBlocksCount expectedNodesCount checkEntryNode checkExitNode printNames = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            if needPrint
            then
                let astName = fst printNames
                Test.SimpleParser.defaultAstToDot mAst astName

            let cfg = ControlFlow (mAst, parserSource, langSource, tokToRealString)
            
            if needPrint
            then
                let cfgName = snd printNames
                cfg.PrintToDot cfgName
            
            Assert.IsTrue(cfg.Entry.Parents.IsEmpty, "Entry node has parent node!")
            Assert.IsTrue(cfg.Exit.Children.IsEmpty, "Exit node has child node!")

            Assert.AreEqual(expectedBlocksCount, cfg.Blocks.Length, "Blocks count isn't equal expected one")
            Assert.AreEqual(expectedNodesCount, cfg.Nodes.Length, "Intermediate nodes count isn't equal expected one")

            let isCorrect = checkExitNode cfg.Exit
            Assert.IsTrue (isCorrect, "Incorrect cfg was builded")

    [<Test>]
    member this.``Cycle A+``() = 
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 1 0 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.RNGLR_EOF 2)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle A+ input.dot" tokToRealString

        let expectedNodes = 3
        let expectedBlocks = 4
        let alwaysTrue =  (fun _ -> true)

        let checkEntryNode' = checkEntryNode tokenToNumber alwaysTrue
        let checkExitNode' = checkExitNode tokenToNumber alwaysTrue

        let printNames = "`Cycle A+ ast.dot", "`Cycle A+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

    [<Test>]
    member this.``Cycle A B*``() = 
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.B 2)
                createEdge 3 2 (Test.SimpleParser.SEMICOLON 3)
                createEdge 2 4 (Test.SimpleParser.RNGLR_EOF 4)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle A B asteriks input.dot" tokToRealString

        let expectedNodes = 3
        let expectedBlocks = 2

        let firstBlocks = [|aNumber|]
        let lastBlocks = [|aNumber; bNumber|]

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num))

        let entryCond = myCond firstBlocks
        let exitCond = myCond lastBlocks

        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        let checkExitNode' = checkExitNode tokenToNumber exitCond
        let printNames = "`Cycle A B asteriks ast.dot", "`Cycle A B asteriks cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

    [<Test>]
    member this.``Cycle A B* C``() = 
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.B 2)
                createEdge 3 2 (Test.SimpleParser.SEMICOLON 3)
                createEdge 2 4 (Test.SimpleParser.C 4)
                createEdge 4 5 (Test.SimpleParser.SEMICOLON 5)
                createEdge 5 6 (Test.SimpleParser.RNGLR_EOF 6)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle A B asteriks C input.dot" tokToRealString

        let expectedNodes = 3
        let expectedBlocks = 3

        let firstBlocks = [|aNumber|]
        let lastBlocks = [|cNumber|]

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num))
        
        let entryCond = myCond firstBlocks
        let exitCond = myCond lastBlocks

        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let printNames = "`Cycle A B asteriks C ast.dot", "`Cycle A B asteriks C cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

    [<Test>]
    member this.``Cycle (A | B)+``() = 
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 0 1 (Test.SimpleParser.B 1)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 2)
                createEdge 1 0 (Test.SimpleParser.SEMICOLON 2)
                createEdge 2 3 (Test.SimpleParser.RNGLR_EOF 3)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle (A or B)+ input.dot" tokToRealString

        let expectedNodes = 3
        let expectedBlocks = 4

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num))

        let firstBlocks = [| aNumber; bNumber |]
        let lastBlocks = [| aNumber; bNumber |]

        let entryCond = myCond firstBlocks
        let exitCond = myCond lastBlocks

        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let printNames = "`Cycle (A or B)+ ast.dot", "`Cycle (A or B)+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

    [<Test>]
    member this.``Cycle A (B+ | C+)``() = 
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 1 4 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.B 2)
                createEdge 3 2 (Test.SimpleParser.SEMICOLON 3)
                createEdge 3 6 (Test.SimpleParser.SEMICOLON 3)
                createEdge 4 5 (Test.SimpleParser.C 4)
                createEdge 5 4 (Test.SimpleParser.SEMICOLON 5)
                createEdge 5 6 (Test.SimpleParser.SEMICOLON 5)
                createEdge 6 7 (Test.SimpleParser.RNGLR_EOF 6)
            ] |> ignore
        if needPrint 
        then qGraph.PrintToDot "`Cycle A (B+ or C+) input.dot" tokToRealString

        let expectedNodes = 4
        let expectedBlocks = 6

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num)) 

        let firstBlocks = [|aNumber|]
        let lastBlocks = [|bNumber; cNumber|]

        let entryCond = myCond firstBlocks
        let exitCond = myCond lastBlocks

        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let printNames = "`Cycle A (B+ or C+) ast.dot", "`Cycle A (B+ or C+) cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames


    [<Test>]
    member this.``Cycle (AB)+``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.B 2)
                createEdge 3 0 (Test.SimpleParser.SEMICOLON 3)
                createEdge 3 4 (Test.SimpleParser.SEMICOLON 3)
                createEdge 4 5 (Test.SimpleParser.RNGLR_EOF 4)
            ] |> ignore
        if needPrint 
        then qGraph.PrintToDot "`Cycle (AB)+ input.dot" tokToRealString

        let expectedNodes = 4
        let expectedBlocks = 4

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num)) 

        let firstBlocks = [|aNumber|]
        let lastBlocks  = [|bNumber|]

        let entryCond = myCond firstBlocks
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond

        let exitCond = myCond lastBlocks
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let printNames = "`Cycle (AB)+ ast.dot", "`Cycle (AB)+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

    [<Test>]
    member this.``Cycle (AB)+C``() = 
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.B 2)
                createEdge 3 0 (Test.SimpleParser.SEMICOLON 3)
                createEdge 3 4 (Test.SimpleParser.SEMICOLON 3)
                createEdge 4 5 (Test.SimpleParser.C 4)
                createEdge 5 6 (Test.SimpleParser.SEMICOLON 5)
                createEdge 6 7 (Test.SimpleParser.RNGLR_EOF 6)
            ] |> ignore
        if needPrint 
        then qGraph.PrintToDot "`Cycle (AB)+C input.dot" tokToRealString
        let expectedNodes = 5
        let expectedBlocks = 5

        let cNumber = tokenToNumber <| Test.SimpleParser.C 0

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num))

        let firstBlocks = [|aNumber|]
        let lastBlocks =  [|cNumber|]

        let entryCond = myCond firstBlocks
        let exitCond = myCond lastBlocks

        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let printNames = "`Cycle (AB)+C ast.dot", "`Cycle (AB)+C cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

    [<Test>]
    member this.``Cycle after cycle A+B+``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 1 0 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.B 2)
                createEdge 3 2 (Test.SimpleParser.SEMICOLON 3)
                createEdge 3 4 (Test.SimpleParser.SEMICOLON 3)
                createEdge 4 5 (Test.SimpleParser.RNGLR_EOF 4)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle after cycle A+B+ input.dot" tokToRealString

        let bNumber = tokenToNumber <| Test.SimpleParser.B 0

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num))

        let firstBlocks = [|aNumber|]
        let lastBlocks =  [|bNumber|]

        let entryCond = myCond firstBlocks
        let exitCond = myCond lastBlocks

        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let expectedNodes = 4
        let expectedBlocks = 6
        let printNames = 
            "`Cycle after cycle A+B+ ast.dot", 
            "`Cycle after cycle A+B+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

    [<Test>]
    member this.``Cycle inside cycle (A+B)+``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 0 (Test.SimpleParser.SEMICOLON 1)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.B 2)
                createEdge 3 0 (Test.SimpleParser.SEMICOLON 3)
                createEdge 3 4 (Test.SimpleParser.SEMICOLON 3)
                createEdge 4 5 (Test.SimpleParser.RNGLR_EOF 4)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle inside cycle (A+B)+ input.dot" tokToRealString

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num))

        let firstBlocks = [|aNumber|]
        let lastBlocks =  [|bNumber|]

        let entryCond = myCond firstBlocks
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond

        let exitCond = myCond lastBlocks
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let expectedNodes = 4
        let expectedBlocks = 6
        let printNames = 
            "`Cycle inside cycle (A+B)+ ast.dot", 
            "`Cycle inside cycle (A+B)+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

    [<Test>]
    member this.``Cycle inside cycle ((AB)+C)+``() = 
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.SimpleParser.A 0)
                createEdge 1 2 (Test.SimpleParser.SEMICOLON 1)
                createEdge 2 3 (Test.SimpleParser.B 2)
                createEdge 3 0 (Test.SimpleParser.SEMICOLON 3)
                createEdge 3 4 (Test.SimpleParser.SEMICOLON 3)
                createEdge 4 5 (Test.SimpleParser.C 4)
                createEdge 5 0 (Test.SimpleParser.SEMICOLON 5)
                createEdge 5 6 (Test.SimpleParser.SEMICOLON 5)
                createEdge 6 7 (Test.SimpleParser.RNGLR_EOF 6)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle inside cycle ((AB)+C)+ input.dot" tokToRealString 

        let myCond expected tokenSet = 
            expected
            |> Array.exists (fun num -> tokenSet |> Array.exists ((=) num))

        let firstBlocks = [|aNumber|]
        let lastBlocks = [| bNumber; cNumber;|]

        let entryCond = myCond firstBlocks
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond

        let exitCond = myCond lastBlocks
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let expectedNodes = 5
        let expectedBlocks = 6
        let printNames = 
            "`Cycle inside cycle ((AB)+C)+ ast.dot", 
            "`Cycle inside cycle ((AB)+C)+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes checkEntryNode' checkExitNode' printNames

[<TestFixture>]
type ``Cycles inside expressions``() = 
    let buildAbstractAst = Test.ExtendedCalcParser.buildAstAbstract
    let tokenToNumber = Test.ExtendedCalcParser.tokenToNumber
    let leftSides = Test.ExtendedCalcParser.leftSide
    let indToString = Test.ExtendedCalcParser.numToString
    let tokenData = Test.ExtendedCalcParser.tokenData

    let semicolon = Test.ExtendedCalcParser.SEMICOLON 0
    let semicolonNumber = tokenToNumber semicolon
    let nodeToType = dict["assign", Assignment;]

    let keywordToInt = dict [Keyword.SEMICOLON, semicolonNumber;]

    let tokToRealString tok = tok |> tokenToNumber |> indToString
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let fst3 (a, _, _) = a
    let snd3 (_, b, _) = b
    let thr3 (_, _, c) = c

    let runTest (qGraph : ParserInputGraph<_>) expectedTokensCount printNames = 
        if needPrint
        then qGraph.PrintToDot <| fst3 printNames <| tokToRealString
        
        match buildAbstractAst qGraph with
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success tree ->
            if needPrint
            then
                Test.ExtendedCalcParser.defaultAstToDot tree <| snd3 printNames

            let cfg = ControlFlow(tree, parserSource, langSource, tokToRealString)
            
            if needPrint
            then cfg.PrintToDot <| thr3 printNames

            Assert.AreEqual(1, cfg.Blocks.Length)
            let innerGraph = cfg.Blocks.[0].TokensGraph
            let toksCount = innerGraph.GetAvailableTokens() |> Seq.length
            Assert.AreEqual(expectedTokensCount, toksCount)
                
    [<Test>]
    member this.``X = Y [+1]*``() = 
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 1)
                createEdge 2 3 (Test.ExtendedCalcParser.Y 2)
                createEdge 3 4 (Test.ExtendedCalcParser.PLUS 3)
                createEdge 4 3 (Test.ExtendedCalcParser.NUMBER 4)
                createEdge 3 5 (Test.ExtendedCalcParser.SEMICOLON 5)
                createEdge 5 6 (Test.ExtendedCalcParser.RNGLR_EOF 6)
            ] |> ignore

        let printNames = 
            "`X = Y [+1] input.dot", 
            "`X = Y [+1] ast.dot", 
            "`X = Y [+1] cfg.dot"
        
        runTest qGraph 6 printNames

    [<Test>]
    member this.``X = Y [+1]* - Z``() = 
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 1)
                createEdge 2 3 (Test.ExtendedCalcParser.Y 2)
                createEdge 3 4 (Test.ExtendedCalcParser.PLUS 3)
                createEdge 4 3 (Test.ExtendedCalcParser.NUMBER 4)
                createEdge 3 5 (Test.ExtendedCalcParser.MINUS 5)
                createEdge 5 6 (Test.ExtendedCalcParser.Z 6)
                createEdge 6 7 (Test.ExtendedCalcParser.SEMICOLON 7)
                createEdge 7 8 (Test.ExtendedCalcParser.RNGLR_EOF 8)
            ] |> ignore

        let printNames = 
            "`X = Y [+1] - Z input.dot", 
            "`X = Y [+1] - Z ast.dot", 
            "`X = Y [+1] - Z cfg.dot"
        
        runTest qGraph 8 printNames

    [<Test>]
    member this.``X = Y [+1[-Z]*]*``() = 
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 1)
                createEdge 2 3 (Test.ExtendedCalcParser.Y 2)
                createEdge 3 4 (Test.ExtendedCalcParser.PLUS 3)
                createEdge 4 3 (Test.ExtendedCalcParser.NUMBER 4)
                createEdge 4 5 (Test.ExtendedCalcParser.Z 5)
                createEdge 5 4 (Test.ExtendedCalcParser.MINUS 6)
                createEdge 3 6 (Test.ExtendedCalcParser.SEMICOLON 7)
                createEdge 6 7 (Test.ExtendedCalcParser.RNGLR_EOF 8)
            ] |> ignore

        let printNames = 
            "`X = Y [+1[-Z]] input.dot",
            "`X = Y [+1[-Z]] ast.dot", 
            "`X = Y [+1[-Z]] cfg.dot"
        
        runTest qGraph 8 printNames

    [<Test>]
    member this.``X = Y [(+1) | (-Z)]*``() = 
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (Test.ExtendedCalcParser.X 0)
                createEdge 1 2 (Test.ExtendedCalcParser.ASSIGN 1)
                createEdge 2 3 (Test.ExtendedCalcParser.Y 2)
                createEdge 3 4 (Test.ExtendedCalcParser.PLUS 3)
                createEdge 4 3 (Test.ExtendedCalcParser.NUMBER 4)
                createEdge 3 5 (Test.ExtendedCalcParser.MINUS 5)
                createEdge 5 3 (Test.ExtendedCalcParser.Z 6)
                createEdge 3 6 (Test.ExtendedCalcParser.SEMICOLON 7)
                createEdge 6 7 (Test.ExtendedCalcParser.RNGLR_EOF 8)
            ] |> ignore

        let printNames = 
            "`X = Y [(+1) | (-Z)] input.dot",
            "`X = Y [+1[-Z]] ast.dot", 
            "`X = Y [+1[-Z]] cfg.dot"
        
        runTest qGraph 8 printNames



//[<EntryPoint>]
let f x = 
    //let cycleInsideExpress = new ``Control Flow Graph building: Cycles inside expressions``()
    //cycleInsideExpress.``X = Y [+1]*``()

    let cfgBuilding = new ``Simple cases``()
    cfgBuilding.``Elementary test``()
    //cfgBuilding.``Ambiguous2 test``()
    
    //let cycleBuilding = new ``Control Flow Graph building: Cycles``()
    //cycleBuilding.``Cycle (A | B)+``()
    //cycleBuilding.``Cycle inside cycle ((AB)+C)+``()
    //cycleBuilding.``Simple Cycle``()
    //cycleBuilding.Cycle()
//    cfgBuilding.``Ambiguous test``()
//    cfgBuilding.``Ambiguous2 test``()
    //let ifBuilding = new ``Control Flow Graph building: If statements``()
//    ifBuilding.``Simple If test``()
    //let undefVariables = new ``Find undefined variables``()
//    undefVariables.``Undef: ambiguous``()
    1