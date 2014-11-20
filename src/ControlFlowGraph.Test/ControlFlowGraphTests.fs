module ControlFlowGraphTests

open AbstractAnalysis.Common
open QuickGraph
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR.AbstractParser
open ControlFlowGraph
open NUnit.Framework

let createEdge from _to label = new ParserEdge<_>(from, _to, label)

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let tokenToPos (tokenData : _ -> obj) token = 
    let t = tokenData token
    match t with
    | :? int as i -> [i] |> Seq.ofList
    | _ -> failwith ""

[<TestFixture>]
type ``Control Flow Graph Building`` () =

    [<Test>]
    member test.``Elementary test``() =
        let buildAbstractAst = RNGLR.ParseExtendedCalc.buildAstAbstract
        let tokenToNumber = RNGLR.ParseExtendedCalc.tokenToNumber
        let leftSides = RNGLR.ParseExtendedCalc.leftSide
        let indToString = RNGLR.ParseExtendedCalc.numToString
        let tokenData = RNGLR.ParseExtendedCalc.tokenData

        let semicolon = RNGLR.ParseExtendedCalc.SEMICOLON 0
        let semicolonNumber = tokenToNumber semicolon
        let nodeToType = dict["assign", Assignment;]
        
        let typeToDelimiters = dict [
                                        Assignment, [semicolonNumber]; 
                                    ]
        
        let stmntNumber = 0
        
        let parserSource = new ParserSource<RNGLR.ParseExtendedCalc.Token>(tokenToNumber, indToString, leftSides, tokenData)
        let langSource = new LanguageSource(nodeToType, typeToDelimiters)

        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0; 1; 2; 3; 4; (*5; 6; 7; 8; 9; 10; 11; 12;*)] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.ONE 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.Y 4)
                createEdge 5 6 (RNGLR.ParseExtendedCalc.EQ 5)
                createEdge 6 7 (RNGLR.ParseExtendedCalc.TWO 6)
                createEdge 7 8 (RNGLR.ParseExtendedCalc.SEMICOLON 7)
                createEdge 8 9 (RNGLR.ParseExtendedCalc.Z 8)
                createEdge 9 10 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 10 11 (RNGLR.ParseExtendedCalc.THREE 10)
                createEdge 11 12 (RNGLR.ParseExtendedCalc.SEMICOLON 11)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseExtendedCalc.defaultAstToDot mAst "elementary.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            cfg.PrintToDot "elementary cfg.dot"

    [<Test>]
    member test.``Simple If test``() =
        let buildAbstractAst = RNGLR.ParseIf.buildAstAbstract
        let tokenToNumber = RNGLR.ParseIf.tokenToNumber
        let leftSides = RNGLR.ParseIf.leftSide
        let indToString = RNGLR.ParseIf.numToString
        let tokenData = RNGLR.ParseIf.tokenData

        let semicolon = RNGLR.ParseIf.SEMICOLON 0
        let semicolonNumber = tokenToNumber semicolon
        let nodeToType = dict[
                                "simple_statement", Assignment;
                                "if_statement", IfStatement;
                            ]
        let ifNumber = tokenToNumber <| RNGLR.ParseIf.IF 0
        let thenNumber = tokenToNumber <| RNGLR.ParseIf.THEN 0
        let elseNumber = tokenToNumber <| RNGLR.ParseIf.ELSE 0
        let endIfNumber = tokenToNumber <| RNGLR.ParseIf.ENDIF 0

        let typeToDelimiters = dict [
                                        Assignment, [semicolonNumber]; 
                                        IfStatement, [ifNumber; thenNumber; elseNumber; endIfNumber];
                                    ]

        let parserSource = new ParserSource<RNGLR.ParseIf.Token>(tokenToNumber, indToString, leftSides, tokenData)
        let langSource = new LanguageSource(nodeToType, typeToDelimiters, elseNumber, endIfNumber)

        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; (*10; 11; 12;*)] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseIf.IF 0)
                createEdge 1 2 (RNGLR.ParseIf.A 1)
                createEdge 2 3 (RNGLR.ParseIf.THEN 2)
                createEdge 3 4 (RNGLR.ParseIf.B 3)
                createEdge 4 5 (RNGLR.ParseIf.SEMICOLON 4)
                createEdge 5 6 (RNGLR.ParseIf.ELSE 5)
                createEdge 6 7 (RNGLR.ParseIf.E 6)
                createEdge 7 8 (RNGLR.ParseIf.SEMICOLON 7)
                createEdge 8 9 (RNGLR.ParseIf.ENDIF 8)
                createEdge 9 10 (RNGLR.ParseIf.F 9)
                createEdge 10 11 (RNGLR.ParseIf.SEMICOLON 10)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseIf.defaultAstToDot mAst "simple if-tree.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "simple if-cfg.dot"
            printfn "%s" <| cfg.ToString()

    [<Test>]
    member test.``Big If test``() =
        let buildAbstractAst = RNGLR.ParseIf.buildAstAbstract
        let tokenToNumber = RNGLR.ParseIf.tokenToNumber
        let leftSides = RNGLR.ParseIf.leftSide
        let indToString = RNGLR.ParseIf.numToString
        let tokenData = RNGLR.ParseIf.tokenData

        let semicolon = RNGLR.ParseIf.SEMICOLON 0
        let semicolonNumber = tokenToNumber semicolon
        let nodeToType = dict[
                                "simple_statement", Assignment;
                                "if_statement", IfStatement;
                            ]
        let ifNumber = tokenToNumber <| RNGLR.ParseIf.IF 0
        let thenNumber = tokenToNumber <| RNGLR.ParseIf.THEN 0
        let elseNumber = tokenToNumber <| RNGLR.ParseIf.ELSE 0
        let endIfNumber = tokenToNumber <| RNGLR.ParseIf.ENDIF 0

        let typeToDelimiters = dict [
                                        Assignment, [semicolonNumber]; 
                                        IfStatement, [ifNumber; thenNumber; elseNumber; endIfNumber];
                                    ]

        let parserSource = new ParserSource<RNGLR.ParseIf.Token>(tokenToNumber, indToString, leftSides, tokenData)
        let langSource = new LanguageSource(nodeToType, typeToDelimiters, elseNumber, endIfNumber)

        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; (*10; 11; 12;*)] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseIf.IF 0)
                createEdge 1 2 (RNGLR.ParseIf.A 1)
                createEdge 2 3 (RNGLR.ParseIf.THEN 2)
                createEdge 3 4 (RNGLR.ParseIf.B 3)
                createEdge 4 5 (RNGLR.ParseIf.SEMICOLON 4)
                createEdge 5 6 (RNGLR.ParseIf.C 5)
                createEdge 6 7 (RNGLR.ParseIf.SEMICOLON 6)
                createEdge 7 8 (RNGLR.ParseIf.ELSE 7)
                createEdge 8 9 (RNGLR.ParseIf.D 8)
                createEdge 9 10 (RNGLR.ParseIf.SEMICOLON 9)
                createEdge 10 11 (RNGLR.ParseIf.E 10)
                createEdge 11 12 (RNGLR.ParseIf.SEMICOLON 11)
                createEdge 12 13 (RNGLR.ParseIf.ENDIF 12)
                createEdge 13 14 (RNGLR.ParseIf.F 13)
                createEdge 14 15 (RNGLR.ParseIf.SEMICOLON 14)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseIf.defaultAstToDot mAst "big if-tree.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "big if-cfg.dot"
            printfn "%s" <| cfg.ToString()

    [<Test>]
    member test.``If without else test``() =
        let buildAbstractAst = RNGLR.ParseIf.buildAstAbstract
        let tokenToNumber = RNGLR.ParseIf.tokenToNumber
        let leftSides = RNGLR.ParseIf.leftSide
        let indToString = RNGLR.ParseIf.numToString
        let tokenData = RNGLR.ParseIf.tokenData

        let semicolon = RNGLR.ParseIf.SEMICOLON 0
        let semicolonNumber =  tokenToNumber semicolon
        let nodeToType = dict[
                                "simple_statement", Assignment;
                                "if_statement", IfStatement;
                            ]
        let ifNumber = tokenToNumber <| RNGLR.ParseIf.IF 0
        let thenNumber = tokenToNumber <| RNGLR.ParseIf.THEN 0
        let elseNumber = tokenToNumber <| RNGLR.ParseIf.ELSE 0
        let endIfNumber = tokenToNumber <| RNGLR.ParseIf.ENDIF 0

        let typeToDelimiters = dict [
                                        Assignment, [semicolonNumber]; 
                                        IfStatement, [ifNumber; thenNumber; elseNumber; endIfNumber];
                                    ]

        let parserSource = new ParserSource<RNGLR.ParseIf.Token>(tokenToNumber, indToString, leftSides, tokenData)
        let langSource = new LanguageSource(nodeToType, typeToDelimiters, elseNumber, endIfNumber)

        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10;] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseIf.IF 0)
                createEdge 1 2 (RNGLR.ParseIf.A 1)
                createEdge 2 3 (RNGLR.ParseIf.THEN 2)
                createEdge 3 4 (RNGLR.ParseIf.B 3)
                createEdge 4 5 (RNGLR.ParseIf.SEMICOLON 4)
                createEdge 5 6 (RNGLR.ParseIf.C 5)
                createEdge 6 7 (RNGLR.ParseIf.SEMICOLON 6)
                createEdge 7 8 (RNGLR.ParseIf.ENDIF 7)
                createEdge 8 9 (RNGLR.ParseIf.F 8)
                createEdge 9 10 (RNGLR.ParseIf.SEMICOLON 9)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseIf.defaultAstToDot mAst "if-without-else-tree.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "if_statement.dot"
            printfn "%s" <| cfg.ToString()
            
    [<Test>]
    member test.``Inner if``() =
        let buildAbstractAst = RNGLR.ParseIf.buildAstAbstract
        let tokenToNumber = RNGLR.ParseIf.tokenToNumber
        let leftSides = RNGLR.ParseIf.leftSide
        let indToString = RNGLR.ParseIf.numToString
        let tokenData = RNGLR.ParseIf.tokenData

        let semicolon = RNGLR.ParseIf.SEMICOLON 0
        let semicolonNumber = tokenToNumber semicolon
        let nodeToType = dict[
                                "if_statement", IfStatement;
                                "simple_statement", Assignment;
                            ]
        let ifNumber = tokenToNumber <| RNGLR.ParseIf.IF 0
        let thenNumber = tokenToNumber <| RNGLR.ParseIf.THEN 0
        let elseNumber = tokenToNumber <| RNGLR.ParseIf.ELSE 0
        let endIfNumber = tokenToNumber <| RNGLR.ParseIf.ENDIF 0
        
        let typeToDelimiters = dict [
                                        Assignment, [semicolonNumber]; 
                                        IfStatement, [ifNumber; thenNumber; elseNumber; endIfNumber];
                                    ]

        let parserSource = new ParserSource<RNGLR.ParseIf.Token>(tokenToNumber, indToString, leftSides, tokenData)
        let langSource = new LanguageSource(nodeToType, typeToDelimiters, elseNumber, endIfNumber)

        let qGraph = new ParserInputGraph<_>()
        let vertices = Array.init 23 (fun i -> i)
        qGraph.AddVertexRange vertices |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1  (RNGLR.ParseIf.IF 0)
                createEdge 1 2  (RNGLR.ParseIf.A 1)
                createEdge 2 3  (RNGLR.ParseIf.THEN 2)
                createEdge 3 4  (RNGLR.ParseIf.IF 3)
                createEdge 4 5  (RNGLR.ParseIf.B 4)
                createEdge 5 6  (RNGLR.ParseIf.THEN 5)
                createEdge 6 7  (RNGLR.ParseIf.C 6)
                createEdge 7 8  (RNGLR.ParseIf.SEMICOLON 7)
                createEdge 8 9  (RNGLR.ParseIf.ELSE 8)
                createEdge 9 10 (RNGLR.ParseIf.D 9)
                createEdge 10 11(RNGLR.ParseIf.SEMICOLON 10)
                createEdge 11 12(RNGLR.ParseIf.ENDIF 11)
                createEdge 12 13(RNGLR.ParseIf.ELSE 12)
                createEdge 13 14(RNGLR.ParseIf.IF 13)
                createEdge 14 15(RNGLR.ParseIf.E 14)
                createEdge 15 16(RNGLR.ParseIf.THEN 15)
                createEdge 16 17(RNGLR.ParseIf.F 16)
                createEdge 17 18(RNGLR.ParseIf.SEMICOLON 17)
                createEdge 18 19(RNGLR.ParseIf.ELSE 18)
                createEdge 19 20(RNGLR.ParseIf.G 19)
                createEdge 20 21(RNGLR.ParseIf.SEMICOLON 20)
                createEdge 21 22(RNGLR.ParseIf.ENDIF 21)
                createEdge 22 23(RNGLR.ParseIf.ENDIF 22)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseIf.defaultAstToDot mAst "Inner if.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "Inner if cfg.dot"
            printfn "%s" <| cfg.ToString()

[<EntryPoint>]
let f x = 
    let cfgBuilding = new ``Control Flow Graph Building``()
//    cfgBuilding.``Elementary test``()
//    cfgBuilding.``Simple If test``()
    cfgBuilding.``Big If test``()
//    cfgBuilding.``If without else test``()
//    cfgBuilding.``Inner if``()
    1