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


    [<Test>]
    member test.``Elementary test``() =
        let qGraph = new ParserInputGraph<_>(0, 12)
        let vertexRange = List.init 13 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore
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
            RNGLR.ParseExtendedCalc.defaultAstToDot mAst "tree elementary.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "cfg elementary.dot"
            printfn "%s" <| cfg.ToString()

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = new ParserInputGraph<_>(0, 15)
        let vertexRange = List.init 16 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore

        //          -> Y = 2;
        // X = 1;                -> X = 4;
        //          -> Z = 3;
        
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
                createEdge 4 9 (RNGLR.ParseExtendedCalc.Z 8)
                createEdge 9 10 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 10 11 (RNGLR.ParseExtendedCalc.THREE 10)
                createEdge 11 8 (RNGLR.ParseExtendedCalc.SEMICOLON 11)
                createEdge 8 12 (RNGLR.ParseExtendedCalc.X 8)
                createEdge 12 13 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 13 14 (RNGLR.ParseExtendedCalc.FOUR 10)
                createEdge 14 15 (RNGLR.ParseExtendedCalc.SEMICOLON 11)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph

        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseExtendedCalc.defaultAstToDot mAst "tree Ambiguous.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)

            printfn "%s" <| cfg.ToString()
            cfg.PrintToDot "cfg Ambiguous.dot"


[<TestFixture>]
type ``Control Flow Graph: If`` () =
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

    [<Test>]
    member test.``Simple If test``() =
        let qGraph = new ParserInputGraph<_>(0, 11)
        let vertexRange = List.init 12 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore
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
            RNGLR.ParseIf.defaultAstToDot mAst "tree simple if.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "cfg simple if.dot"
            printfn "%s" <| cfg.ToString()

    [<Test>]
    member test.``Big If test``() =
        let qGraph = new ParserInputGraph<_>(0, 15)
        let vertexRange = List.init 16 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore
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
            RNGLR.ParseIf.defaultAstToDot mAst "tree big if.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "cfg big if.dot"
            printfn "%s" <| cfg.ToString()

    [<Test>]
    member test.``If without else test``() =
        let qGraph = new ParserInputGraph<_>(0, 10)

        let vertexRange = List.init 11 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore

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
            RNGLR.ParseIf.defaultAstToDot mAst "tree if-without-else.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "cfg if-without-else.dot"
            printfn "%s" <| cfg.ToString()
            
    [<Test>]
    member test.``Inner if``() =
        let qGraph = new ParserInputGraph<_>(0, 23)
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
            RNGLR.ParseIf.defaultAstToDot mAst "Tree inner if.dot"
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens)
            
            cfg.PrintToDot "cfg inner if.dot"
            printfn "%s" <| cfg.ToString()

[<EntryPoint>]
let f x = 
    let cfgBuilding = new ``Control Flow Graph Building``()
//    cfgBuilding.``Ambiguous test``()
    let ifBuilding = new ``Control Flow Graph: If``()
    ifBuilding.``If without else test``()
    1