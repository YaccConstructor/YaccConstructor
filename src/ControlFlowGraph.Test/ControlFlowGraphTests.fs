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
        
    let tokToRealString tok = tok |> tokenToNumber |> indToString
    let parserSource = new ParserSource<RNGLR.ParseExtendedCalc.Token>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, typeToDelimiters)

    let runTest graph (printOpt : _ option) = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            if printOpt.IsSome 
            then
                let astName = fst printOpt.Value
                RNGLR.ParseExtendedCalc.defaultAstToDot mAst astName

            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens, tokToRealString)
            
            if printOpt.IsSome 
            then
                let cfgName = snd printOpt.Value
                cfg.PrintToDot cfgName
            
            printfn "%s" <| cfg.ToString()

    [<Test>]
    member test.``Elementary test``() =
        let qGraph = new ParserInputGraph<_>(0, 13)
        let vertexRange = List.init 14 (fun i -> i)
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
                createEdge 12 13 (RNGLR.ParseExtendedCalc.RNGLR_EOF 12)
            ] |> ignore

        let printOpt = Some <| ("ast elementary (cfg construction).dot", "cfg elementary (cfg construction).dot")
        runTest qGraph None

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = new ParserInputGraph<_>(0, 16)
        let vertexRange = List.init 17 (fun i -> i)
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
                createEdge 15 16 (RNGLR.ParseExtendedCalc.RNGLR_EOF 12)
            ] |> ignore

        let printOpt = None //Some <| ("ast ambiguous (cfg construction).dot", "cfg ambiguous (cfg construction).dot")
        runTest qGraph printOpt

    [<Test>]
    member this.``Ambiguous2 test``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        let vertexRange = List.init 6 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 0 1 (RNGLR.ParseExtendedCalc.Y 1)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 2)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.FIVE 3)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 4)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.RNGLR_EOF 5)
            ] |> ignore

        let printOpt = None // "ast ambiguous2 (cfg construction).dot" "cfg ambiguous2 (cfg construction).dot"
        runTest qGraph printOpt


//[<TestFixture>]
type ``Control Flow Graph: If`` () =
    let buildAbstractAst = RNGLR.ParseIf.buildAstAbstract
    let tokenToNumber = RNGLR.ParseIf.tokenToNumber
    let leftSides = RNGLR.ParseIf.leftSide
    let indToString = RNGLR.ParseIf.numToString
    let tokenData = RNGLR.ParseIf.tokenData

    let semicolonNumber = tokenToNumber <| RNGLR.ParseIf.SEMICOLON 0
    let ifNumber = tokenToNumber <| RNGLR.ParseIf.IF 0
    let thenNumber = tokenToNumber <| RNGLR.ParseIf.THEN 0
    let elseNumber = tokenToNumber <| RNGLR.ParseIf.ELSE 0
    let endIfNumber = tokenToNumber <| RNGLR.ParseIf.ENDIF 0
    
    let nodeToType = dict[
                                "simple_statement", Assignment;
                                "if_statement", IfStatement;
                          ]
    

    let tokToRealString tok = tok |> tokenToNumber |> indToString

    let typeToDelimiters = dict [
                                    Assignment, [semicolonNumber]; 
                                    IfStatement, [ifNumber; thenNumber; elseNumber; endIfNumber];
                                ]

    let parserSource = new ParserSource<RNGLR.ParseIf.Token>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, typeToDelimiters, elseNumber, endIfNumber)

    let runTest graph (printOpt : _ option) = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            
            if printOpt.IsSome
            then
                let astName = fst printOpt.Value
                RNGLR.ParseIf.defaultAstToDot mAst astName
            
            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens, tokToRealString)
            
            if printOpt.IsSome
            then
                let cfgName = snd printOpt.Value
                cfg.PrintToDot cfgName
            
            printfn "%s" <| cfg.ToString()

  //  [<Test>]
    member test.``Simple If test``() =
        let qGraph = new ParserInputGraph<_>(0, 12)
        let vertexRange = List.init 13 (fun i -> i)
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
                createEdge 11 12 (RNGLR.ParseIf.RNGLR_EOF 11)
            ] |> ignore

        let printOpt = None //"ast simple if (cfg construction).dot" "cfg simple if (cfg construction).dot"
        runTest qGraph printOpt

    //[<Test>]
    member test.``Big If test``() =
        let qGraph = new ParserInputGraph<_>(0, 15)
        let vertexRange = List.init 17 (fun i -> i)
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
                createEdge 15 16 (RNGLR.ParseIf.RNGLR_EOF 15)
            ] |> ignore

        let printOpt = None //"ast big if (cfg construction).dot" "cfg big if (cfg construction).dot"
        runTest qGraph printOpt

    //[<Test>]
    member test.``If without else test``() =
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
                createEdge 5 6 (RNGLR.ParseIf.C 5)
                createEdge 6 7 (RNGLR.ParseIf.SEMICOLON 6)
                createEdge 7 8 (RNGLR.ParseIf.ENDIF 7)
                createEdge 8 9 (RNGLR.ParseIf.F 8)
                createEdge 9 10 (RNGLR.ParseIf.SEMICOLON 9)
                createEdge 10 11 (RNGLR.ParseIf.RNGLR_EOF 10)
            ] |> ignore

        let printOpt = None //"ast if-without-else (cfg construction).dot" "cfg if-without-else (cfg construction).dot"
        runTest qGraph printOpt
            
    //[<Test>]
    member test.``Inner if``() =
        let qGraph = new ParserInputGraph<_>(0, 24)
        let vertices = Array.init 25 (fun i -> i)
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
                createEdge 23 24(RNGLR.ParseIf.RNGLR_EOF 23)
            ] |> ignore

        let printOpt = None //"ast inner if (cfg construction).dot" "cfg inner if (cfg construction).dot"
        runTest qGraph printOpt


[<TestFixture>]
type ``Find undefined variables`` () =
    
    let buildAbstractAst = RNGLR.ParseExtendedCalc.buildAstAbstract
    let tokenToNumber = RNGLR.ParseExtendedCalc.tokenToNumber
    let leftSides = RNGLR.ParseExtendedCalc.leftSide
    let indToString = RNGLR.ParseExtendedCalc.numToString
    let tokenData = RNGLR.ParseExtendedCalc.tokenData

    let semicolonNumber = tokenToNumber <| RNGLR.ParseExtendedCalc.SEMICOLON 0
    let eqNumber = tokenToNumber <| RNGLR.ParseExtendedCalc.EQ 0

    let nodeToType = dict["assign", Assignment;]
        
    let typeToDelimiters = dict [Assignment, [semicolonNumber]; ]
        
    let varsNumbers = 
        [RNGLR.ParseExtendedCalc.X 0; RNGLR.ParseExtendedCalc.Y 0; RNGLR.ParseExtendedCalc.Z 0]
        |> List.map (fun t -> tokenToNumber t)

    let isVariable tok = varsNumbers |> List.exists (fun t -> t = tok) 

    let tokToRealName tok = tok |> tokenToNumber |> indToString   
        
    let parserSource = new ParserSource<RNGLR.ParseExtendedCalc.Token>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, typeToDelimiters, -1, -1, eqNumber, isVariable)

    let runTest qGraph expected (printOpt : _ option) = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            if printOpt.IsSome
            then
                let astName = fst printOpt.Value
                RNGLR.ParseExtendedCalc.defaultAstToDot mAst astName

            let cfg = ControlFlow (mAst, parserSource, langSource, mAst.Tokens, tokToRealName)
            
            if printOpt.IsSome
            then
                let cfgName = snd printOpt.Value
                cfg.PrintToDot cfgName
            
            let errorList = cfg.FindUndefVariable()
            
            printfn "%A" errorList
            printfn "Expected: %d. Actual: %d." expected errorList.Length 
            
            Assert.AreEqual(expected, errorList.Length)

    [<Test>]
    member test.``Elementary``() = 
        let qGraph = new ParserInputGraph<_>(0, 9)
        let vertexRange = List.init 10 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.Z 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.Y 4)
                createEdge 5 6 (RNGLR.ParseExtendedCalc.EQ 5)
                createEdge 6 7 (RNGLR.ParseExtendedCalc.X 6)
                createEdge 7 8 (RNGLR.ParseExtendedCalc.SEMICOLON 7)
                createEdge 8 9 (RNGLR.ParseExtendedCalc.RNGLR_EOF 8)
            ] |> ignore

        let expected = 1
        let printOpt = None //"ast elementary (undefined variables).dot" "cfg elementary (undefined variables).dot"
        runTest qGraph expected printOpt

    [<Test>]
    member test.``X = X``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        let vertexRange = List.init 6 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.X 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.RNGLR_EOF 4)
            ] |> ignore

        let expected = 1
        let printOpt = None //"ast X = X (undefined variables).dot" "cfg X = X (undefined variables).dot"
        runTest qGraph expected printOpt

    [<Test>]
    member test.``Undef: ambiguous``() =
        let qGraph = new ParserInputGraph<_>(0, 18)
        let vertexRange = List.init 19 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore

        //          -> Y = 2;
        // X = 1;                -> X = Y * Z;
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
                createEdge 12 13 (RNGLR.ParseExtendedCalc.EQ 12)
                createEdge 13 14 (RNGLR.ParseExtendedCalc.Y 13)
                createEdge 14 15 (RNGLR.ParseExtendedCalc.MULT 14)
                createEdge 15 16 (RNGLR.ParseExtendedCalc.Z 15)
                createEdge 16 17 (RNGLR.ParseExtendedCalc.SEMICOLON 16)
                createEdge 17 18 (RNGLR.ParseExtendedCalc.RNGLR_EOF 17)
            ] |> ignore

        let expected = 2
        let printOpt = None //"ast ambiguous1 (undefined variables).dot" "cfg ambiguous1 (undefined variables).dot"
        runTest qGraph expected printOpt
            
    [<Test>]
    member test.``Undef: ambiguous 2``() =
        let qGraph = new ParserInputGraph<_>(0, 15)
        let vertexRange = List.init 16 (fun i -> i)
        qGraph.AddVertexRange vertexRange |> ignore

        //        ---> Y = 2; ---> 
        // X = 1; ---------------> X = Y * Z;   
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
                createEdge 4 9 (RNGLR.ParseExtendedCalc.X 8)
                createEdge 8 9 (RNGLR.ParseExtendedCalc.X 8)
                createEdge 9 10 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 10 11 (RNGLR.ParseExtendedCalc.Y 10)
                createEdge 11 12 (RNGLR.ParseExtendedCalc.MULT 11)
                createEdge 12 13 (RNGLR.ParseExtendedCalc.Z 12)
                createEdge 13 14 (RNGLR.ParseExtendedCalc.SEMICOLON 13)
                createEdge 14 15 (RNGLR.ParseExtendedCalc.RNGLR_EOF 14)
            ] |> ignore

        let expected = 2
        let printOpt = None //"ast ambiguous2 (undefined variables).dot" "cfg ambiguous2 (undefined variables).dot"
        runTest qGraph expected printOpt

//[<EntryPoint>]
let f x = 
    let cfgBuilding = new ``Control Flow Graph Building``()
//    cfgBuilding.``Elementary test``()
    cfgBuilding.``Ambiguous2 test``()
//    let ifBuilding = new ``Control Flow Graph: If``()
//    ifBuilding.``If without else test``()
//    let undefVariables = new ``Find undefined variables``()
//    undefVariables.``Undef: ambiguous 2``()
    1