//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.


module RNGLRAbstractParserTests

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractAnalysis.Common
open RNGLR.SimpleCalc
open RNGLR.PrettySimpleCalc
open Yard.Generators.RNGLR.AbstractParser
open YC.Tests.Helper
open RNGLR.Tesssst
open Yard.Generators.ARNGLR.Parser

open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation

let baseInputGraphsPath = "../../../Tests/AbstractRNGLR/DOT"

let path name = path baseInputGraphsPath name

let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)

let loadLexerInputGraph gFile =
    let qGraph = loadDotToQG baseInputGraphsPath gFile
    let lexerInputG = new LexerInputGraph<_>()
    lexerInputG.StartVertex <- 0
    for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new LexerEdge<_,_>(e.Source,e.Target,Some (e.Tag, e.Tag)))
    lexerInputG

let test buildAstAbstract qGraph nodesCount edgesCount epsilonsCount termsCount ambiguityCount = 
    let start = System.DateTime.Now
    let r = (new Parser<_>()).Parse  buildAstAbstract qGraph
    printfn "Result = %A \n Time = %A" r (System.DateTime.Now - start)
    match r with
    | Error (num, tok, message) ->
        let msg = sprintf "Error in position %d on Token %A: %s" num tok message
        printfn "%A" msg
        Assert.Fail msg
    | Success(tree) ->
        //tree.PrintAst()
        let n, e, eps, t, amb = tree.CountCounters()
        Assert.AreEqual(nodesCount, n, "Nodes count mismatch")
//        Assert.AreEqual(edgesCount, e, "Edges count mismatch")
//        Assert.AreEqual(epsilonsCount, eps, "Epsilons count mismatch")
//        Assert.AreEqual(termsCount, t, "Terms count mismatch")
//        Assert.AreEqual(ambiguityCount, amb, "Ambiguities count mismatch")
       // Assert.Pass()

let perfTest parse inputLength graph =    
    for x in 0..inputLength do
        let qGraph = graph x
        let start = System.DateTime.Now
        for y in 0..9 do
            match parse qGraph with
            | Success _ -> ()
            | Error (i, t, msg) -> failwithf "Performance test failed wit message:%A" msg

        let time = (System.DateTime.Now - start).TotalMilliseconds / 10.0
        System.GC.Collect()
        printfn "%0i : %A" x time

let perfTest2 parse graph =    
    for i = 10 to 200 do
        let g = graph (1 + i) 2 
        let start = System.DateTime.Now
        let r = parse g
        let finish = System.DateTime.Now - start
        printfn "%i  : %A" (i+1) finish.TotalSeconds
        System.GC.Collect()
        match r with
        | Error _ ->
            printfn "Error"     
        | Success tree->
            ()//printfn "%s" "sss"
    

//let errorTest inputFilePath shouldContainsSuccess errorsCount =
//    printfn "==============================================================="
//    let lexerInputGraph = loadLexerInputGraph inputFilePath
//    let qGraph = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, RNGLR.ParseCalc.RNGLR_EOF 0)
//    let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
//    printfn "%A" r
//    match r with
//    | Error (_,tok, message, debug, _) ->
//        printfn "Errors in file %s on Tokens %A: %s" inputFilePath tok message
//        debug.drawGSSDot "out.dot"
//        if shouldContainsSuccess
//        then Assert.Fail(sprintf "Test %s should produce sucess parsing result but its fully failed." inputFilePath)
//        else Assert.AreEqual(errorsCount, tok.Length, (sprintf "Errors count mismatch in test %s." inputFilePath))
//    | Success(tree, tok, _) ->
//        tree.PrintAst()
//        RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
//        if shouldContainsSuccess
//        then Assert.AreEqual(errorsCount, tok.Length, (sprintf "Errors count mismatch in test %s." inputFilePath))
//        else Assert.Fail(sprintf "Test %s should not produce sucess parsing result but it is produce." inputFilePath)

[<TestFixture>]
type ``RNGLR abstract parser tests`` () =

    [<Test>]
    member this.``Load graph test from DOT`` () =
        let g = loadGraphFromDOT(path "IFExists_lex.dot")
        Assert.AreEqual(g.Edges |> Seq.length, 29)
        Assert.AreEqual(g.Vertices |> Seq.length, 25)

    [<Test>]
    member this.``Load graph test from DOT to QuickGraph`` () =
        let g = loadGraphFromDOT(path "IFExists_lex.dot")
        let qGraph = new AdjacencyGraph<int, TaggedEdge<_,string>>()
        g.Edges 
        |> Seq.iter(
            fun e -> 
                let edg = e :?> DotEdge<string>
                qGraph.AddVertex(int edg.Source.Id) |> ignore
                qGraph.AddVertex(int edg.Destination.Id) |> ignore
                qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id,edg.Label)) |> ignore)
        Assert.AreEqual(qGraph.Edges |> Seq.length, 29)
        Assert.AreEqual(qGraph.Vertices |> Seq.length, 25)

    [<Test>]
    member this._01_PrettySimpleCalc_SequenceInput () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 13 12 0 3 0

    [<Test>]
    member this._01_PrettySimpleCalc_SequenceInput_1 () =
        let qGraph = new ParserInputGraph<_>([|0|], [|4|])
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 13 12 0 3 0

    [<Test>]
    member this._01_PrettySimpleCalc_SequenceInput_MultipleFinalVertices () =
        let qGraph = new ParserInputGraph<_>([|0|], [|1; 2; 3; 4|])
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 13 12 0 3 0

    [<Test>]
    member this._01_PrettySimpleCalc_SequenceInput_MultipleStartAndFinalVertices () =
        let qGraph = new ParserInputGraph<_>([|0; 2|], [|4; 5|])
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 1 5 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 22 22 0 4 1

    [<Test>]
    member this._02_PrettySimpleCalcSimple_BranchedInput () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 0 3 (RNGLR.PrettySimpleCalc.NUM 4)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 15 14 0 4 1

    [<Test>]
    member this._03_PrettySimpleCalc_BranchedInput () =
        let qGraph = new ParserInputGraph<_>(2, 9)
        qGraph.AddVerticesAndEdgeRange
            [
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 3 4 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 4 5 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 6 (RNGLR.PrettySimpleCalc.PLUS 4)
             edg 6 5 (RNGLR.PrettySimpleCalc.NUM 5)
             edg 5 7 (RNGLR.PrettySimpleCalc.PLUS 6)
             edg 7 8 (RNGLR.PrettySimpleCalc.NUM 7)
             edg 8 9 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 34 40 0 11 2

    [<Test>]
    member this._04_PrettySimpleCalc_LotsOfVariants () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 4 (RNGLR.PrettySimpleCalc.PLUS 4)
             edg 4 5 (RNGLR.PrettySimpleCalc.NUM 5)
             edg 3 6 (RNGLR.PrettySimpleCalc.PLUS 6)
             edg 6 5 (RNGLR.PrettySimpleCalc.NUM 7)
             edg 5 7 (RNGLR.PrettySimpleCalc.PLUS 8)
             edg 7 8 (RNGLR.PrettySimpleCalc.NUM 9)
             edg 8 9 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 56 74 0 20 4

    [<Test>]
    member this._05_NotAmbigousSimpleCalc_LotsOfVariants () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalc.NUM 5)
             edg 3 6 (RNGLR.NotAmbigousSimpleCalc.PLUS 6)
             edg 6 5 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 5 7 (RNGLR.NotAmbigousSimpleCalc.PLUS 8)
             edg 7 8 (RNGLR.NotAmbigousSimpleCalc.NUM 9)
             edg 8 9 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 22 22 0 9 1

    [<Test>]
    member this._06_NotAmbigousSimpleCalc_Loop () =
        let qGraph = new ParserInputGraph<_>(0 , 7)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalc.NUM 5)
             edg 5 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 6)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 6 7 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 22 22 0 9 1

    [<Test>]
    member this._07_NotAmbigousSimpleCalc_Loop2 () =
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (RNGLR.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 1 (RNGLR.NotAmbigousSimpleCalc.NUM 6)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 6 7 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 18 18 0 7 1

    [<Test>]
    member this._08_NotAmbigousSimpleCalc_Loop3 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (RNGLR.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 7 (RNGLR.NotAmbigousSimpleCalc.NUM 6)
             edg 7 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 8)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 6 8 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 22 22 0 9 1
        
    [<Test>]
    member this._09_NotAmbigousSimpleCalc_Loop4 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (RNGLR.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 3 (RNGLR.NotAmbigousSimpleCalc.NUM 6)             
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 6 8 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 18 18 0 7 1

    [<Test>]
    member this._10_NotAmbigousSimpleCalc_Loop5 () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (RNGLR.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 3 (RNGLR.NotAmbigousSimpleCalc.NUM 6)
             edg 3 8 (RNGLR.NotAmbigousSimpleCalc.PLUS 7)
             edg 8 3 (RNGLR.NotAmbigousSimpleCalc.NUM 8)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 9)
             edg 6 9 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 21 22 0 9 1

    [<Test>]
    member this._11_NotAmbigousSimpleCalc_Loop6 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalc.NUM 5)
             edg 5 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 6)
             edg 5 7 (RNGLR.NotAmbigousSimpleCalc.PLUS 6)
             edg 7 1 (RNGLR.NotAmbigousSimpleCalc.NUM 8)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 6 8 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 25 26 0 11 2

    [<Test>]
    member this._12_NotAmbigousSimpleCalc_Loop7 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalc.NUM 5)
             edg 7 5 (RNGLR.NotAmbigousSimpleCalc.NUM 6)
             edg 5 7 (RNGLR.NotAmbigousSimpleCalc.PLUS 7)
             edg 7 1 (RNGLR.NotAmbigousSimpleCalc.NUM 8)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 9)
             edg 6 8 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 25 26 0 11 2

    [<Test>]
    member this._13_NotAmbigousSimpleCalc_Loop8 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalc.NUM 5)
             edg 5 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 6)
             edg 7 5 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 5 7 (RNGLR.NotAmbigousSimpleCalc.PLUS 8)
             edg 7 1 (RNGLR.NotAmbigousSimpleCalc.NUM 9)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 10)
             edg 6 8 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph 28 30 0 13 3

    [<Test>]
    member this._14_NotAmbigousSimpleCalcWith2Ops_Loop () =
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 4)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 5)
             edg 5 2 (RNGLR.NotAmbigousSimpleCalcWith2Ops.MULT 6)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 7)
             edg 6 7 (RNGLR.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.NotAmbigousSimpleCalcWith2Ops.buildAstAbstract qGraph 22 22 0 9 1

    [<Test>]
    member this._15_NotAmbigousSimpleCalcWith2Ops_Loops () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 3)
             edg 2 4 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 4)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 5)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 6)
             edg 5 2 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 7)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 8)
             edg 6 7 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 9)
             edg 7 8 (RNGLR.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.NotAmbigousSimpleCalcWith2Ops.buildAstAbstract qGraph 22 22 0 9 1

    [<Test>]
    member this._16_Stars_Loop () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars.STAR 1)
             edg 0 1 (RNGLR.Stars.SEMI 2)
             edg 1 2 (RNGLR.Stars.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars.buildAstAbstract qGraph 15 14 0 6 1

    [<Test>]
    member this._17_Stars2_Loop () =
        let qGraph = new ParserInputGraph<_>(0, 1)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars2.STAR 1)
             edg 0 1 (RNGLR.Stars2.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars2.buildAstAbstract qGraph 34 36 0 10 4

    [<Test>]
    member this._18_Stars2_Loop2 () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars2.STAR 1)
             edg 0 1 (RNGLR.Stars2.STAR 2)
             edg 1 2 (RNGLR.Stars2.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars2.buildAstAbstract qGraph 42 42 0 14 4

    [<Test>]
    member this._19_FirstEps () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.FirstEps.Z 1)
            edg 1 3 (RNGLR.FirstEps.N 2)
            edg 3 4 (RNGLR.FirstEps.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.FirstEps.buildAstAbstract qGraph 14 13 2 2 0
    
    [<Test>]
    member this._20_CroppedBrackets () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (RNGLR.CroppedBrackets.LBR 1)
            edg 0 1 (RNGLR.CroppedBrackets.NUM 2)
            edg 1 1 (RNGLR.CroppedBrackets.RBR 3)
            edg 1 2 (RNGLR.CroppedBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.CroppedBrackets.buildAstAbstract qGraph 14 14 0 6 2

    [<Test>]
    member this._21_Brackets () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (RNGLR.Brackets.LBR 1)
            edg 0 1 (RNGLR.Brackets.NUM 2)
            edg 1 1 (RNGLR.Brackets.RBR 3)
            edg 1 2 (RNGLR.Brackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.Brackets.buildAstAbstract qGraph 14 14 0 6 2

    [<Test>]
    member this._22_Brackets_BackEdge () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (RNGLR.Brackets.LBR 1)
            edg 0 1 (RNGLR.Brackets.NUM 2)
            edg 1 1 (RNGLR.Brackets.RBR 3)
            edg 1 0 (RNGLR.Brackets.NUM 4)
            edg 1 2 (RNGLR.Brackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.Brackets.buildAstAbstract qGraph 81 134 0 18 16

    [<Test>]
    member this._23_UnambiguousBrackets () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 1)
            edg 1 1 (RNGLR.StrangeBrackets.LBR 2)
            edg 1 2 (RNGLR.StrangeBrackets.RBR 3)
            edg 2 2 (RNGLR.StrangeBrackets.RBR 4)
            edg 2 3 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 20 20 2 8 2

    [<Test>]
    member this._24_UnambiguousBrackets_Circle () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 0)
            edg 1 0 (RNGLR.StrangeBrackets.RBR 1)
            edg 0 9 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 24 24 4 8 2

    [<Test>]
    member this._24_UnambiguousBrackets_Circle_MultipleStartVertices () =
        let qGraph = new ParserInputGraph<_>([|0; 1|], [|9; 10|])
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 0)
            edg 1 0 (RNGLR.StrangeBrackets.RBR 1)
            edg 0 9 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            edg 1 10 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 24 24 4 8 2 // ???

    [<Test>]
    member this._24_UnambiguousBrackets_Circle_MultipleStartVertices_1 () =
        let qGraph = new ParserInputGraph<_>([|0; 1|], [|9|])
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 0)
            edg 1 0 (RNGLR.StrangeBrackets.RBR 1)
            edg 0 9 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 24 24 4 8 2


    [<Test>]
    member this._25_UnambiguousBrackets_BiggerCircle () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 0)
            edg 1 2 (RNGLR.StrangeBrackets.RBR 1)
            edg 2 3 (RNGLR.StrangeBrackets.LBR 2)
            edg 3 0 (RNGLR.StrangeBrackets.RBR 3)
            edg 0 9 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 25 25 4 8 1

    [<Test>]
    member this._26_UnambiguousBrackets_Inf () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (RNGLR.StrangeBrackets.LBR 0)
            edg 0 0 (RNGLR.StrangeBrackets.RBR 1)
            edg 0 9 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 53 62 6 24 3

    //[<Test>]
    member this.EpsilonKiller () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.EpsilonKiller.B 0)
            edg 1 2 (RNGLR.EpsilonKiller.A 1)
            edg 2 3 (RNGLR.EpsilonKiller.A 2)
            edg 3 4 (RNGLR.EpsilonKiller.A 3)
            edg 4 9 (RNGLR.EpsilonKiller.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.EpsilonKiller.buildAstAbstract qGraph 24 24 4 8 2

    [<Test>]
    member this._27_UnambiguousBrackets_WithoutEmptyString () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 0)
            edg 1 0 (RNGLR.StrangeBrackets.RBR 1)
            edg 1 2 (RNGLR.StrangeBrackets.RBR 2)
            edg 2 9 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

    [<Test>]
    member this._28_UnambiguousBrackets_DifferentPathLengths () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 0)
            edg 1 2 (RNGLR.StrangeBrackets.RBR 1)
            edg 2 3 (RNGLR.StrangeBrackets.LBR 2)
            edg 3 4 (RNGLR.StrangeBrackets.RBR 3)
            edg 2 5 (RNGLR.StrangeBrackets.LBR 4)
            edg 5 6 (RNGLR.StrangeBrackets.RBR 5)
            edg 6 3 (RNGLR.StrangeBrackets.LBR 6)
            edg 4 9 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 25 24 4 8 1

    [<Test>]
    member this._29_AandB_Linear () =
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.AandB.A 0)
            edg 1 2 (RNGLR.AandB.B 1)
            edg 2 3 (RNGLR.AandB.A 2)
            edg 3 4 (RNGLR.AandB.A 3)
            edg 4 5 (RNGLR.AandB.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.AandB.buildAstAbstract qGraph 19 18 0 8 1

    [<Test>]
    member this._29_AandB_Circle () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.AandB.A 0)
            edg 1 2 (RNGLR.AandB.B 1)
            edg 2 3 (RNGLR.AandB.A 2)
            edg 3 2 (RNGLR.AandB.A 3)
            edg 3 4 (RNGLR.AandB.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.AandB.buildAstAbstract qGraph 36 37 0 16 3

    [<Test>]
    member this._29_AandB_Branch () =
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.AandB.A 0)
            edg 1 2 (RNGLR.AandB.B 1)
            edg 2 3 (RNGLR.AandB.A 2)
            edg 3 4 (RNGLR.AandB.A 3)
            edg 2 4 (RNGLR.AandB.A 4)
            edg 4 5 (RNGLR.AandB.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.AandB.buildAstAbstract qGraph 23 22 0 11 1

    member this.``Not Ambigous Simple Calc. Branch. Perf`` i inpLength isLoop =  
        let tpl x =
            [
             yield!
                 [edg x (x + 1) (RNGLR.NotAmbigousSimpleCalc.NUM  x)
                  edg (x + 1) (x + 2) (RNGLR.NotAmbigousSimpleCalc.PLUS (x + 1))
                  edg (x + 2) (x + 3) (RNGLR.NotAmbigousSimpleCalc.NUM (x + 2))
                  edg (x + 3) (x + 4) (RNGLR.NotAmbigousSimpleCalc.PLUS (x + 3))]            
             yield![for y in 0..i do
                        yield edg (x + (if isLoop then 4 else 2)) (x + 5 + y) (RNGLR.NotAmbigousSimpleCalc.NUM 5)
                        yield edg (x + 5 + y) (x + (if isLoop then 2 else 4)) (RNGLR.NotAmbigousSimpleCalc.PLUS 6)]
            
             yield edg (x + 4) (x + 6 + i) (RNGLR.NotAmbigousSimpleCalc.NUM (x + 4))
             yield edg (x + 6 + i) (x + 7 + i) (RNGLR.NotAmbigousSimpleCalc.PLUS (x + 4))
            ]

        let graph x =
            let eog = (x + 1) * (7 + i) 
            let qGraph = new ParserInputGraph<_>(0 , eog + 2)
            for j in 0..x do
                tpl (j * (7 + i)) |> qGraph.AddVerticesAndEdgeRange |> ignore
                    

            
            [edg eog (eog + 1) (RNGLR.NotAmbigousSimpleCalc.NUM (x + 1))                            
             edg (eog + 1) (eog + 2) (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF (x + 1))]
            |> qGraph.AddVerticesAndEdgeRange
            |> ignore
            //qGraph.PrintToDot "out.dot" (RNGLR.NotAmbigousSimpleCalc.tokenToNumber >> RNGLR.NotAmbigousSimpleCalc.numToString)
            qGraph

        let parse = (new Parser<_>()).Parse RNGLR.NotAmbigousSimpleCalc.buildAstAbstract
        perfTest parse inpLength graph

//    member this.``TSQL performance test`` i inpLength isLoop =  
//        let tpl x =
//            [
//             yield! [for y in 0 .. i - 2  -> edg x (x + 1) (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))]
//             yield edg (if isLoop then x + 1 else x) (if isLoop then x else x + 1) (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
//             yield edg (x + 1) (x + 2) (Yard.Examples.MSParserAbstract.L_comma_(new FSA<_>()))
//            ]
//
//        let graph x =
//            let eog = x * 2 + 3
//            let qGraph = new ParserInputGraph<_>(0, eog + 4)
//            for j in 0 .. x do
//                tpl (j * 2 + 1) |> qGraph.AddVerticesAndEdgeRange |> ignore
//
//            [ edg 0 1 (Yard.Examples.MSParserAbstract.L_select (new FSA<_>()))
//              edg eog (eog + 1) (Yard.Examples.MSParserAbstract.IDENT (new FSA<_>()))              
//              edg (eog + 1) (eog + 2) (Yard.Examples.MSParserAbstract.L_from (new FSA<_>()))
//              edg (eog + 2) (eog + 3) (Yard.Examples.MSParserAbstract.IDENT (new FSA<_>()))
//              edg (eog + 3) (eog + 4) (Yard.Examples.MSParserAbstract.RNGLR_EOF (new FSA<_>()))              
//            ]
//            |> qGraph.AddVerticesAndEdgeRange
//            |> ignore
//            //qGraph.PrintToDot "out.dot" (Yard.Examples.MSParser.tokenToNumber >> Yard.Examples.MSParser.numToString)
//            qGraph
//
//        let parse = (new Parser<_>()).Parse Yard.Examples.MSParserAbstract.buildAstAbstract
//        perfTest parse inpLength graph
//
//    member this.``TSQL performance test 2`` i inpLength isLoop =  
//        let tpl x =
//            [
//             yield! 
//                [
//                    for y in 0 .. i - 2  do
//                        yield edg x (x + y*2 + 2) (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
//                        yield edg (x + y*2 + 2) (x + y*2 + 3) (Yard.Examples.MSParserAbstract.L_plus_(new FSA<_>()))
//                        yield edg (x + y*2 + 3) (x + 1) (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
//                ]
//             yield edg (if isLoop then x + 1 else x) (if isLoop then x else x + 1) (Yard.Examples.MSParserAbstract.DEC_NUMBER(new FSA<_>()))
//             yield edg (x + 1) (x + (i-2)*2 + 4) (Yard.Examples.MSParserAbstract.L_comma_(new FSA<_>()))
//            ]
//
//        let graph x =
//            let eog = (x + 1) * (2 + (i-1) * 2) + 1
//            let qGraph = new ParserInputGraph<_>(0, eog + 4)
//            for j in 0 .. x do
//                tpl (j * (2 + (i-1) * 2) + 1) |> qGraph.AddVerticesAndEdgeRange |> ignore
//
//            [ edg 0 1 (Yard.Examples.MSParserAbstract.L_select (new FSA<_>()))
//              edg eog (eog + 1) (Yard.Examples.MSParserAbstract.IDENT (new FSA<_>()))              
//              edg (eog + 1) (eog + 2) (Yard.Examples.MSParserAbstract.L_from (new FSA<_>()))
//              edg (eog + 2) (eog + 3) (Yard.Examples.MSParserAbstract.IDENT (new FSA<_>()))
//              edg (eog + 3) (eog + 4) (Yard.Examples.MSParserAbstract.RNGLR_EOF (new FSA<_>()))              
//            ]
//            |> qGraph.AddVerticesAndEdgeRange
//            |> ignore
//            //qGraph.PrintToDot "out.dot" (Yard.Examples.MSParser.tokenToNumber >> Yard.Examples.MSParser.numToString)
//            qGraph
//
//        let parse = (new Parser<_>()).Parse Yard.Examples.MSParserAbstract.buildAstAbstract
//        perfTest parse inpLength graph
//
//
//    member this.``TSQL performance test for Alvor`` i inpLength isLoop =  
//        let tpl x =
//            [
//             yield! [ for y in 0 .. i - 2 -> sprintf "\"X%A + Y%A\"" (x + y*2 + 2)  (x + y*2 + 3)]
//             yield sprintf "\"%A\"" x             
//            ] |> String.concat ", "
//            |> fun s -> "{" + s + "}"
//            |> fun s -> if isLoop then "(" + s + ")+" else s 
//
//
//        let graph x =
//            let eog = (x + 1) * (2 + (i-1) * 2) + 1
//            let qGraph = new ParserInputGraph<_>(0, eog + 4)
//            let query = 
//                [for j in 0 .. x -> tpl (j * (2 + (i-1) * 2) + 1)]
//                |> String.concat "\",\""
//
//            "\"select \"" + query + "\", ddd from tbl\""
//            //qGraph.PrintToDot "out.dot" (Yard.Examples.MSParser.tokenToNumber >> Yard.Examples.MSParser.numToString)
//        seq{for i in 0..inpLength -> graph i}
//        |> fun s -> System.IO.File.WriteAllLines("sql_perf.txt",s)
//

    [<Test>]
    member this.bio1_2 () =
        let final = 1000 
        let qGraph = new ParserInputGraph<_>(0, final)
        let rand = new System.Random()
        let getSmb i = 
            match rand.Next(0,3) with
            | 0 -> RNGLR.bio1.A i
            | 1 -> RNGLR.bio1.B i
            | 2 -> RNGLR.bio1.C i
            | 3 -> RNGLR.bio1.D i
        [for i in 0 .. (final-1) do yield! [edg i (i + 1) (getSmb i); edg (i+1) final (RNGLR.bio1.RNGLR_EOF i)]]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore
        test RNGLR.bio1.buildAstAbstract qGraph 25 24 4 8 1


    [<Test>]
    member this.bio2_4 () =
        let bp = @"D:\YC\YaccConstructor\Tests\bio\infernal\"
        let file = 
            "1k-tRNA.fa"
            //"t1.fa"
            //"tremitted-Plant_SRP.fa"
            //"1k-4.fa"
        let textData =             
            File.ReadAllLines(Path.Combine(bp,file))
            |> Seq.skip 1
            |> Seq.takeWhile (fun s -> not <| s.StartsWith">")
            |> Seq.collect(fun s -> s.ToCharArray())
        let i = ref 0
        let getSmb ch i = 
            match ch with
            | 'A' -> RNGLR.bio2.A i
            | 'U' -> RNGLR.bio2.U i
            | 'C' -> RNGLR.bio2.C i
            | 'G' -> RNGLR.bio2.G i
        let edges = 
            textData
            |> Seq.mapi(fun i ch -> edg i (i+1) (getSmb ch i))
            |> Array.ofSeq
        let l = edges |> Array.length
        
        let qGraph = new ParserInputGraph<_>([|0..l|], [|l + 1|])
        qGraph.AddVerticesAndEdgeRange edges |> ignore
        qGraph.AddVerticesAndEdgeRange [for i in 0..l -> edg i (l+1) (RNGLR.bio2.RNGLR_EOF 0)] 
        test RNGLR.bio2.buildAstAbstract qGraph 25 24 4 8 1

    [<Test>]
    member this.bio2_3 () =
        let final = 300 
        let qGraph = new ParserInputGraph<_>(0, final)
        let rand = new System.Random()
        let getSmb i = 
            match rand.Next(0,3) with
            | 0 -> RNGLR.bio2.A i
            | 1 -> RNGLR.bio2.U i
            | 2 -> RNGLR.bio2.C i
            | 3 -> RNGLR.bio2.G i
        [yield! [for i in 0 .. (final-1) -> edg i (i + 1) (getSmb i)]; yield edg (final-1) final (RNGLR.bio2.RNGLR_EOF 0)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore
        test RNGLR.bio2.buildAstAbstract qGraph 25 24 4 8 1

    [<Test>]
    member this.bio1 () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.bio1.C 0)
            edg 1 2 (RNGLR.bio1.B 1)
            edg 2 3 (RNGLR.bio1.A 2)
            edg 3 4 (RNGLR.bio1.B 3)
            edg 4 1 (RNGLR.bio1.C 4)
            edg 1 9 (RNGLR.bio1.C 5)
            edg 1 5 (RNGLR.bio1.D 6)
            edg 5 6 (RNGLR.bio1.B 7)
            edg 6 7 (RNGLR.bio1.A 8)
            edg 7 8 (RNGLR.bio1.D 9)
            edg 6 3 (RNGLR.bio1.B 10)
            edg 8 10 (RNGLR.bio1.RNGLR_EOF 0)
            edg 9 10 (RNGLR.bio1.RNGLR_EOF 0)
            edg 1 10 (RNGLR.bio1.RNGLR_EOF 0)
            edg 2 10 (RNGLR.bio1.RNGLR_EOF 0)
            edg 3 10 (RNGLR.bio1.RNGLR_EOF 0)
            edg 4 10 (RNGLR.bio1.RNGLR_EOF 0)
            edg 5 10 (RNGLR.bio1.RNGLR_EOF 0)
            edg 6 10 (RNGLR.bio1.RNGLR_EOF 0)
            edg 7 10 (RNGLR.bio1.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.bio1.buildAstAbstract qGraph 25 24 4 8 1

    [<Test>]
    member this.Testsss () =
        let grGenerator len = 
            let final = 110
            let qGraph = new ParserInputGraph<_>(0, final)
            let mutable b = 0
            let mutable e = 1
            for i = 0 to len - 1 do
                qGraph.AddVerticesAndEdge <| edg  b e (RNGLR.Tesssst.B 0) |> ignore
                b <- e
                e <- e + 1
            qGraph.AddVerticesAndEdge <| edg  b final (RNGLR.Tesssst.RNGLR_EOF 0) |> ignore
            //qGraph.FinalStates <- [|e|]
            qGraph
        for i in 1..100 do
            let qGraph = grGenerator i
            let start = System.DateTime.Now
            let r = (new Parser<_>()).Parse  RNGLR.Tesssst.buildAstAbstract qGraph
            printfn "%A" (System.DateTime.Now - start)
    

        //test RNGLR.bio1.buildAstAbstract qGraph 25 24 4 8 1


    [<Test>]
    member this.bio2 () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.bio2.C 0)
            edg 1 2 (RNGLR.bio2.U 1)
            edg 2 3 (RNGLR.bio2.A 2)
            edg 3 4 (RNGLR.bio2.U 3)
            edg 4 1 (RNGLR.bio2.C 4)
            edg 1 9 (RNGLR.bio2.C 5)
            edg 1 5 (RNGLR.bio2.G 6)
            edg 5 6 (RNGLR.bio2.U 7)
            edg 6 7 (RNGLR.bio2.A 8)
            edg 7 8 (RNGLR.bio2.G 9)
            edg 6 3 (RNGLR.bio2.U 10)
            edg 8 10 (RNGLR.bio2.RNGLR_EOF 0)
            edg 9 10 (RNGLR.bio2.RNGLR_EOF 0)
            //edg 1 10 (RNGLR.bio2.RNGLR_EOF 0)
            //edg 2 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 3 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 4 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 5 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 6 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 7 10 (RNGLR.bio2.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.bio2.buildAstAbstract qGraph 25 24 4 8 1

    [<Test>]
    member this.bio2_2 () =
        let qGraph = new ParserInputGraph<_>(0, 17)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.bio2.C 0)
            edg 1 2 (RNGLR.bio2.A 1)
            edg 2 3 (RNGLR.bio2.U 2)
            edg 3 4 (RNGLR.bio2.C 3)
            edg 4 5 (RNGLR.bio2.G 4)
            edg 5 6 (RNGLR.bio2.C 5)
            edg 6 7 (RNGLR.bio2.A 6)
            edg 7 8 (RNGLR.bio2.A 7)
            edg 8 9 (RNGLR.bio2.C 8)
            edg 9 10 (RNGLR.bio2.C 9)
            edg 10 11 (RNGLR.bio2.G 10)
            edg 11 12 (RNGLR.bio2.C 10)
            edg 12 13 (RNGLR.bio2.G 10)
            edg 13 14 (RNGLR.bio2.A 10)
            edg 14 15 (RNGLR.bio2.U 10)
            edg 15 16 (RNGLR.bio2.G 10)
            edg 16 17 (RNGLR.bio2.RNGLR_EOF 0)            
            //edg 1 10 (RNGLR.bio2.RNGLR_EOF 0)
            //edg 2 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 3 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 4 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 5 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 6 10 (RNGLR.bio2.RNGLR_EOF 0)
//            edg 7 10 (RNGLR.bio2.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.bio2.buildAstAbstract qGraph 25 24 4 8 1

[<EntryPoint>]
let f x =
    if System.IO.Directory.Exists "dot" 
    then 
        System.IO.Directory.GetFiles "dot" |> Seq.iter System.IO.File.Delete
    else System.IO.Directory.CreateDirectory "dot" |> ignore
    let t = new ``RNGLR abstract parser tests`` () 
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
//    t._01_PrettySimpleCalc_SequenceInput ()
//    t._02_PrettySimpleCalc_SimpleBranchedInput ()
//    t._03_PrettySimpleCalc_BranchedInput ()
//    t._04_PrettySimpleCalc_LotsOfVariants() 
//    t._05_NotAmbigousSimpleCalc_LotsOfVariants()
   // t._06_NotAmbigousSimpleCalc_Loop()
//    t._07_NotAmbigousSimpleCalc_Loop2. ()
//    t._08_NotAmbigousSimpleCalc_Loop3. ()
//    t._09_NotAmbigousSimpleCalc_Loop4. ()
//    t._10_NotAmbigousSimpleCalc_Loop5. ()
//    t._11_NotAmbigousSimpleCalc_Loop6. ()
//    t._12_NotAmbigousSimpleCalc_Loop7. ()
//    t._13_NotAmbigousSimpleCalc_Loop8. ()
//    t._14_NotAmbigousSimpleCalcWith2Ops_Loop. ()
//    t._15_NotAmbigousSimpleCalcWith2Ops_Loops. ()
//    t._16_Stars_Loop. () 
//    t._17_Stars2_Loop. () 
//    t._18_Stars2_Loop2. () 
//    t._19_FirstEps ()
//    t._20_CroppedBrackets ()
//    t._21_Brackets ()
//    t._22_Brackets_BackEdge ()
//    t._23_UnambiguousBrackets ()
//    t._24_UnambiguousBrackets_Circle()
//    t._25_UnambiguousBrackets_BiggerCircle ()
//    t._26_UnambiguousBrackets_Inf()
//    t._27_UnambiguousBrackets_WithoutEmptyString()
//    t._28_UnambiguousBrackets_DifferentPathLengths ()
    //t.``TSQL performance test for GLL`` ()
    //t._29_AandB_Circle ()
    //t.``TSQL performance test 2`` 2 100 false
    t.Testsss()
    0
