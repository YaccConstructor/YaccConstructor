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
open RNGLR.ParseSimpleCalc
open RNGLR.PrettySimpleCalc
open Yard.Generators.RNGLR.AbstractParser
open YC.Tests.Helper
open Yard.Generators.ARNGLR.Parser


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
    let r = (new Parser<_>()).Parse  buildAstAbstract qGraph
    printfn "%A" r
    match r with
    | Error (num, tok, message) ->
        printfn "Error in position %d on Token %A: %s" num tok message
        Assert.Fail "!!!!!!"
    | Success(tree) ->
        //tree.PrintAst()
        let n, e, eps, t, amb = tree.CountCounters()
        Assert.AreEqual(nodesCount, n, "Nodes count mismatch")
        Assert.AreEqual(edgesCount, e, "Edges count mismatch")
        Assert.AreEqual(epsilonsCount, eps, "Epsilons count mismatch")
        Assert.AreEqual(termsCount, t, "Terms count mismatch")
        Assert.AreEqual(ambiguityCount, amb, "Ambiguities count mismatch")
        Assert.Pass()

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
    member this.``01_PrettySimpleCalc.SequenceInput.`` () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 13 12 0 3 0

    [<Test>]
    member this.``02_PrettySimpleCalc.SimpleBranchedInput.`` () =
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
    member this.``03_PrettySimpleCalc.BranchedInput.`` () =
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
    member this.``04_PrettySimpleCalc.LotsOfVariants.`` () =
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
    member this.``05_NotAmbigousSimpleCalc.LotsOfVariants.`` () =
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
    member this.``06_NotAmbigousSimpleCalc.Loop.`` () =
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
    member this.``07_NotAmbigousSimpleCalc.Loop2.`` () =
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
    member this.``08_NotAmbigousSimpleCalc.Loop3.`` () =
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
    member this.``09_NotAmbigousSimpleCalc.Loop4.`` () =
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
    member this.``10_NotAmbigousSimpleCalc.Loop5.`` () =
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
    member this.``11_NotAmbigousSimpleCalc.Loop6.`` () =
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
    member this.``12_NotAmbigousSimpleCalc.Loop7.`` () =
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
    member this.``13_NotAmbigousSimpleCalc.Loop8.`` () =
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
    member this.``14_NotAmbigousSimpleCalcWith2Ops.Loop.`` () =
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
    member this.``15_NotAmbigousSimpleCalcWith2Ops.Loops.`` () =
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
    member this.``16_Stars.Loop.`` () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars.STAR 1)
             edg 0 1 (RNGLR.Stars.SEMI 2)
             edg 1 2 (RNGLR.Stars.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars.buildAstAbstract qGraph 15 14 0 6 1

    [<Test>]
    member this.``17_Stars2.Loop.`` () =
        let qGraph = new ParserInputGraph<_>(0, 1)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars2.STAR 1)
             edg 0 1 (RNGLR.Stars2.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars2.buildAstAbstract qGraph 34 36 0 10 4

    [<Test>]
    member this.``18_Stars2.Loop2.`` () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars2.STAR 1)
             edg 0 1 (RNGLR.Stars2.STAR 2)
             edg 1 2 (RNGLR.Stars2.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars2.buildAstAbstract qGraph 42 42 0 14 4

    [<Test>]
    member this.``19_FirstEps`` () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.FirstEps.Z 1)
            edg 1 3 (RNGLR.FirstEps.N 2)
            edg 3 4 (RNGLR.FirstEps.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.FirstEps.buildAstAbstract qGraph 14 13 2 2 0
    
    [<Test>]
    member this.``20_CroppedBrackets`` () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (RNGLR.CroppedBrackets.LBR 1)
            edg 0 1 (RNGLR.CroppedBrackets.NUM 2)
            edg 1 1 (RNGLR.CroppedBrackets.RBR 3)
            edg 1 2 (RNGLR.CroppedBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.CroppedBrackets.buildAstAbstract qGraph 20 20 0 9 3

    [<Test>]
    member this.``21_Brackets`` () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (RNGLR.Brackets.LBR 1)
            edg 0 1 (RNGLR.Brackets.NUM 2)
            edg 1 1 (RNGLR.Brackets.RBR 3)
            edg 1 2 (RNGLR.Brackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.Brackets.buildAstAbstract qGraph 20 20 0 9 3

    [<Test>]
    member this.``22_Brackets.BackEdge`` () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (RNGLR.Brackets.LBR 1)
            edg 0 1 (RNGLR.Brackets.NUM 2)
            edg 1 1 (RNGLR.Brackets.RBR 3)
            edg 1 0 (RNGLR.Brackets.NUM 4)
            edg 1 2 (RNGLR.Brackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.Brackets.buildAstAbstract qGraph 20 20 0 9 3

    [<Test>]
    member this.``22_StrangeBrackets`` () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 1)
            edg 1 1 (RNGLR.StrangeBrackets.LBR 2)
            edg 1 2 (RNGLR.StrangeBrackets.RBR 3)
            edg 2 2 (RNGLR.StrangeBrackets.RBR 4)
            edg 2 3 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 20 20 0 9 3

    [<Test>]
    member this.``22_StrangeBrackets.BackEdge`` () =
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            //edg 1 1 (RNGLR.StrangeBrackets.LBR 2)
           [edg 0 1 (RNGLR.StrangeBrackets.LBR 1)
            edg 1 2 (RNGLR.StrangeBrackets.LBR 3)
            edg 2 3 (RNGLR.StrangeBrackets.RBR 1)
            edg 3 4 (RNGLR.StrangeBrackets.RBR 3)
            edg 4 5 (RNGLR.StrangeBrackets.LBR 1)
            edg 5 6 (RNGLR.StrangeBrackets.RBR 3)
            //edg 2 2 (RNGLR.StrangeBrackets.RBR 4)
            //edg 2 4 (RNGLR.StrangeBrackets.LBR 5)
            //edg 4 0 (RNGLR.StrangeBrackets.RBR 6)
            edg 6 7 (RNGLR.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test RNGLR.StrangeBrackets.buildAstAbstract qGraph 20 20 0 9 3

[<EntryPoint>]
let f x =
    if System.IO.Directory.Exists "dot" 
    then 
        System.IO.Directory.GetFiles "dot" |> Seq.iter System.IO.File.Delete
    else System.IO.Directory.CreateDirectory "dot" |> ignore
    let t = new ``RNGLR abstract parser tests`` () 

//    t.``01_PrettySimpleCalc.SequenceInput.`` ()
//    t.``02_PrettySimpleCalc.SimpleBranchedInput.`` ()
//    t.``03_PrettySimpleCalc.BranchedInput.`` ()
//    t.``04_PrettySimpleCalc.LotsOfVariants.``() 
//    t.``05_NotAmbigousSimpleCalc.LotsOfVariants.``()
//    t.``06_NotAmbigousSimpleCalc.Loop.`` ()
//    t.``07_NotAmbigousSimpleCalc.Loop2.`` ()
//    t.``08_NotAmbigousSimpleCalc.Loop3.`` ()
//    t.``09_NotAmbigousSimpleCalc.Loop4.`` ()
//    t.``10_NotAmbigousSimpleCalc.Loop5.`` ()
//    t.``11_NotAmbigousSimpleCalc.Loop6.`` ()
//    t.``12_NotAmbigousSimpleCalc.Loop7.`` ()
//    t.``13_NotAmbigousSimpleCalc.Loop8.`` ()
//    t.``14_NotAmbigousSimpleCalcWith2Ops.Loop.`` ()
//    t.``15_NotAmbigousSimpleCalcWith2Ops.Loops.`` ()
//    t.``16_Stars.Loop.`` () 
//    t.``17_Stars2.Loop.`` () 
//    t.``18_Stars2.Loop2.`` () 
//    t.``19_FirstEps`` ()
//    t.``20_CroppedBrackets`` ()
    //t.``21_Brackets`` ()
    t.``22_StrangeBrackets.BackEdge`` ()
    0
    