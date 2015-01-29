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

let test buildAstAbstract qGraph= 
    let r = (new Parser<_>()).Parse  buildAstAbstract qGraph
    printfn "%A" r
    match r with
    | Error (num, tok, message) ->
        printfn "Error in position %d on Token %A: %s" num tok message
        Assert.Fail "!!!!!!"
    | Success(tree) ->
        tree.PrintAst()
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
    member this.``Epsilons. Sequence input.`` () =
        let qGraph = new ParserInputGraph<_>(0, 3)        
        qGraph.AddVerticesAndEdgeRange
            [edg 0 2 (RNGLR.Epsilons.NUM 1)
             //edg 1 2 (RNGLR.Epsilons.PLUS 0)
             edg 2 3 (RNGLR.Epsilons.RNGLR_EOF 5)
             ] |> ignore

        test RNGLR.Epsilons.buildAstAbstract qGraph

    [<Test>]
    member this.``Epsilons2. Sequence input.`` () =
        let qGraph = new ParserInputGraph<_>(0, 4)        
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.Epsilons2.ZZZ 1)
             edg 1 2 (RNGLR.Epsilons2.YYY 1)
             edg 2 3 (RNGLR.Epsilons2.NUM 2)
             //edg 2 3 (RNGLR.Epsilons2.PLUS 0)
             edg 3 4 (RNGLR.Epsilons2.RNGLR_EOF 5)
             ] |> ignore

        test RNGLR.Epsilons2.buildAstAbstract qGraph

    [<Test>]
    member this.``Pretty Simple Calc. Sequence input.`` () =
        let qGraph = new ParserInputGraph<_>(0, 4)        
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 0)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 2)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 5)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Pretty Simple Calc. Simple branched input.`` () =
        let qGraph = new ParserInputGraph<_>(0, 4)        
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 0)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 2)
             edg 0 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 5)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Pretty Simple Calc. Branched input.`` () =
        let qGraph = new ParserInputGraph<_>(2, 9)
        qGraph.AddVerticesAndEdgeRange
            [
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 2)
             edg 3 4 (RNGLR.PrettySimpleCalc.PLUS 3)
             edg 4 5 (RNGLR.PrettySimpleCalc.NUM 4)
             edg 3 6 (RNGLR.PrettySimpleCalc.PLUS 5)
             edg 6 5 (RNGLR.PrettySimpleCalc.NUM 6)
             edg 5 7 (RNGLR.PrettySimpleCalc.PLUS 3)
             edg 7 8 (RNGLR.PrettySimpleCalc.NUM 7)
             edg 8 9 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Pretty Simple Calc. Lots Of Variants.`` () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM  1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 0)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 2)
             edg 3 4 (RNGLR.PrettySimpleCalc.PLUS 3)
             edg 4 5 (RNGLR.PrettySimpleCalc.NUM 4)
             edg 3 6 (RNGLR.PrettySimpleCalc.PLUS 5)
             edg 6 5 (RNGLR.PrettySimpleCalc.NUM 6)
             edg 5 7 (RNGLR.PrettySimpleCalc.PLUS 3)
             edg 7 8 (RNGLR.PrettySimpleCalc.NUM 7)
             edg 8 9 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc. Lots Of Variants.`` () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 0)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 2)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 3)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalc.NUM 4)
             edg 3 6 (RNGLR.NotAmbigousSimpleCalc.PLUS 5)
             edg 6 5 (RNGLR.NotAmbigousSimpleCalc.NUM 6)
             edg 5 7 (RNGLR.NotAmbigousSimpleCalc.PLUS 3)
             edg 7 8 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 8 9 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc. Loop.`` () =
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
        
        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc. Loop2.`` () =
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
        
        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc. Loop3.`` () =
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

        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph
        
    [<Test>]
    member this.``Not Ambigous Simple Calc. Loop4.`` () =
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

        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc. Loop5.`` () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (RNGLR.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 3 (RNGLR.NotAmbigousSimpleCalc.NUM 6)
             edg 3 8 (RNGLR.NotAmbigousSimpleCalc.PLUS 8)
             edg 8 3 (RNGLR.NotAmbigousSimpleCalc.NUM 9)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 6 9 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc. Loop6.`` () =
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


        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc. Loop7.`` () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalc.NUM 5)
             edg 7 5 (RNGLR.NotAmbigousSimpleCalc.NUM 9)
             edg 5 7 (RNGLR.NotAmbigousSimpleCalc.PLUS 6)
             edg 7 1 (RNGLR.NotAmbigousSimpleCalc.NUM 8)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 6 8 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc. Loop8.`` () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalc.NUM 5)
             edg 5 2 (RNGLR.NotAmbigousSimpleCalc.PLUS 6)
             edg 7 5 (RNGLR.NotAmbigousSimpleCalc.NUM 9)
             edg 5 7 (RNGLR.NotAmbigousSimpleCalc.PLUS 6)
             edg 7 1 (RNGLR.NotAmbigousSimpleCalc.NUM 8)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalc.NUM 7)
             edg 6 8 (RNGLR.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test RNGLR.NotAmbigousSimpleCalc.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc With 2 Ops. Loop.`` (0, 7) =
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
        
        test RNGLR.NotAmbigousSimpleCalcWith2Ops.buildAstAbstract qGraph

    [<Test>]
    member this.``Not Ambigous Simple Calc With 2 Ops. Loops.`` () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM  1)
             edg 1 2 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 6)
             edg 2 3 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 7)
             edg 2 4 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 2)
             edg 3 4 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 5)
             edg 4 5 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 8)
             edg 5 2 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 4)
             edg 4 6 (RNGLR.NotAmbigousSimpleCalcWith2Ops.PLUS 9)
             edg 6 7 (RNGLR.NotAmbigousSimpleCalcWith2Ops.NUM 3)
             edg 7 8 (RNGLR.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 113)
             ] |> ignore
        
        test RNGLR.NotAmbigousSimpleCalcWith2Ops.buildAstAbstract qGraph

    [<Test>]
    member this.``Stars. Loop.`` () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars.STAR 1)
             edg 0 1 (RNGLR.Stars.SEMI 2)
             edg 1 2 (RNGLR.Stars.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars.buildAstAbstract qGraph

    [<Test>]
    member this.``Stars2. Loop.`` () =
        let qGraph = new ParserInputGraph<_>(0, 1)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars2.STAR 1)
             edg 0 1 (RNGLR.Stars2.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars2.buildAstAbstract qGraph

    [<Test>]
    member this.``Stars2. Loop2.`` () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (RNGLR.Stars2.STAR 1)
             edg 0 1 (RNGLR.Stars2.STAR 2)
             edg 1 2 (RNGLR.Stars2.RNGLR_EOF 0)
             ] |> ignore
        
        test RNGLR.Stars2.buildAstAbstract qGraph

//    [<Test>]
//    member this.``Calc. Sequence input.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.ParseCalc.NUMBER  1)
//             edg 1 2 (RNGLR.ParseCalc.PLUS 0)
//             edg 2 3 (RNGLR.ParseCalc.NUMBER 2)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _ ,_) ->
//            tree.PrintAst()
//            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass()

//    [<Test>]
//    member this.``Calc. Branched input.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.ParseCalc.NUMBER  1)
//             edg 1 2 (RNGLR.ParseCalc.PLUS 0)
//             edg 2 3 (RNGLR.ParseCalc.NUMBER 2)
//             edg 3 4 (RNGLR.ParseCalc.MULT 3)
//             edg 4 5 (RNGLR.ParseCalc.NUMBER 4)
//             edg 3 6 (RNGLR.ParseCalc.DIV 5)
//             edg 6 5 (RNGLR.ParseCalc.NUMBER 6)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
//    [<Test>]
//    member this.``Calc. Branched input error.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVerticesAndEdgeRange
//            [
//             edg 0 3 (RNGLR.ParseCalc.NUMBER 2)
//             edg 3 4 (RNGLR.ParseCalc.MULT 3)
//             edg 4 5 (RNGLR.ParseCalc.NUMBER 4)
//             edg 3 6 (RNGLR.ParseCalc.DIV 5)
//             edg 6 5 (RNGLR.ParseCalc.PLUS 6)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Pass()
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
//            Assert.Fail "This test should "
//    
//    [<Test>]
//    member this.``Pretty Simple Calc. Error Handling Temp.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVerticesAndEdgeRange
//            [
//             edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
//             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
//             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
//             edg 3 4 (RNGLR.PrettySimpleCalc.PLUS 4)
//             edg 4 5 (RNGLR.PrettySimpleCalc.NUM 5)
//             edg 5 6 (RNGLR.PrettySimpleCalc.RNGLR_EOF 6)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.PrettySimpleCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.PrettySimpleCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass() 
//
//    [<Test>]
//    member this.``Pretty Simple Calc. Error Is Not Handled Without EOF.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVerticesAndEdgeRange
//            [
//             edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
//             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
//             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
//             edg 1 3 (RNGLR.PrettySimpleCalc.PLUS 4)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.PrettySimpleCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.PrettySimpleCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass() 
//
//    [<Test>]
//    member this.``Pretty Simple Calc. Error Is Handled With EOF.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVerticesAndEdgeRange
//            [
//             edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
//             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
//             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
//             edg 1 3 (RNGLR.PrettySimpleCalc.PLUS 4)
//             edg 1 3 (RNGLR.PrettySimpleCalc.RNGLR_EOF 5)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.PrettySimpleCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Pass()
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.PrettySimpleCalc.defaultAstToDot tree "ast.dot"
//            Assert.Fail "!!!!" 
//
//    [<Test>]
//    member this.``Calc. Branched input 2.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.ParseCalc.NUMBER  1)
//             edg 1 2 (RNGLR.ParseCalc.PLUS 0)
//             edg 2 3 (RNGLR.ParseCalc.NUMBER 2)
//             edg 3 4 (RNGLR.ParseCalc.MULT 3)
//             edg 4 5 (RNGLR.ParseCalc.NUMBER 4)
//             edg 3 6 (RNGLR.ParseCalc.MINUS 5)
//             edg 6 5 (RNGLR.ParseCalc.NUMBER 6)
//             edg 5 7 (RNGLR.ParseCalc.MULT 3)
//             edg 7 8 (RNGLR.ParseCalc.NUMBER 4)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
////    [<Test>]
////    member this.``Simple calc. Sequence input. Full.`` () =
////        let lexerInputGraph = loadLexerInputGraph "test_8.dot"
////        let qGraph = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph 
////        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
////        printfn "%A" r
////        match r with
////        | Error (num, tok, message, debug, _) ->
////            printfn "Error in position %d on Token %A: %s" num tok message
////            debug.drawGSSDot "out.dot"
////        | Success(tree, _, _) ->
////            tree.PrintAst()
////            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
////        Assert.Pass()
//
//    [<Test>]
//    member this.``Simple calc. Branch binop input.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.ParseSimpleCalc.NUM 1)
//             edg 1 2 (RNGLR.ParseSimpleCalc.PLUS 0)
//             edg 1 2 (RNGLR.ParseSimpleCalc.PLUS 3)
//             edg 2 3 (RNGLR.ParseSimpleCalc.NUM 2)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
//    [<Test>]
//    member this.``Lexer and parser`` () =
//        let lexerInputGraph = loadLexerInputGraph "lexer_and_parser_simple_test.dot"
//        let qGraph = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, RNGLR.ParseCalc.RNGLR_EOF 0)
//
//        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
//    [<Test>]
//    member this.``Errors 1`` () =
//        errorTest "errors1.dot" true 1
//    
//    [<Test>]
//    member this.``Errors 2`` () =
//        errorTest "errors2.dot" true 1
//
//    [<Test>]
//    member this.``Errors 3`` () =
//        errorTest "errors3.dot" true 1
//
//    [<Test>]
//    member this.``Errors 4`` () =
//        errorTest "errors4.dot" false 0
//
//    [<Test>]
//    member this.``Errors 5`` () =
//        errorTest "errors5.dot" true 1
//
//    [<Test>]
//    member this.``Errors 6`` () =
//        errorTest "errors6.dot" true 1
//
//    [<Test>]
//    member this.``Errors 8`` () =
//        errorTest "errors8.dot" true 1
//
//    [<Test>]
//    member this.``Errors 9`` () =
//        errorTest "errors9.dot" true 1
//        
//    [<Test>]
//    member this.``Errors 10`` () =
//        errorTest "errors10.dot" false 2
//
//    [<Test>]
//    member this.``Errors 11`` () =
//        errorTest "errors11.dot" false 3
//
//    [<Test>]
//    member this.``Errors 12`` () =
//        errorTest "errors12.dot" false 3
//    
//    [<Test>]
//    member this.``Errors 13`` () =
//        errorTest "errors13.dot" true 1
//    
//    [<Test>]
//    member this.``Errors 14`` () =
//        errorTest "errors14.dot" true 2
//
//    [<Test>]
//    member this.``Errors 15`` () =
//        errorTest "errors15.dot" true 2
//
//    [<Test>]
//    member this.``Errors 16`` () =
//        errorTest "errors16.dot" true 1
//
//    [<Test>]
//    member this.``Simple calc. Branch binop and second arg.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.ParseSimpleCalc.NUM 1)
//             edg 1 2 (RNGLR.ParseSimpleCalc.PLUS 0)
//             edg 1 3 (RNGLR.ParseSimpleCalc.PLUS 3)
//             edg 2 4 (RNGLR.ParseSimpleCalc.NUM 2)
//             edg 3 4 (RNGLR.ParseSimpleCalc.NUM 4)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
//    [<Test>]
//    member this.``Simple calc. Branch binop and first arg.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.ParseSimpleCalc.NUM 1)
//             edg 0 2 (RNGLR.ParseSimpleCalc.NUM 2)
//             edg 1 3 (RNGLR.ParseSimpleCalc.PLUS 3)
//             edg 2 3 (RNGLR.ParseSimpleCalc.PLUS 4)
//             edg 3 4 (RNGLR.ParseSimpleCalc.NUM 5)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
//    [<Test>]
//    member this.``Simple calc with nterm. Seq input.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.SimpleCalcWithNTerm.NUM 1)
//             edg 1 2 (RNGLR.SimpleCalcWithNTerm.PLUS 0)
//             edg 2 3 (RNGLR.SimpleCalcWithNTerm.NUM 2)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerm.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.SimpleCalcWithNTerm.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
//
//    [<Test>]
//    member this.``Simple calc with nterm. Branch binop and first arg.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.SimpleCalcWithNTerm.NUM 1)
//             edg 0 2 (RNGLR.SimpleCalcWithNTerm.NUM 2)
//             edg 1 3 (RNGLR.SimpleCalcWithNTerm.PLUS 3)
//             edg 2 3 (RNGLR.SimpleCalcWithNTerm.PLUS 4)
//             edg 3 4 (RNGLR.SimpleCalcWithNTerm.NUM 5)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerm.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.SimpleCalcWithNTerm.defaultAstToDot tree "ast.dot"            
//            Assert.Pass()
//
//    [<Test>]
//    member this.``Simple calc with nterm 2. Seq input.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.SimpleCalcWithNTerms_2.NUM 1)
//             edg 1 2 (RNGLR.SimpleCalcWithNTerms_2.PLUS 0)
//             edg 2 3 (RNGLR.SimpleCalcWithNTerms_2.NUM 2)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_2.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.SimpleCalcWithNTerms_2.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
//
//    [<Test>]
//    member this.``Simple calc with nterm 2. Brabch first operand.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.SimpleCalcWithNTerms_2.NUM 1)
//             edg 0 1 (RNGLR.SimpleCalcWithNTerms_2.NUM 3)
//             edg 1 2 (RNGLR.SimpleCalcWithNTerms_2.PLUS 0)
//             edg 2 3 (RNGLR.SimpleCalcWithNTerms_2.NUM 2)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_2.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.SimpleCalcWithNTerms_2.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
//    [<Test>]
//    member this.``Simple calc with nterm 2. Fully brabched.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.SimpleCalcWithNTerms_2.NUM 1)
//             edg 0 4 (RNGLR.SimpleCalcWithNTerms_2.NUM 3)
//             edg 4 5 (RNGLR.SimpleCalcWithNTerms_2.PLUS 4)
//             edg 5 3 (RNGLR.SimpleCalcWithNTerms_2.NUM 5)
//             edg 1 2 (RNGLR.SimpleCalcWithNTerms_2.PLUS 0)
//             edg 2 3 (RNGLR.SimpleCalcWithNTerms_2.NUM 2)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_2.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.SimpleCalcWithNTerms_2.defaultAstToDot tree "ast.dot"        
//            Assert.Pass()
//
//    [<Test>]
//    member this.``Simple calc with nterm 3. Seq input.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.SimpleCalcWithNTerms_3.NUM 1)
//             edg 1 2 (RNGLR.SimpleCalcWithNTerms_3.PLUS 0)
//             edg 2 3 (RNGLR.SimpleCalcWithNTerms_3.NUM 2)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_3.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.SimpleCalcWithNTerms_3.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
////    [<Test>]
////    member this.``Calc demo`` () =
////        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
////        //qGraph.AddVertexRange[0;1;2;3] |> ignore
////        let n = RNGLR.ParseSimpleCalcDemo.NUM 
////        let p = RNGLR.ParseSimpleCalcDemo.PLUS 
////        let m = RNGLR.ParseSimpleCalcDemo.MULT
////        let l = RNGLR.ParseSimpleCalcDemo.LBR
////        let r = RNGLR.ParseSimpleCalcDemo.RBR
////        qGraph.AddVerticesAndEdgeRange
////            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| n 1)
////             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| p 0)
////             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| n 4)
////             new AbstractParsing.Common.ParserEdge<_>(3,8,lbl <| m 4)
////             new AbstractParsing.Common.ParserEdge<_>(1,4,lbl <| m 5)
////             new AbstractParsing.Common.ParserEdge<_>(4,5,lbl <| n 5)
////             new AbstractParsing.Common.ParserEdge<_>(5,8,lbl <| p 1)
////             new AbstractParsing.Common.ParserEdge<_>(1,6,lbl <| p 2)
////             new AbstractParsing.Common.ParserEdge<_>(6,7,lbl <| n 6)
////             new AbstractParsing.Common.ParserEdge<_>(7,8,lbl <| p 3)
////             new AbstractParsing.Common.ParserEdge<_>(8,9,lbl <| l 0)
////             new AbstractParsing.Common.ParserEdge<_>(9,10,lbl <| n 7)
////             new AbstractParsing.Common.ParserEdge<_>(10,11,lbl <| m 6)
////             new AbstractParsing.Common.ParserEdge<_>(11,12,lbl <| n 8)
////             new AbstractParsing.Common.ParserEdge<_>(12,13,lbl <| r 1)
////             ] |> ignore
////
////        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalcDemo.buildAstAbstract qGraph
////        printfn "%A" r
////        match r with
////        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
////            printfn "Error in position %d on Token %A: %s" num tok message
////            debug.drawGSSDot "out.dot"
////            Assert.Fail "!!!!!!"
////        | Yard.Generators.RNGLR.Parser.Success(tree, _, _) ->
////            tree.PrintAst()
////            RNGLR.ParseSimpleCalcDemo.defaultAstToDot tree "ast.dot"
////            Assert.Pass()
//
//
//    [<Test>]
//    member this.``Simple calc with nterm 4. Seq input.`` () =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0;1;2;3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (RNGLR.SimpleCalcWithNTerms_4.NUM 1)
//             edg 1 2 (RNGLR.SimpleCalcWithNTerms_4.PLUS 0)
//             edg 2 3 (RNGLR.SimpleCalcWithNTerms_4.NUM 2)
//             ] |> ignore
//
//        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_4.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//            Assert.Fail "!!!!!!"
//        | Success(tree, _, _) ->
//            tree.PrintAst()
//            RNGLR.SimpleCalcWithNTerms_4.defaultAstToDot tree "ast.dot"
//            Assert.Pass()
//
////    member this.tsqlPerpT() =
////        let loadLexerInputGraph gFile =
////            let qGraph = loadDotToQG baseInputGraphsPath gFile
////            let lexerInputG = new AbstractLexer.Common.LexerInputGraph<_>()
////            lexerInputG.StartVertex <- 0
////            for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AbstractLexer.Common.LexerEdge<_,_>(e.Source,e.Target,Some (e.Tag, null)))
////            lexerInputG
////        for i in [10..10] do
////            let bp = System.IO.Path.Combine(baseInputGraphsPath,@"..\..\AbstractPerformance\TSQL\" + string i)
////            let times = new ResizeArray<_>(250)
////            for f in System.IO.Directory.GetFiles(bp,"*.dot") 
////                     |> Array.filter(fun x -> System.IO.Path.GetFileNameWithoutExtension x |> int <= 55)
////                     |> Array.sortBy(fun x -> System.IO.Path.GetFileNameWithoutExtension x |> int)
////                      do
////                let lexerInputGraph = loadLexerInputGraph(f)
////                let eof = Yard.Examples.MSParser.RNGLR_EOF(Yard.Utils.SourceText.SourceText(),[||])
////                let qGraph = MSLexer._fslex_tables.Tokenize(MSLexer.fslex_actions_tokens, lexerInputGraph, eof)
////                let start = System.DateTime.Now
////                let r = (new Parser<_>()).Parse Yard.Examples.MSParser.buildAstAbstract qGraph
////                let t = (System.DateTime.Now-start) 
////                match r with 
////                |  Yard.Generators.RNGLR.Parser.Success(tree, _) ->
////                    let f = System.IO.Path.GetFileName f
////                    times.Add (f + " " + string t.TotalSeconds)
////                    printfn "%i %s %A" i f t.TotalSeconds  
////                | _ -> ()
////            System.IO.File.WriteAllLines(System.IO.Path.Combine(bp,sprintf "arnglr_%i" i),times)

[<EntryPoint>]
let f x =
    if System.IO.Directory.Exists "dot" 
    then 
        System.IO.Directory.GetFiles "dot" |> Seq.iter System.IO.File.Delete
    else System.IO.Directory.CreateDirectory "dot" |> ignore
    let t = new ``RNGLR abstract parser tests`` () 

//    t.``Pretty Simple Calc. Sequence input.`` ()
//    t.``Pretty Simple Calc. Simple branched input.`` ()
//    t.``Pretty Simple Calc. Branched input.`` ()
//    t.``Pretty Simple Calc. Lots Of Variants.``() 
//    t.``Not Ambigous Simple Calc. Lots Of Variants.``()
//    t.``Not Ambigous Simple Calc. Loop.`` ()
//    t.``Not Ambigous Simple Calc. Loop2.`` ()
//    t.``Not Ambigous Simple Calc. Loop3.`` ()
//    t.``Not Ambigous Simple Calc. Loop4.`` ()
//    t.``Not Ambigous Simple Calc. Loop5.`` ()
//    t.``Not Ambigous Simple Calc. Loop6.`` ()
//    t.``Not Ambigous Simple Calc. Loop7.`` ()
//    t.``Not Ambigous Simple Calc. Loop8.`` ()
//    t.``Not Ambigous Simple Calc With 2 Ops. Loop.`` ()
//    t.``Not Ambigous Simple Calc With 2 Ops. Loops.`` ()
//    t.``Stars. Loop.`` () 
    //t.``Stars2. Loop.`` () 
    t.``Epsilons2. Sequence input.``()
    0
    