//  RNGLRAbstractParserTests.fs contains tests for RNGLRAbstractParser project.
//
//  Copyright 2013 Semyon Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.


module RNGLRAbstractParserTests

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractParsing.Common
open RNGLR.ParseSimpleCalc
open Yard.Generators.RNGLR.AbstractParser
open AbstractLexer.Common

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../../../../Tests/AbstractRNGLR/DOT"

let path name = System.IO.Path.Combine(baseInputGraphsPath,name)

let loadDotToQG gFile =
    let g = loadGraphFromDOT(path gFile)
    let qGraph = new AdjacencyGraph<int, TaggedEdge<_,string>>()
    g.Edges 
    |> Seq.iter(
        fun e -> 
            let edg = e :?> DotEdge<string>
            qGraph.AddVertex(int edg.Source.Id) |> ignore
            qGraph.AddVertex(int edg.Destination.Id) |> ignore
            qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id,edg.Label)) |> ignore)
    qGraph

let loadLexerInputGraph gFile =
    let qGraph = loadDotToQG gFile
    let lexerInputG = new LexerInputGraph<_>()
    lexerInputG.StartVertex <- 0
    for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AEdge<_,_>(e.Source,e.Target,(Some e.Tag, Some e.Tag)))
    lexerInputG




let lbl tokenId = new AbstractParsing.Common.EdgeLabel<_,_>(tokenId,[||]) 

[<TestFixture>]
type ``RNGLR abstract parser tests`` () =
    let path name = System.IO.Path.Combine(baseInputGraphsPath,name)
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
    member this.``Simple calc. Sequence input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_,_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.AEdge<_,_>(0,1,lbl <| NUM 1)
             new AbstractParsing.Common.AEdge<_,_>(1,2,lbl <| PLUS 0)
             new AbstractParsing.Common.AEdge<_,_>(2,3,lbl <| NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.parserSource qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.AParser.Error (num, tok, message, debug) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
        | Yard.Generators.RNGLR.AParser.Success tree ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
        Assert.Pass()

    [<Test>]
    member this.``Calc. Sequence input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_,_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.AEdge<_,_>(0,1,lbl <| RNGLR.ParseCalc.NUMBER  1)
             new AbstractParsing.Common.AEdge<_,_>(1,2,lbl <| RNGLR.ParseCalc.PLUS 0)
             new AbstractParsing.Common.AEdge<_,_>(2,3,lbl <| RNGLR.ParseCalc.NUMBER 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.parserSource qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.AParser.Error (num, tok, message, debug) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
        | Yard.Generators.RNGLR.AParser.Success tree ->
            tree.PrintAst()
            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
        Assert.Pass()

//    [<Test>]
//    member this.``Simple calc. Sequence input. Full.`` () =
//        let lexerInputGraph = loadLexerInputGraph "test_8.dot"
//        let qGraph = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph 
//        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.parserSource qGraph
//        printfn "%A" r
//        match r with
//        | Yard.Generators.RNGLR.AParser.Error (num, tok, message, debug) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//        | Yard.Generators.RNGLR.AParser.Success tree ->
//            tree.PrintAst()
//            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
//        Assert.Pass()

    [<Test>]
    member this.``Simple calc. Branch binop input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_,_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.AEdge<_,_>(0,1,lbl <| NUM 1)
             new AbstractParsing.Common.AEdge<_,_>(1,2,lbl <| PLUS 0)
             new AbstractParsing.Common.AEdge<_,_>(1,2,lbl <| PLUS 3)
             new AbstractParsing.Common.AEdge<_,_>(2,3,lbl <| NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.parserSource qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.AParser.Error (num, tok, message, debug) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
        | Yard.Generators.RNGLR.AParser.Success tree ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
            //tree.Nodes |> Array.iteri (fun i x -> printfn "%2d: %A" i x)
            //printfn "%A" tree.Order
//            let args = {
//                tokenToRange = fun _ -> 0,0
//                zeroPosition = 0
//                clearAST = false
//                filterEpsilons = true
//            }
//
//
//            printfn "Result: %A" (RNGLR.ParseCalc.translate args tree)
//            tree.ChooseSingleAst()
//            tree.PrintAst()

        Assert.Pass()


    [<Test>]
    member this.``Simple calc. Branch binop and second arg.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_,_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.AEdge<_,_>(0,1,lbl <| NUM 1)
             new AbstractParsing.Common.AEdge<_,_>(1,2,lbl <| PLUS 0)
             new AbstractParsing.Common.AEdge<_,_>(1,3,lbl <| PLUS 3)
             new AbstractParsing.Common.AEdge<_,_>(2,4,lbl <| NUM 2)
             new AbstractParsing.Common.AEdge<_,_>(3,4,lbl <| NUM 4)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.parserSource qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.AParser.Error (num, tok, message, debug) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
        | Yard.Generators.RNGLR.AParser.Success tree ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
            
        Assert.Pass()

    [<Test>]
    member this.``Simple calc. Branch binop and first arg.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_,_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.AEdge<_,_>(0,1,lbl <| NUM 1)
             new AbstractParsing.Common.AEdge<_,_>(0,2,lbl <| NUM 2)
             new AbstractParsing.Common.AEdge<_,_>(1,3,lbl <| PLUS 3)
             new AbstractParsing.Common.AEdge<_,_>(2,3,lbl <| PLUS 4)
             new AbstractParsing.Common.AEdge<_,_>(3,4,lbl <| NUM 5)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.parserSource qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.AParser.Error (num, tok, message, debug) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
        | Yard.Generators.RNGLR.AParser.Success tree ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
            
        Assert.Pass()

    [<Test>]
    member this.``Simple calc with nterm. Seq input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_,_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.AEdge<_,_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 1)
             new AbstractParsing.Common.AEdge<_,_>(1,2,lbl <| RNGLR.SimpleCalcWithNTerm.PLUS 0)
             new AbstractParsing.Common.AEdge<_,_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerm.parserSource qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.AParser.Error (num, tok, message, debug) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
        | Yard.Generators.RNGLR.AParser.Success tree ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerm.defaultAstToDot tree "ast.dot"
            
        Assert.Pass()


    [<Test>]
    member this.``Simple calc with nterm. Branch binop and first arg.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_,_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.AEdge<_,_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 1)
             new AbstractParsing.Common.AEdge<_,_>(0,2,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 2)
             new AbstractParsing.Common.AEdge<_,_>(1,3,lbl <| RNGLR.SimpleCalcWithNTerm.PLUS 3)
             new AbstractParsing.Common.AEdge<_,_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerm.PLUS 4)
             new AbstractParsing.Common.AEdge<_,_>(3,4,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 5)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerm.parserSource qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.AParser.Error (num, tok, message, debug) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
        | Yard.Generators.RNGLR.AParser.Success tree ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerm.defaultAstToDot tree "ast.dot"
            
        Assert.Pass()

[<EntryPoint>]
let f x =
    let t = new ``RNGLR abstract parser tests`` () 
    //t.``Simple calc. Branch binop input.``  ()
    //t.``Calc. Sequence input.``()
    //t.``Simple calc. Branch binop and first arg.``()
    //t.``Simple calc. Branch binop and second arg.``()
    //t.``Simple calc with nterm. Seq input.``()
    t.``Simple calc. Sequence input.``()
    0