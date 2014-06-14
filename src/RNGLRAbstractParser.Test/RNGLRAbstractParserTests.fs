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
open AbstractParsing.Common
open RNGLR.ParseSimpleCalc
open RNGLR.PrettySimpleCalc
open Yard.Generators.RNGLR.AbstractParser
open AbstractLexer.Common

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../../../Tests/AbstractRNGLR/DOT"

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

//let loadLexerInputGraph gFile =
//    let qGraph = loadDotToQG gFile
//    let lexerInputG = new LexerInputGraph<_>()
//    lexerInputG.StartVertex <- 0
//    for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new ParserEdge<_>(e.Source,e.Target,(Some e.Tag, Some e.Tag)))
//    lexerInputG




let lbl tokenId = tokenId

[<TestFixture>]
type ``RNGLR abstract parser tests`` () =
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
        let lexerInputG = new AbstractLexer.Common.LexerInputGraph<_>()
        lexerInputG.StartVertex <- 0
        for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AbstractLexer.Common.LexerEdge<_,_>(e.Source,e.Target,Some (e.Tag, e.Tag)))
        lexerInputG


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
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.ParseSimpleCalc.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.ParseSimpleCalc.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.ParseSimpleCalc.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass()

    [<Test>]
    member this.``Calc. Sequence input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.ParseCalc.NUMBER  1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.ParseCalc.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.ParseCalc.NUMBER 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass()

    [<Test>]
    member this.``Calc. Branched input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.ParseCalc.NUMBER  1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.ParseCalc.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.ParseCalc.NUMBER 2)
             new AbstractParsing.Common.ParserEdge<_>(3,4,lbl <| RNGLR.ParseCalc.MULT 3)
             new AbstractParsing.Common.ParserEdge<_>(4,5,lbl <| RNGLR.ParseCalc.NUMBER 4)
             new AbstractParsing.Common.ParserEdge<_>(3,6,lbl <| RNGLR.ParseCalc.DIV 5)
             new AbstractParsing.Common.ParserEdge<_>(6,5,lbl <| RNGLR.ParseCalc.NUMBER 6)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass()

    [<Test>]
    member this.``Calc. Branched input error.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVerticesAndEdgeRange
            [
             new AbstractParsing.Common.ParserEdge<_>(0,3,lbl <| RNGLR.ParseCalc.NUMBER 2)
             new AbstractParsing.Common.ParserEdge<_>(3,4,lbl <| RNGLR.ParseCalc.MULT 3)
             new AbstractParsing.Common.ParserEdge<_>(4,5,lbl <| RNGLR.ParseCalc.NUMBER 4)
             new AbstractParsing.Common.ParserEdge<_>(3,6,lbl <| RNGLR.ParseCalc.DIV 5)
             new AbstractParsing.Common.ParserEdge<_>(6,5,lbl <| RNGLR.ParseCalc.PLUS 6)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Pass()
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
            Assert.Fail "!!!!"
    
    [<Test>]
    member this.``Pretty Simple Calc. Error Handling Temp.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVerticesAndEdgeRange
            [
             new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.PrettySimpleCalc.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.PrettySimpleCalc.PLUS 2)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.PrettySimpleCalc.NUM 3)
             new AbstractParsing.Common.ParserEdge<_>(3,4,lbl <| RNGLR.PrettySimpleCalc.PLUS 4)
             new AbstractParsing.Common.ParserEdge<_>(4,5,lbl <| RNGLR.PrettySimpleCalc.NUM 5)
             new AbstractParsing.Common.ParserEdge<_>(5,6,lbl <| RNGLR.PrettySimpleCalc.RNGLR_EOF 6)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.PrettySimpleCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.PrettySimpleCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass() 

    [<Test>]
    member this.``Pretty Simple Calc. Error Is Not Handled Without EOF.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVerticesAndEdgeRange
            [
             new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.PrettySimpleCalc.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.PrettySimpleCalc.PLUS 2)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.PrettySimpleCalc.NUM 3)
             new AbstractParsing.Common.ParserEdge<_>(1,3,lbl <| RNGLR.PrettySimpleCalc.PLUS 4)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.PrettySimpleCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.PrettySimpleCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass() 

    [<Test>]
    member this.``Pretty Simple Calc. Error Is Handled With EOF.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVerticesAndEdgeRange
            [
             new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.PrettySimpleCalc.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.PrettySimpleCalc.PLUS 2)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.PrettySimpleCalc.NUM 3)
             new AbstractParsing.Common.ParserEdge<_>(1,3,lbl <| RNGLR.PrettySimpleCalc.PLUS 4)
             new AbstractParsing.Common.ParserEdge<_>(1,3,lbl <| RNGLR.PrettySimpleCalc.RNGLR_EOF 5)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.PrettySimpleCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Pass()
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.PrettySimpleCalc.defaultAstToDot tree "ast.dot"
            Assert.Fail "!!!!" 

    [<Test>]
    member this.``Calc. Branched input 2.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.ParseCalc.NUMBER  1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.ParseCalc.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.ParseCalc.NUMBER 2)
             new AbstractParsing.Common.ParserEdge<_>(3,4,lbl <| RNGLR.ParseCalc.MULT 3)
             new AbstractParsing.Common.ParserEdge<_>(4,5,lbl <| RNGLR.ParseCalc.NUMBER 4)
             new AbstractParsing.Common.ParserEdge<_>(3,6,lbl <| RNGLR.ParseCalc.MINUS 5)
             new AbstractParsing.Common.ParserEdge<_>(6,5,lbl <| RNGLR.ParseCalc.NUMBER 6)
             new AbstractParsing.Common.ParserEdge<_>(5,7,lbl <| RNGLR.ParseCalc.MULT 3)
             new AbstractParsing.Common.ParserEdge<_>(7,8,lbl <| RNGLR.ParseCalc.NUMBER 4)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass()

//    [<Test>]
//    member this.``Simple calc. Sequence input. Full.`` () =
//        let lexerInputGraph = loadLexerInputGraph "test_8.dot"
//        let qGraph = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph 
//        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
//        printfn "%A" r
//        match r with
//        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
//            printfn "Error in position %d on Token %A: %s" num tok message
//            debug.drawGSSDot "out.dot"
//        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
//            tree.PrintAst()
//            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
//        Assert.Pass()

    [<Test>]
    member this.``Simple calc. Branch binop input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.ParseSimpleCalc.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.ParseSimpleCalc.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.ParseSimpleCalc.PLUS 3)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.ParseSimpleCalc.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass()

    [<Test>]
    member this.``Lexer and parser`` () =
        let lexerInputGraph = loadLexerInputGraph "lexer_and_parser_simple_test.dot"
        let qGraph = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, RNGLR.ParseCalc.RNGLR_EOF 0)

        let r = (new Parser<_>()).Parse  RNGLR.ParseCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass()


    [<Test>]
    member this.``Simple calc. Branch binop and second arg.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.ParseSimpleCalc.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.ParseSimpleCalc.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(1,3,lbl <| RNGLR.ParseSimpleCalc.PLUS 3)
             new AbstractParsing.Common.ParserEdge<_>(2,4,lbl <| RNGLR.ParseSimpleCalc.NUM 2)
             new AbstractParsing.Common.ParserEdge<_>(3,4,lbl <| RNGLR.ParseSimpleCalc.NUM 4)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass()

    [<Test>]
    member this.``Simple calc. Branch binop and first arg.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.ParseSimpleCalc.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(0,2,lbl <| RNGLR.ParseSimpleCalc.NUM 2)
             new AbstractParsing.Common.ParserEdge<_>(1,3,lbl <| RNGLR.ParseSimpleCalc.PLUS 3)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.ParseSimpleCalc.PLUS 4)
             new AbstractParsing.Common.ParserEdge<_>(3,4,lbl <| RNGLR.ParseSimpleCalc.NUM 5)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalc.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalc.defaultAstToDot tree "ast.dot"
            Assert.Pass()

    [<Test>]
    member this.``Simple calc with nterm. Seq input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.SimpleCalcWithNTerm.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerm.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerm.defaultAstToDot tree "ast.dot"
            Assert.Pass()


    [<Test>]
    member this.``Simple calc with nterm. Branch binop and first arg.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(0,2,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 2)
             new AbstractParsing.Common.ParserEdge<_>(1,3,lbl <| RNGLR.SimpleCalcWithNTerm.PLUS 3)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerm.PLUS 4)
             new AbstractParsing.Common.ParserEdge<_>(3,4,lbl <| RNGLR.SimpleCalcWithNTerm.NUM 5)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerm.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerm.defaultAstToDot tree "ast.dot"            
            Assert.Pass()

    [<Test>]
    member this.``Simple calc with nterm 2. Seq input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.SimpleCalcWithNTerms_2.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_2.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerms_2.defaultAstToDot tree "ast.dot"
            Assert.Pass()


    [<Test>]
    member this.``Simple calc with nterm 2. Brabch first operand.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 3)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.SimpleCalcWithNTerms_2.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_2.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerms_2.defaultAstToDot tree "ast.dot"
            Assert.Pass()

    [<Test>]
    member this.``Simple calc with nterm 2. Fully brabched.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(0,4,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 3)
             new AbstractParsing.Common.ParserEdge<_>(4,5,lbl <| RNGLR.SimpleCalcWithNTerms_2.PLUS 4)
             new AbstractParsing.Common.ParserEdge<_>(5,3,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 5)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.SimpleCalcWithNTerms_2.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerms_2.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_2.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerms_2.defaultAstToDot tree "ast.dot"        
            Assert.Pass()

    [<Test>]
    member this.``Simple calc with nterm 3. Seq input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerms_3.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.SimpleCalcWithNTerms_3.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerms_3.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_3.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerms_3.defaultAstToDot tree "ast.dot"
            Assert.Pass()

    [<Test>]
    member this.``Calc demo`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        //qGraph.AddVertexRange[0;1;2;3] |> ignore
        let n = RNGLR.ParseSimpleCalcDemo.NUM 
        let p = RNGLR.ParseSimpleCalcDemo.PLUS 
        let m = RNGLR.ParseSimpleCalcDemo.MULT
        let l = RNGLR.ParseSimpleCalcDemo.LBR
        let r = RNGLR.ParseSimpleCalcDemo.RBR
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| n 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| p 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| n 4)
             new AbstractParsing.Common.ParserEdge<_>(3,8,lbl <| m 4)
             new AbstractParsing.Common.ParserEdge<_>(1,4,lbl <| m 5)
             new AbstractParsing.Common.ParserEdge<_>(4,5,lbl <| n 5)
             new AbstractParsing.Common.ParserEdge<_>(5,8,lbl <| p 1)
             new AbstractParsing.Common.ParserEdge<_>(1,6,lbl <| p 2)
             new AbstractParsing.Common.ParserEdge<_>(6,7,lbl <| n 6)
             new AbstractParsing.Common.ParserEdge<_>(7,8,lbl <| p 3)
             new AbstractParsing.Common.ParserEdge<_>(8,9,lbl <| l 0)
             new AbstractParsing.Common.ParserEdge<_>(9,10,lbl <| n 7)
             new AbstractParsing.Common.ParserEdge<_>(10,11,lbl <| m 6)
             new AbstractParsing.Common.ParserEdge<_>(11,12,lbl <| n 8)
             new AbstractParsing.Common.ParserEdge<_>(12,13,lbl <| r 1)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.ParseSimpleCalcDemo.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.ParseSimpleCalcDemo.defaultAstToDot tree "ast.dot"
            Assert.Pass()


    [<Test>]
    member this.``Simple calc with nterm 4. Seq input.`` () =
        let qGraph = new AbstractParsing.Common.ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [new AbstractParsing.Common.ParserEdge<_>(0,1,lbl <| RNGLR.SimpleCalcWithNTerms_4.NUM 1)
             new AbstractParsing.Common.ParserEdge<_>(1,2,lbl <| RNGLR.SimpleCalcWithNTerms_4.PLUS 0)
             new AbstractParsing.Common.ParserEdge<_>(2,3,lbl <| RNGLR.SimpleCalcWithNTerms_4.NUM 2)
             ] |> ignore

        let r = (new Parser<_>()).Parse  RNGLR.SimpleCalcWithNTerms_4.buildAstAbstract qGraph
        printfn "%A" r
        match r with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!!!"
        | Yard.Generators.RNGLR.Parser.Success(tree, _) ->
            tree.PrintAst()
            RNGLR.SimpleCalcWithNTerms_4.defaultAstToDot tree "ast.dot"
            Assert.Pass()


[<EntryPoint>]
let f x =
    if System.IO.Directory.Exists "dot" 
    then 
        System.IO.Directory.GetFiles "dot" |> Seq.iter System.IO.File.Delete
    else System.IO.Directory.CreateDirectory "dot" |> ignore
    let t = new ``RNGLR abstract parser tests`` () 
    //t.``Simple calc. Branch binop input.``  ()
    //t.``Calc. Sequence input.``()
    //t.``Calc. Branched input error.``()
    t.``Calc demo``()
    //t.``Simple calc. Branch binop and first arg.``()
    //t.``Simple calc. Branch binop and second arg.``()
    //t.``Simple calc with nterm. Seq input.``()
    //t.``Simple calc with nterm 2. Seq input.``()
    //t.``Simple calc with nterm 3. Seq input.``()
    //t.``Simple calc with nterm 4. Seq input.``()
    //t.``Simple calc. Sequence input.``()
    //t.``Simple calc with nterm 2. Brabch first operand.``()
    //t.``Simple calc with nterm 2. Fully brabched.``()
    0
