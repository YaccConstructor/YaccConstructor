module AbstractFsLex.Test

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractLexer.Common
open AbstractLexer.Core
open QuickGraph.Algorithms

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../../../../Tests/AbstractLexing/DOT"

[<TestFixture>]
type ``Abstract lexer tests`` () =    

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

    [<Test>]
    member this.``Load graph test from DOT`` () =
        let g = loadGraphFromDOT(path "test_00.dot")
        Assert.AreEqual(g.Edges |> Seq.length, 4)
        Assert.AreEqual(g.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Load graph test from DOT to QuickGraph`` () =
        let qGraph = loadDotToQG "test_00.dot"
        Assert.AreEqual(qGraph.Edges |> Seq.length, 4)
        Assert.AreEqual(qGraph.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Load graph test from DOT to lexer input graph`` () =
        let qGraph = loadDotToQG "test_00.dot"
        let lexerInputG = new LexerInputGraph<_>()
        lexerInputG.StartVertex <- 0
        for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AEdge<_,_>(e.Source,e.Target,(Some e.Tag, Some e.Tag)))
        Assert.AreEqual(lexerInputG.Edges |> Seq.length, 4)
        Assert.AreEqual(lexerInputG.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Load graph test from DOT to lexer inner graph`` () =
        let lexerInnerGraph = new LexerInnerGraph<_>(loadLexerInputGraph "test_00.dot")
        Assert.AreEqual(lexerInnerGraph.StartVertex, 0)
        Assert.AreEqual(lexerInnerGraph.Edges |> Seq.length, 6)
        Assert.AreEqual(lexerInnerGraph.Vertices |> Seq.length, 6)

    [<Test>]
    member this.``Calc. Simple number.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_0.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
        Assert.AreEqual(res.Edges |> Seq.length, 1)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

    [<Test>]
    member this.``Calc. Simple sum.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Start from PLUS.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Calc. Two-digit numbers sum.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)
//
//    [<Test>]
//    member this.``Calc. Branched multy-digit numbers.`` () =
//        let lexerInputGraph = loadLexerInputGraph "test_4_1.dot"
//        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
//        Assert.AreEqual(res.Edges |> Seq.length, 2)
//        Assert.AreEqual(res.Vertices |> Seq.length, 2)
//
//    [<Test>]
//    member this.``Calc. Branched multy-digit numbers with Binop.`` () =
//        let lexerInputGraph = loadLexerInputGraph "test_4_2.dot"
//        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
//        Assert.AreEqual(res.Edges |> Seq.length, 3)
//        Assert.AreEqual(res.Vertices |> Seq.length, 3)
//
//    [<Test>]
//    member this.``Calc. Branched multy-digit numbers sum 1.`` () =
//        let lexerInputGraph = loadLexerInputGraph "test_4_3.dot"
//        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
//        Assert.AreEqual(res.Edges |> Seq.length, 4)
//        Assert.AreEqual(res.Vertices |> Seq.length, 4)
//
//    [<Test>]
//    member this.``Calc. Branched multy-digit numbers sum 2.`` () =
//        let lexerInputGraph = loadLexerInputGraph "test_4_4.dot"
//        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
//        Assert.AreEqual(res.Edges |> Seq.length, 5)
//        Assert.AreEqual(res.Vertices |> Seq.length, 4)
//
//    [<Test>]
//    member this.``Calc. Branched binop sum.`` () =
//        let lexerInputGraph = loadLexerInputGraph "test_5.dot"
//        let res = Calc.Lexer._fslex_tables.Tokenize Calc.Lexer.fslex_actions_token lexerInputGraph
//        Assert.AreEqual(res.Edges |> Seq.length, 4)
//        Assert.AreEqual(res.Vertices |> Seq.length, 4)



//[<EntryPoint>]
//let f x =
//    let t = new ``Abstract lexer tests`` () 
//    t.``Calc. Branched multy-digit numbers sum 2.``()
//    1