module AbstractFsLex.Test

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractLexer.Common

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

    [<Test>]
    member this.``Load graph test from DOT`` () =
        let g = loadGraphFromDOT(path "test_1.dot")
        Assert.AreEqual(g.Edges |> Seq.length, 5)
        Assert.AreEqual(g.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Load graph test from DOT to QuickGraph`` () =
        let qGraph = loadDotToQG "test_1.dot"
        Assert.AreEqual(qGraph.Edges |> Seq.length, 5)
        Assert.AreEqual(qGraph.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Load graph test from DOT to lexer input graph`` () =
        let qGraph = loadDotToQG "test_1.dot"
        let lexerInputG = new LexerInputGraph<_>()
        lexerInputG.StartVertex <- 0
        for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AEdge<_,_>(e.Source,e.Target,(Some e.Tag, Some e.Tag)))
        Assert.AreEqual(lexerInputG.Edges |> Seq.length, 5)
        Assert.AreEqual(lexerInputG.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Load graph test from DOT to lexer inner graph`` () =
        let qGraph = loadDotToQG "test_1.dot"
        let lexerInputG = new LexerInputGraph<_>()
        lexerInputG.StartVertex <- 0
        for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AEdge<_,_>(e.Source,e.Target,(Some e.Tag, Some e.Tag)))
        let lexerInnerGraph = new LexerInnerGraph<_>(lexerInputG)
        Assert.AreEqual(lexerInnerGraph.StartVertex, 0)
        Assert.AreEqual(lexerInnerGraph.Edges |> Seq.length, 7)
        Assert.AreEqual(lexerInnerGraph.Vertices |> Seq.length, 7)

//[<EntryPoint>]
//let f x =
//    let t = new ``Abstract lexer tests`` () 
//    t.``Load graph test from DOT to lexer inner graph``()
//    1