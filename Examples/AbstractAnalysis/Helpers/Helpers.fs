module Helpers

open Graphviz4Net.Dot.AntlrParser
open AbstractLexer.Common
open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Graphviz
open Graphviz4Net.Dot
open System.IO
open AbstractLexer.Core

let loadGraphFromDOT (filePath:string) = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let loadDotToQG path =
    let g = loadGraphFromDOT path
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
    for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new LexerEdge<_,_>(e.Source,e.Target,Some (e.Tag, e.Tag+"|")))
    lexerInputG

let printTokenizedGraph res printTag (fName:string) =
    let f = GraphvizAlgorithm(res)
    let printEdg (e:AbstractParsing.Common.ParserEdge<_>) =
        let printBrs brs =
            "["
            + (brs |> Array.map (fun (x:Position<_>) -> x.back_ref) |> String.concat "; ")
            + "]"
        printTag e.Tag printBrs 
    f.FormatEdge.Add(fun e -> (e.EdgeFormatter.Label.Value <- printEdg e.Edge))
    let str = f.Generate()
    let c = System.IO.Path.GetInvalidFileNameChars()
    let fName1 = c |> Array.fold (
                                    fun (name:string) ch -> name.Replace(ch,'_')) fName
    System.IO.File.WriteAllText(fName1 + ".lexer.dot" ,str)

