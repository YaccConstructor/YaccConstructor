module Helpers

open Graphviz4Net.Dot.AntlrParser
open AbstractLexer.Common
open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Graphviz
open Graphviz4Net.Dot
open System.IO

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
