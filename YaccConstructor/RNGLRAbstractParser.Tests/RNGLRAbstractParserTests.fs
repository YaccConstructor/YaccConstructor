// Learn more about F# at http://fsharp.net

module RNGLRAbstractParserTests

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph


let graph filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let g = graph @"D:\projects\string-embedded-languages-analysis-platform\Tests\Materials\DOTGraphs\str\_NumSeq.dot"
let l = g.Edges |> Seq.map(fun e -> (e :?> DotEdge<string>).Label)

let qGraph = new AdjacencyGraph<int, TaggedEdge<_,string>>()

g.Edges |> Seq.iter(fun e -> let edg = e :?> DotEdge<string> in qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id,edg.Label)) |> ignore)

printfn "%A" l

let f = 56
printfn "12431423"