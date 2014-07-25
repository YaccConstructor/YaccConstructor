module YC.Tests.Helper

open NUnit.Framework
open System.Linq
open System.IO
open QuickGraph
open Graphviz4Net.Dot.AntlrParser
open Graphviz4Net.Dot

let path baseInputGraphsPath name = System.IO.Path.Combine(baseInputGraphsPath,name)

let filesAreEqual file1 file2 =
    let all1 = File.ReadAllBytes file1
    let all2 = File.ReadAllBytes file2
    Assert.AreEqual (all1.Length, all2.Length)
    Assert.IsTrue(Array.forall2 (=) all1 all2)

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let loadDotToQG baseInputGraphsPath gFile =
    let g = loadGraphFromDOT(path baseInputGraphsPath gFile)
    let qGraph = new AdjacencyGraph<int, TaggedEdge<_,string>>()
    g.Edges 
    |> Seq.iter(
        fun e -> 
            let edg = e :?> DotEdge<string>
            qGraph.AddVertex(int edg.Source.Id) |> ignore
            qGraph.AddVertex(int edg.Destination.Id) |> ignore
            qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id,edg.Label)) |> ignore)
    qGraph