module YC.FST.AbstractLexing.Tests.CommonTestChecker

open AbstractParser.Tokens
open YC.FST.AbstractLexing.Interpreter
open NUnit.Framework
open Microsoft.FSharp.Collections 
open QuickGraph 
open AbstractAnalysis.Common
open Graphviz4Net.Dot.AntlrParser
open Graphviz4Net.Dot
open YC.FST.FstApproximation
open System.IO

let eof = RNGLR_EOF(new GraphTokenValue<_>())   

let printTok =
     fun x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])

let checkGraph (graph:AdjacencyGraph<_,_>) countE countV  =
    Assert.AreEqual(graph.EdgeCount, countE, "Count of edges not equal expected number. ")
    Assert.AreEqual(graph.VertexCount, countV, "Count of vertices not equal expected number. ")

let printSmbString (x:char*Position<_>) = 
        (fst x).ToString() + "_br: " + (snd x).back_ref + "(" + (snd x).start_offset.ToString() + "," + (snd x).end_offset.ToString() + ")"

let printBref =       
    let printGr (gr:GraphTokenValue<_>) = 
        let strs = ref ""
        for edge in gr.Edges do
            strs := !strs + "[" + edge.Source.ToString() + ", " + 
                                  "{" + edge.Label.ToString() + "_br: " + edge.BackRef + "(" + edge.StartPos.ToString() + ", " + edge.EndPos.ToString() + ")"+ "}" +
                                  edge.Target.ToString() + "] ;"
        !strs        
             
    fun x ->
        match x with
            | NUMBER(gr) -> "NUM: " + printGr gr
            | MINUS(gr) -> "MINUS: " + printGr gr
            | LBRACE(gr) -> "LBRACE: " + printGr gr
            | RBRACE(gr) -> "RBRACE: " + printGr gr
            | DIV(gr) -> "DIV: " + printGr gr
            | PLUS(gr) -> "PLUS: "  + printGr gr
            | POW(gr)  -> "POW: "  + printGr gr
            | MULT(gr) -> "MULT: " + printGr gr
            | LITERAL(gr) -> "LITERAL: " + printGr gr
            | x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])  


let path baseInputGraphsPath name = System.IO.Path.Combine(baseInputGraphsPath,name)

let loadGraphFromDOT filePath =
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let loadDotToQG baseInputGraphsPath gFile =
    let qGraph = loadGraphFromDOT(path baseInputGraphsPath gFile)
    let graphAppr = new Appr<_>()
    graphAppr.InitState <- ResizeArray.singleton 0

    for e in qGraph.Edges do
        let edg = e :?> DotEdge<string>
        new TaggedEdge<_,_>(int edg.Source.Id, int edg.Destination.Id, (Smb(edg.Label, edg.Label))) |> graphAppr.AddVerticesAndEdge |> ignore

    graphAppr.FinalState <- ResizeArray.singleton (Seq.max graphAppr.Vertices)
    graphAppr

