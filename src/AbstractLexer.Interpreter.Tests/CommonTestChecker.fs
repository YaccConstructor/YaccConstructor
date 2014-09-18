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

let eof = RNGLR_EOF("", [||])

let printTok =
     fun x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])

let printBref =
    let printBrs brs =
        "["
        + (brs |> Array.map (fun (pos:Position<_>) -> pos.back_ref ) |> String.concat "; ")
        + "]"        
        + (brs |> Array.map (fun (pos:Position<_>) -> "(" + pos.start_offset.ToString() + ", " + pos.end_offset.ToString() + ")") |> String.concat "; ")         

    fun x ->
        match x with
            | NUMBER(v,br) -> "NUM: " + v + printBrs br
            | MINUS(v,br) -> "-: " + printBrs br
            | LBRACE(v,br) -> "(: " + printBrs br
            | RBRACE(v,br) -> "): " + printBrs br
            | DIV(v,br) -> "/: " + printBrs br
            | PLUS(v,br) -> "+: " + printBrs br
            | POW(v,br)  -> "**: " + printBrs br
            | MULT(v,br) -> "*: " + printBrs br
            | LITERAL(v,br) -> "LITERAL: " + v + printBrs br 
            | x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0]) 
   
let checkGraph (graph:AdjacencyGraph<_,_>) countE countV  =
    Assert.AreEqual(graph.EdgeCount, countE, "Count of edges not equal expected number. ")
    Assert.AreEqual(graph.VertexCount, countV, "Count of vertices not equal expected number. ")

let checkArr expectedArr actualArr =
    if Array.length expectedArr = Array.length actualArr
    then 
        Array.iteri2 (
            fun i x1 x2 -> 
            if x1 <> x2 then Assert.Fail ("Arrays differ at position: " + string i)) expectedArr actualArr
        Assert.Pass()
    else Assert.Fail ("Arrays have different length")
 
let positions (parserInputGraph : ParserInputGraph<_>) getVal =
    parserInputGraph.Edges 
        |> Seq.collect
            (fun e -> 
                match e.Tag with
                    | NUMBER(v,br) 
                    | MINUS(v,br) 
                    | LBRACE(v,br) 
                    | RBRACE(v,br) 
                    | DIV(v,br) 
                    | PLUS(v,br) 
                    | POW(v,br)  
                    | MULT(v,br) 
                    | LITERAL(v,br) ->
                        br |> Array.map getVal
                    | RNGLR_EOF _ -> [||])
        |> Array.ofSeq

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