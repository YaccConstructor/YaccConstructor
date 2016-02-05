module YC.FST.AbstractLexing.Tests.CommonTestChecker

open AbstractParser.Tokens
open YC.FST.AbstractLexing.Interpreter
open QuickGraph.FSA.FsaApproximation
open QuickGraph.FSA.GraphBasedFsa
open NUnit.Framework
open Microsoft.FSharp.Collections 
open QuickGraph 
open AbstractAnalysis.Common
open Graphviz4Net.Dot.AntlrParser
open Graphviz4Net.Dot
open System.IO

let eof = RNGLR_EOF(new FSA<_>())   

let printTok =
     fun x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])

let checkGraph (graph:AdjacencyGraph<_,_>) countE countV  =
    Assert.AreEqual(graph.EdgeCount, countE, "Count of edges not equal expected number. ")
    Assert.AreEqual(graph.VertexCount, countV, "Count of vertices not equal expected number. ")

let printSmbString (x:char*Position<_>) = 
        (fst x).ToString() + "_br: " + (snd x).back_ref + "(" + (snd x).start_offset.ToString() + "," + (snd x).end_offset.ToString() + ")"

let printBref printSmbString =       
    let printGr (gr:FSA<_>) = 
        let strs = ref ""
        for edge in gr.Edges do
            strs := !strs + "[" + edge.Source.ToString() + ", " + 
                                  "{" + (match edge.Tag with |Smbl x -> printSmbString x |_ -> "") + "}" + edge.Target.ToString() + "] ;"
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
        new TaggedEdge<_,_>(int edg.Source.Id, int edg.Destination.Id, (edg.Label, edg.Label)) |> graphAppr.AddVerticesAndEdge |> ignore

    graphAppr.FinalState <- ResizeArray.singleton (Seq.max graphAppr.Vertices)
    graphAppr

let checkArr expectedArr actualArr =
    if Array.length expectedArr = Array.length actualArr
    then 
        Array.iteri2 (
            fun i x1 x2 -> 
            if x1 <> x2 then Assert.Fail ("Arrays differ at position: " + string i)) expectedArr actualArr
        Assert.Pass()
    else Assert.Fail ("Arrays have different length")
 
let countEdges (parserInputGraph : ParserInputGraph<_>) =
   parserInputGraph.Edges 
    |> Seq.map (fun e -> 
                    match e.Tag with
                        | NUMBER(gr) 
                        | MINUS(gr) 
                        | LBRACE(gr) 
                        | RBRACE(gr) 
                        | DIV(gr) 
                        | PLUS(gr) 
                        | POW(gr)  
                        | MULT(gr) 
                        | LITERAL(gr) -> gr.EdgeCount
                        | RNGLR_EOF _ -> 0) 
    |> Array.ofSeq 

let ToDot (parserInputGraph : ParserInputGraph<_>) filePrintPath toStr =
    let rank s l =
        "{ rank=" + s + "; " + (l |> string) + " }\n"
    let s = 
        "digraph G {\n" 
        + "rankdir = LR\n"
        + "node [shape = circle]\n"
        + sprintf "%i[style=filled, fillcolor=green]\n" parserInputGraph.InitStates.[0]
        + sprintf "%i[shape = doublecircle, style=filled, fillcolor=red]\n" parserInputGraph.FinalStates.[0]
        + rank "same" parserInputGraph.InitStates.[0]
        + rank "min" parserInputGraph.InitStates.[0]
        + rank "same" parserInputGraph.FinalStates.[0] 
        + rank "max" parserInputGraph.FinalStates.[0]
    
    let strs =
            parserInputGraph.Edges
            |> Seq.map (fun edge ->
                sprintf "%i -> %i [label=\"%s\"]; \n" edge.Source edge.Target  (toStr edge.Tag)) 
                                      
    System.IO.File.WriteAllText(filePrintPath, s + (String.concat "" strs) + "\n}")
    ()

