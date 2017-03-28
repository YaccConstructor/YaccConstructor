module YC.FST.AbstractLexing.Tests.CommonTestChecker

open System
open AbstractParser.Tokens
open YC.FST.AbstractLexing.Interpreter
open QuickGraph.FSA.GraphBasedFsa
open NUnit.Framework
open Microsoft.FSharp.Collections 
open QuickGraph 
open AbstractAnalysis.Common
open System.IO
open System.Collections.Generic
open YC.Utils.StructClass

let eof = RNGLR_EOF(new FSA<_>())

let printTok =
     fun x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])

let checkGraph (graph:AdjacencyGraph<_,_>) countE countV  =
    Assert.AreEqual(graph.EdgeCount, countE, "Count of edges not equal expected number. ")
    Assert.AreEqual(graph.VertexCount, countV, "Count of vertices not equal expected number. ")

let printSmbString (x:char*Position<_>) = 
        (fst x).ToString() + "_br: " + (snd x).BackRef + "(" + (snd x).StartOffset.ToString() + "," + (snd x).EndOffset.ToString() + ")"

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

let loadDotToQG path =
    let dot = File.ReadAllText(path)
    let vertexFunc = fun v attrs -> int v
    let edgeFunc = fun v1 v2 (attrs: IDictionary<string, string>) -> new TaggedEdge<_,_>(v1, v2, (attrs.Item("label"), attrs.Item("label")))
     
    BidirectionalGraph<_,_>.LoadDot(dot, new Func<_,_,_>(vertexFunc), new Func<_,_,_,_>(edgeFunc))

let approximateQG (graph: BidirectionalGraph<int,TaggedEdge<int,(string * 'br)>>) = 
    let fsa = new FSA<_>()
    fsa.InitState <- ResizeArray.singleton 0
    fsa.FinalState <- ResizeArray.singleton (Seq.max graph.Vertices)
        
    let counter = graph.Vertices |> Seq.max |> ref

    let splitEdge (edg: TaggedEdge<int,(string * 'br)>) str br =
        let start = edg.Source
        let _end = edg.Target
        let pos = ref 0

        match str with
        | Some("") -> [|new EdgeFSA<_>(start, _end, Eps)|]
        | None -> [|new EdgeFSA<_>(start, _end, Eps)|]
        | Some(s) ->
            let l = s.Length
            let ss = s.ToCharArray()
            Array.init l
                (fun i ->
                    match i with
                    | 0 when (l = 1)     -> new EdgeFSA<_>(start, _end, Smbl(ss.[i], new Position<'br>(0,1,br)))
                    | 0                  -> new EdgeFSA<_>(start, (incr counter; !counter), Smbl(ss.[i], new Position<_>(!pos,(incr pos; !pos),br)))
                    | i when (i = l - 1) -> new EdgeFSA<_>(!counter, _end, Smbl(ss.[i], new Position<_>(!pos,(incr pos; !pos),br)))
                    | i                  -> new EdgeFSA<_>(!counter, (incr counter; !counter), Smbl(ss.[i], new Position<_>(!pos,(incr pos; !pos),br)))
                )

    for edge in graph.Edges do
        match edge.Tag with
        | (str, br) ->
            let x = splitEdge edge (Some str) br
            x |> fsa.AddVerticesAndEdgeRange
            |> ignore
        
    fsa

let checkArr expectedArr actualArr =
    if Array.length expectedArr = Array.length actualArr
    then 
        Array.iteri2 (
            fun i x1 x2 -> 
            if x1 <> x2 then
                printfn "%A %A" expectedArr actualArr
                Assert.Fail ("Arrays differ at position: " + string i)) expectedArr actualArr
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
                sprintf "%i -> %i [label=\"%s\"]; \n" edge.Source edge.Target (toStr edge.Tag)) 
                                      
    System.IO.File.WriteAllText(filePrintPath, s + (String.concat "" strs) + "\n}")
    ()

