module Yard.Generators.RNGLR.ReadBack.Printers

open Yard.Generators.RNGLR.ReadBack.Graphs
open Yard.Generators.Common
open Yard.Generators.RNGLR.ReadBack
open System.IO
open System.Collections.Generic
open FSharpx.Collections.Experimental

let gssToDot (tokenToNumber : _ -> int) (tokens : BlockResizeArray<_>) (leftSide : int[])
        (initNodes : seq<GssVertex>) (numToString : int -> string) (errInd: int) (path : string) =
    use out = new System.IO.StreamWriter (path)
    let was = new Dictionary<_,_>()
    let levels = new Dictionary<_,_>()
    out.WriteLine "digraph GSS {"
    let print s = out.WriteLine ("    " + s)
    let curNum = ref 0
    print "rankdir=RL"
    let getAstString (ast : SppfLabel) =
        match ast with
        | SppfLabel.Terminal t -> tokens.[t] |> tokenToNumber |> numToString    
        | SppfLabel.Reduction (prod, _) 
        | EpsilonReduction prod -> leftSide.[prod] |> numToString
        | _ -> failwith "Unexpected ast"

    let rec dfs (u : GssVertex) =
        was.Add (u, !curNum)
        if not <| levels.ContainsKey u.Level then
            levels.[u.Level] <- [!curNum]
        else
            levels.[u.Level] <- !curNum :: levels.[u.Level]
        print <| sprintf "%d [label=\"%d\"]" !curNum u.State
        incr curNum
        if u.firstOutEdge.IsSome then
            handleEdge u u.firstOutEdge.Value
            if u.otherOutEdges <> null then
                u.otherOutEdges |> Array.iter (handleEdge u)

    and handleEdge u (e : GssEdge) =
        let v = e.Dest
        if not <| was.ContainsKey v then
            dfs v
        print <| sprintf "%d -> %d [label=\"%s\"]" was.[u] was.[v] (getAstString e.Label)

    for v in initNodes do
        if not <| was.ContainsKey v then
            dfs v
    
    for level in levels do
        print <| sprintf "{rank=same; %s}" (level.Value |> List.map (fun (u : int) -> string u) |> String.concat " ")

    out.WriteLine "}"
    out.Close()

let sppfToDot<'TokenType> (tokens : 'TokenType[]) (sppf : Sppf) 
                    (leftSide : int[]) (tokenToNumber : 'TokenType -> int) (numberToString : int -> string) 
                    (file : string) =
    
    let nextIndex = 
        let cur = ref -1
        fun () -> 
            incr cur
            !cur
    
    use out = new StreamWriter(file)
    out.WriteLine("digraph SPPF{")
    out.WriteLine("    rankdir=LR")

    let printVertex num isStart isAccepting =
        let str = ref <| sprintf "    %d [label = \"\", shape = circle" num
        if isStart then
            str := !str + ", style = filled"
        if isAccepting then
            str := !str + ", peripheries = 2"
        str := !str + "]"
        out.WriteLine(!str)
    
    let printEdge source dest label = 
        let str = sprintf "    %d -> %d [label = \"%s\"]" source dest label
        out.WriteLine(str)

    let reductionToDot (sppf : Sppf) =
        let startVertex, numberOfNfaSates, endLevel, acceptingNfaStates = sppf
        let dict = new SppfSearchDictionary<int>(numberOfNfaSates)
        let searchStack = new Stack<int * SppfLabel * SppfVertex>(10)
        
        let isAccepting (vertex : SppfVertex) =
            let nfaVertex, gssVertex = vertex.nfaVertex, vertex.gssVertex
            if gssVertex.Level <> endLevel then false
            else
               Set.contains nfaVertex.label acceptingNfaStates

        let searchStep sourceIndex (edgeLabel : SppfLabel) (dest : SppfVertex)=
            let destIndex =
                let nfaVertex, gssVertex = dest.nfaVertex, dest.gssVertex
                match dict.TryGet nfaVertex.label gssVertex.Level gssVertex.State with
                | Some x -> x
                | None -> 
                    let dI = nextIndex()
                    dict.Add dest dI
                    printVertex dI false (isAccepting dest)
                    for edge in dest.outEdges do
                        searchStack.Push (dI, edge.Label, edge.Dest)
                    dI
            
            let edgeLabel =
                match edgeLabel with
                | SppfLabel.Terminal tokenId -> tokens.[tokenId] |> tokenToNumber |> numberToString
                | SppfLabel.Reduction (prod, child) -> leftSide.[prod] |> numberToString
                | SppfLabel.EpsilonReduction prod -> leftSide.[prod] |> numberToString
                | SppfLabel.Epsilon -> "eps"
                | _ -> ""

            printEdge sourceIndex destIndex edgeLabel
        
        let startIndex = nextIndex()
        dict.Add startVertex startIndex            
        printVertex startIndex true <| isAccepting startVertex
        
        for edge in startVertex.outEdges do
            searchStack.Push (startIndex, edge.Label, edge.Dest)

        while searchStack.Count > 0 do
            let sourceIndex, edgeLabel, dest = searchStack.Pop()
            searchStep sourceIndex edgeLabel dest
            

    reductionToDot sppf

    out.WriteLine("}")

let nfaToDot (vertices : VertexWithBackTrack<int, int>[]) numToString epsilonIndex (file : string) =
    use out = new StreamWriter(file)
    out.WriteLine("digraph SPPF{")
    out.WriteLine("    rankdir=LR")
    for i = 0 to vertices.Length - 1 do
        let vertex = vertices.[i]
        for edge in vertex.outEdges do
            let label =
                match edge.label with
                | n  when n = epsilonIndex -> "eps"
                | _ -> numToString edge.label
            out.WriteLine(sprintf "    %d -> %d [label = \"%s\"]" vertex.label edge.dest.label label)
    out.WriteLine("}")