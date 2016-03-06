module Yard.Generators.RNGLR.ReadBack.Printers

open Yard.Generators.RNGLR.ReadBack.Graphs
open Yard.Generators.Common
open Yard.Generators.RNGLR.ReadBack
open System.IO
open System.Collections.Generic


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
        let searchStack = new Stack<int * SppfLabel * Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>>(10)
        
        let isAccepting (vertex : Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>) =
            let nfaVertex, gssVertex = vertex.label
            if gssVertex.Level <> endLevel then false
            else
               Set.contains nfaVertex.label acceptingNfaStates

        let searchStep sourceIndex (edgeLabel : SppfLabel) (dest : Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>)=
            let destIndex =
                let nfaVertex, gssVertex = dest.label
                match dict.TryGet nfaVertex.label gssVertex.Level gssVertex.State with
                | Some x -> x
                | None -> 
                    let dI = nextIndex()
                    dict.Add dest dI
                    printVertex dI false (isAccepting dest)
                    for edge in dest.outEdges do
                        searchStack.Push (dI, edge.label, edge.dest)
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
            searchStack.Push (startIndex, edge.label, edge.dest)

        while searchStack.Count > 0 do
            let sourceIndex, edgeLabel, dest = searchStack.Pop()
            searchStep sourceIndex edgeLabel dest
            

    reductionToDot sppf

    out.WriteLine("}")