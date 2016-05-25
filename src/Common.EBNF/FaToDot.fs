module Yard.Generators.Common.FaToDot
open Yard.Generators.Common
open Yard.Generators.Common.EBNF
open System.IO

let faToDot (stateToVertex : Vertex<_,_>[]) (startStates : int[]) (finishStates : Set<int>) (indexator : IndexatorEBNF) (file : string) =
    
    let startStates = Set.ofArray startStates

    use out = new StreamWriter(file)
    
    let printVertex num isStart isAccepting =
        let str = ref <| sprintf "    %d [label = \"%d\", shape = circle" num num
        if isStart then
            str := !str + ", style = filled"
        if isAccepting then
            str := !str + ", peripheries = 2"
        str := !str + "]"
        out.WriteLine(!str)
    
    let printEdge source dest label numLabel = 
        let str = sprintf "    %d -> %d [label = \"%s | %d\"]" source dest label numLabel
        out.WriteLine(str)
    
    out.WriteLine("digraph SPPF{")
    out.WriteLine("    rankdir=LR")
    for i = 0 to stateToVertex.Length - 1 do
        let vertex = stateToVertex.[i]
        printVertex vertex.label (startStates.Contains vertex.label) (finishStates.Contains vertex.label)
        for edge in vertex.outEdges do
            let label =
                match indexator.indexToSymbol edge.label with
                | GrammarSymbol.NonTerm x
                | GrammarSymbol.Term x
                | GrammarSymbol.Literal x -> x
                | GrammarSymbol.Epsilon -> "eps"
            printEdge vertex.label edge.dest.label label edge.label
    out.WriteLine("}")