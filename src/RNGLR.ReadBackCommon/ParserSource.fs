namespace Yard.Generators.RNGLR.ReadBack

open Yard.Generators.Common
open Yard.Generators.Common.EBNF
open System.Collections.Generic

type VertexWithBackTrack<'VertexLabel, 'EdgeLabel>(label : 'VertexLabel) =
    inherit Vertex<'VertexLabel, 'EdgeLabel>(label)
    let _in = new ResizeArray<Edge<'VertexLabel, 'EdgeLabel>>(4)
    member private this.addBackTrackEdge edge =
        _in.Add edge
    member this.addEdgeWithBackTrack edge = 
        this.addEdge edge
        (edge.dest :?> VertexWithBackTrack<_,_>).addBackTrackEdge (new Edge<_,_>(this, edge.label))
    member this.inEdges = _in

type SymbolType =
    | Nonterminal
    | Terminal
    | Epsilon

type ParserSourceReadBack<'TokenType> (gotos : int[][]
                               , reduces : (int * int)[][][]
                               , zeroReduces : int[][][]
                               , accStates : bool[]
                               , nfas : NFATable
                               , leftSide : int[]
                               , startRule : int
                               , eofIndex : int
                               , tokenToNumber : 'TokenType -> int
                               , indexToSymbolType: int -> SymbolType
                               , acceptEmptyInput : bool
                               , numToString : int -> string
                               , nonTermToRule : int[]
                               , canInferEpsilon : bool[] 
                               , epsilonIndex : int
                               , errorIndex : int) =
    
    let _nfas =
        let openNfa nfa =
            let numberOfStates, allTransitions = nfa
            let stateToVertex = Array.init numberOfStates (fun i -> new VertexWithBackTrack<_,_>(i))
            let rec setAllTransitions = function
                | (state, transitions) :: ats ->
                    let vertex : VertexWithBackTrack<_, _> = stateToVertex.[state]
                    let rec setTransitions = function
                    | (dest, label) :: ts ->
                        vertex.addEdgeWithBackTrack(new Edge<_,_>(stateToVertex.[dest], label))
                        setTransitions ts
                    | [] -> ()
                    setTransitions transitions
                    setAllTransitions ats
                | [] -> ()
            setAllTransitions allTransitions
            stateToVertex

        nfas |> Array.map openNfa

    member this.Reduces = reduces
    member this.ZeroReduces = zeroReduces
    member this.Gotos = gotos
    member this.AccStates = accStates
    member this.RightSideNFA = _nfas
    member this.LeftSide = leftSide
    member this.StartRule = startRule
    member this.EofIndex = eofIndex
    member this.TokenToNumber = tokenToNumber
    member this.indexToSymbolType = indexToSymbolType
    member this.AcceptEmptyInput = acceptEmptyInput
    member this.NumToString = numToString
    member this.EpsilonIndex = epsilonIndex
    member this.ErrorIndex = errorIndex
    member this.CanInferEpsilon = canInferEpsilon
    member this.NontermToRule = nonTermToRule