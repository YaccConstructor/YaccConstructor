namespace Yard.Generators.RNGLR.ReadBack

open Yard.Generators.Common
open Yard.Generators.Common.EBNF
open System.Collections.Generic

[<AllowNullLiteral>]
type VertexWithBackTrack<'VertexLabel, 'EdgeLabel>(label : 'VertexLabel) =
    inherit Vertex<'VertexLabel, 'EdgeLabel>(label)
    let _in = new ResizeArray<Edge<'VertexLabel, 'EdgeLabel>>(4)
    member this.addEdgeWithBackTrack edge = 
        this.addEdge edge
        let backTrack = new Edge<_,_>(this, edge.label)
        edge.dest.addEdge backTrack
        _in.Add backTrack
    member this.inEdges = _in

type ParserSourceReadBack<'TokenType> (gotos : int[][]
                               , reduces : (int * int)[][][]
                               , zeroReduces : int[][][]
                               , accStates : bool[]
                               , nfas : NFATable
                               , leftSide : int[]
                               , startRule : int
                               , eofIndex : int
                               , tokenToNumber : 'TokenType -> int
                               , acceptEmptyInput : bool
                               , numToString : int -> string
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
            numberOfStates, stateToVertex

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
    member this.AcceptEmptyInput = acceptEmptyInput
    member this.NumToString = numToString
    member this.ErrorIndex = errorIndex