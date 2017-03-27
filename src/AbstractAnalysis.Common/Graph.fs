﻿namespace AbstractAnalysis.Common

open QuickGraph
open System.Runtime.CompilerServices

[<Measure>] type token
[<Measure>] type gssVertex
[<Measure>] type nodeMeasure
[<Measure>] type positionInInput
[<Measure>] type positionInGrammar
[<Measure>] type length
[<Measure>] type leftPosition
[<Measure>] type extension

type IParserInput =
    abstract member InitialPositions: array<int<positionInInput>>    
    abstract member ForAllOutgoingEdges: int<positionInInput> -> (int<token> -> int<positionInInput> -> unit) -> unit
    abstract member PositionToString : int -> string

type LexerEdge<'l ,'br  when 'l: equality> (s,e,t) =
        inherit TaggedEdge<int,Option<'l*'br>>(s,e,t)
        let l,br =
            match t with
            | Some (l,br) -> Some l, Some br
            | None -> None, None

        member this.BackRef = br
        member this.Label = l

type ParserEdge<'tagType>(s, e, t)=
    inherit TaggedEdge<int, 'tagType>(s, e, t)

type ParserInputGraph<'EdgeObjType>(initialVertices, finalVertices, edgeObjToToken) = 
    inherit AdjacencyGraph<int, ParserEdge<'EdgeObjType>>()

    member val InitStates = initialVertices 
    member val FinalStates = finalVertices with get, set

    member this.PrintToDot name (numToString: int<token> -> string) = 
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        for i = 0 to this.VertexCount - 1 do
            out.Write (i.ToString() + "; ")
        out.WriteLine()
        for i in this.Vertices do
            let edges = this.OutEdges i
            for e in edges do
                let tokenName = edgeObjToToken e.Tag |> numToString
                out.WriteLine (e.Source.ToString() + " -> " + e.Target.ToString() + "[label=\"" + tokenName + "\"]")
        out.WriteLine("}")
        out.Close()

    new (initial : int, final : int, edgeObjToToken) = 
        ParserInputGraph<_>([|initial|], [|final|], edgeObjToToken)

    new (n : int, edgeObjToToken) =
        let allVerticles = [|for i in 0 .. n - 1 -> i|]
        ParserInputGraph<_>(allVerticles, allVerticles, edgeObjToToken)

    interface IParserInput with
        member this.InitialPositions = 
            Array.map (fun x -> x * 1<positionInInput>) this.InitStates

        member this.ForAllOutgoingEdges curPosInInput pFun =
            let outEdges = int curPosInInput |> this.OutEdges
            outEdges |> Seq.iter 
                (fun e ->
                    pFun (edgeObjToToken e.Tag) (e.Target * 1<positionInInput>)
                )

        member this.PositionToString (pos : int) = 
            sprintf "%i" pos

type LinearInput (initialPositions, input:array<int<token>>) =
    interface IParserInput with
        member x.PositionToString(pos: int): string = 
            sprintf "%i" pos
        member this.InitialPositions = initialPositions
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput pFun =
            if int curPosInInput < input.Length
            then pFun input.[int curPosInInput] (curPosInInput + 1<positionInInput>)
    
    member this.Input = input

    new (input:array<int<token>>) = LinearInput ([|0<positionInInput>|], input)

type SimpleGraphInput<'tagType> (initialPositions, getTokenFromTag:'tagType -> int<token>) =
    inherit AdjacencyGraph<int, ParserEdge<'tagType>>()

    interface IParserInput with
        member this.InitialPositions = initialPositions

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput pFun =
            let outEdges = int curPosInInput |> this.OutEdges
            outEdges
            |> Seq.iter 
                (fun e ->
                    pFun (getTokenFromTag e.Tag) (e.Target * 1<positionInInput>)
                )
        
        member x.PositionToString(pos: int): string = 
            sprintf "%i" pos