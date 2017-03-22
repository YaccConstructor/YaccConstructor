namespace AbstractAnalysis.Common

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

 type LinearInput (initialPositions, input:array<int<token>>) =
        interface IParserInput with
            member this.InitialPositions = initialPositions
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.ForAllOutgoingEdges curPosInInput pFun =
                if int curPosInInput < input.Length
                then pFun input.[int curPosInInput] (curPosInInput + 1<positionInInput>)

        member this.Input = input

        new (input:array<int<token>>) = LinearInput ([|0<positionInInput>|], input)

type SimpleGraphInput<'tagType> (initialPositions, getTokenFromTag:'tagType -> int<token>) =
    inherit AdjacencyGraph<int, TaggedEdge<int, 'tagType>>()
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