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
[<Measure>] type nonterm
[<Measure>] type extension

type LexerEdge<'l ,'br  when 'l: equality> (s,e,t) =
    inherit TaggedEdge<int,Option<'l*'br>>(s,e,t)
    let l,br =
        match t with
        | Some (l,br) -> Some l, Some br
        | None -> None, None

    member this.BackRef = br
    member this.Label = l

type IParserInput =
    abstract member InitialPositions: array<int<positionInInput>>    
    abstract member ForAllOutgoingEdges: int<positionInInput> -> (int<token> -> int<positionInInput> -> unit) -> unit

type ParserEdge<'token>(s, e, t)=
    inherit TaggedEdge<int, 'token>(s,e,t)
    
type ParserInputGraph<'token>(initialVertices : int[], finalVertices : int[]) = 
    inherit AdjacencyGraph<int,ParserEdge<'token>>()

    member val InitStates = initialVertices 
    member val FinalStates = finalVertices with get, set

    member this.PrintToDot name (numToString: 'token -> string) (*(tokenToString : 'token -> string) (numToToken : int -> 'token)*) = 
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        for i=0 to this.VertexCount-1 do
            out.Write (i.ToString() + "; ")
        out.WriteLine()
        for i in this.Vertices do
            let edges = this.OutEdges i
            for e in edges do
                let tokenName = e.Tag |> numToString(*numToToken |> tokenToString*)
                out.WriteLine (e.Source.ToString() + " -> " + e.Target.ToString() + "[label=\"" + tokenName + "\"]")
        out.WriteLine("}")
        out.Close()      

    new (initial : int, final : int) = 
        ParserInputGraph<_>([|initial|], [|final|])
 

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

type EdgeCompressedGraphInput<'tagType> (protoGraph:AdjacencyGraph<int, TaggedEdge<int, 'tagType>>, getTokensFromTag:'tagType -> array<int<token>>) =
    let packPosition edge position =
        if (edge < 65536) && (position < 65536) then ((int position <<< 16) ||| int edge) * 1<positionInInput>
        else failwith "Edge or position is greater then 65535!!"
    let edges = protoGraph.Edges |> Array.ofSeq
    let initialPositions = 
        let buf = new ResizeArray<_>()
        edges
        |> Array.iteri (fun i e ->
            buf.Add(packPosition -1 e.Source)
            buf.Add(packPosition -1 e.Target)
            for j in 1 .. (getTokensFromTag e.Tag).Length - 1 do
                buf.Add (packPosition i j)
            )
        buf.ToArray()

    interface IParserInput with
        member this.InitialPositions = initialPositions
        member this.ForAllOutgoingEdges curPosInInput pFun =
            let inline getEdge (packedValue : int<positionInInput>)      = int (int packedValue &&& 0xffff)
            let inline getPosOnEdge (packedValue : int<positionInInput>) = int (uint32 packedValue >>> 16)
            let eId = getEdge curPosInInput
            if  eId = -1
            then 
                let vId = getPosOnEdge curPosInInput                
                protoGraph.OutEdges vId
                |> Seq.iter (fun e -> 
                    let nextPos =
                        if (getTokensFromTag e.Tag).Length = 1
                        then packPosition -1 e.Target
                        else packPosition (Array.findIndex ((=)e) edges) 1
                    pFun (getTokensFromTag e.Tag).[0] nextPos
                    )
            else
                let e = edges.[eId]
                let posOnEdge = getPosOnEdge curPosInInput
                let tokens = getTokensFromTag e.Tag
                let nextPos =
                    if posOnEdge = tokens.Length - 1
                    then packPosition -1 e.Target
                    else packPosition eId (posOnEdge + 1)
                pFun tokens.[posOnEdge] nextPos
