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
    inherit TaggedEdge<int, 'token>(s, e, t)
    
type ParserInputGraph<'token>(initialVertices : int[], finalVertices : int[]) = 
    inherit AdjacencyGraph<int, ParserEdge<'token>>()

    member val InitStates = initialVertices 
    member val FinalStates = finalVertices with get, set

    member this.PrintToDot name (numToString: 'token -> string) (*(tokenToString : 'token -> string) (numToToken : int -> 'token)*) = 
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        for i = 0 to this.VertexCount - 1 do
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

    new (n : int) =
        let allVerticles = [|for i in 0 .. n - 1 -> i|]
        ParserInputGraph<_>(allVerticles, allVerticles)
 

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
