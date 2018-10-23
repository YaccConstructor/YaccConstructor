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
[<Measure>] type priority

type LexerEdge<'l ,'br  when 'l: equality> (s, e, t) =
    inherit TaggedEdge<int,Option<'l * 'br>>(s, e, t)
    let l, br =
        match t with
        | Some (l, br) -> Some l, Some br
        | None -> None, None

    member this.BackRef = br
    member this.Label = l

type IParserInput =
    abstract member InitialPositions   : array<int<positionInInput>>
    abstract member FinalPositions     : array<int<positionInInput>>
    abstract member ForAllOutgoingEdges: int<positionInInput> -> int<priority> -> (int<token> -> int<positionInInput> -> int<priority> -> unit) -> unit
    
    abstract member PositionToString : int<positionInInput> -> string

type ParserEdge<'tag>(s, e, t)=
    inherit TaggedEdge<int, 'tag>(s, e, t)
    
type SimpleInputGraph<'tag>(initialVertices : int[], finalVertices : int[], tagToToken : 'tag -> int<token>) = 
    inherit AdjacencyGraph<int, ParserEdge<'tag>>()

    member val InitStates = initialVertices 
    member val FinalStates = finalVertices with get, set
    member val TagToToken = tagToToken with get

    member this.PrintToDot name (tagToString: 'tag -> string) (*(tokenToString : 'token -> string) (numToToken : int -> 'token)*) = 
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        for i=0 to this.VertexCount-1 do
            out.Write (i.ToString() + "; ")
        out.WriteLine()
        for i in this.Vertices do
            let edges = this.OutEdges i
            for e in edges do
                let tokenName = e.Tag |> tagToString 
                out.WriteLine (e.Source.ToString() + " -> " + e.Target.ToString() + "[label=\"" + tokenName + "\"]")
        out.WriteLine("}")
        out.Close()      

    new (initial : int, final : int, tagToToken : 'tag -> int<token>) = 
        SimpleInputGraph<_>([|initial|], [|final|], tagToToken)

    new (n : int, tagToToken : 'tag -> int<token>) =
        let allVertices = [|for i in 0 .. n - 1 -> i|]
        SimpleInputGraph<_>(allVertices, allVertices, tagToToken)

    new (initial : int<positionInInput>[], tagToToken : 'tag -> int<token>) = 
        let casted = Array.map(fun x -> int x) initial
        SimpleInputGraph<_>(casted, casted, tagToToken)
 
    interface IParserInput with
        member this.InitialPositions = 
            Array.map(fun x -> x * 1<positionInInput>) this.InitStates
        
        member this.FinalPositions = 
            Array.map(fun x -> x * 1<positionInInput>) this.FinalStates

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput priority pFun =
            let outEdges = int curPosInInput |> this.OutEdges
            outEdges |> Seq.iter
                (fun e -> pFun (this.TagToToken e.Tag) (e.Target * 1<positionInInput>) priority)

        member this.PositionToString (pos : int<positionInInput>) =
            sprintf "%i" pos


type LinearInput (initialPositions, input:array<int<token>>) =
    interface IParserInput with
        member x.PositionToString(pos: int<positionInInput>): string = 
            sprintf "%i" pos

        member this.InitialPositions = initialPositions
        
        member this.FinalPositions = [|input.Length * 1<positionInInput>|]
        
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput priority pFun =
            if int curPosInInput < input.Length
            then pFun input.[int curPosInInput] (curPosInInput + 1<positionInInput>) priority

    member this.Input = input

    new (input:array<int<token>>) = LinearInput ([|0<positionInInput>|], input)

type LinearIputWithErrors(input: int<token> array, epsionTag, nextSymbolsForInsert) = 
    interface IParserInput with
        member x.PositionToString(pos: int<positionInInput>): string = 
            sprintf "%i" pos

        member this.InitialPositions = [|0<positionInInput>|]
        
        member this.FinalPositions = [|input.Length * 1<positionInInput>|]

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput priority pFun =
            if int curPosInInput < input.Length
            then 
                pFun input.[int curPosInInput] (curPosInInput + 1<positionInInput>) (priority + 1<priority>)
                pFun epsionTag (curPosInInput + 1<positionInInput>) (priority + (input.Length - int curPosInInput + 1) * (input.Length * 100) * 1<priority>)

    member this.Input = input

type GraphLabelledVertex<'tagType when 'tagType : equality> (initialVertices : 'tagType[], finalVertices : 'tagType[], tagToToken : 'tagType -> int) = 
    inherit AdjacencyGraph<'tagType, TaggedEdge<'tagType, 'tagType>>()

    let vMap = new System.Collections.Generic.Dictionary<_,_>()
    let vBackMap = new ResizeArray<_>()

    member val InitStates = initialVertices 
    member val FinalStates = finalVertices with get, set
    member val TagToToken = tagToToken with get

    member this.AddEdges (edges: TaggedEdge<'tagType, 'tagType>[]) = 
        this.AddVerticesAndEdgeRange edges |> ignore
        this.Vertices
        |> Seq.iteri (fun i v ->
            vMap.Add(v, i)
            vBackMap.Add v
        )

    new (initial : 'tagType[], tagToToken : 'tagType -> int) = 
          GraphLabelledVertex<_>(initial, initial, tagToToken)

    interface IParserInput with
        member this.InitialPositions = 
            Array.map(fun x -> 
                match (vMap.TryGetValue x) with 
                | (true, i) -> 
                    i * 2<positionInInput>
                | (false, _) -> failwithf "There is no vertex %A" x
            ) this.InitStates
        
        member this.FinalPositions =
            Array.map(fun x -> 
                match (vMap.TryGetValue x) with 
                | (true, i) -> 
                    i * 2<positionInInput>
                | (false, _) -> failwithf "There is no vertex %A" x
            ) this.FinalStates

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput priority pFun =
            if ((int)curPosInInput % 2) = 0 
            then 
                let v = vBackMap.[(int)curPosInInput / 2]
                pFun ((this.TagToToken v) * 1<token>) (curPosInInput + 1<positionInInput>) priority
            else 
                let v = vBackMap.[((int)curPosInInput - 1) / 2]
                this.OutEdges v |> Seq.iter
                    (fun e -> pFun ((this.TagToToken e.Tag) * 1<token>) (vMap.[e.Target] * 2<positionInInput>) priority) 

        member this.PositionToString (pos : int<positionInInput>) =
            sprintf "%i" pos