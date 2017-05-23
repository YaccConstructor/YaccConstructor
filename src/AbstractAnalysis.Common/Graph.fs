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

type LexerEdge<'l ,'br  when 'l: equality> (s, e, t) =
    inherit TaggedEdge<int,Option<'l * 'br>>(s, e, t)
    let l, br =
        match t with
        | Some (l, br) -> Some l, Some br
        | None -> None, None

    member this.BackRef = br
    member this.Label = l

type IParserInput =
    abstract member InitialPositions: array<int<positionInInput>>
        
    abstract member ForAllOutgoingEdges: int<positionInInput> -> (int<token> -> int<positionInInput> -> unit) -> unit
    
    abstract member PositionToString : int -> string

type ParserEdge<'tag>(s, e, t)=
    inherit TaggedEdge<int, 'tag>(s, e, t)
    
type SimpleInputGraph<'tag>(initialVertices : int[], finalVertices : int[], tagToToken : 'tag -> int) = 
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

    new (initial : int, final : int, tagToToken : 'tag -> int) = 
        SimpleInputGraph<_>([|initial|], [|final|], tagToToken)

    new (n : int, tagToToken : 'tag -> int) =
        let allVertices = [|for i in 0 .. n - 1 -> i|]
        SimpleInputGraph<_>(allVertices, allVertices, tagToToken)

    new (initial : int<positionInInput>[], tagToToken : 'tag -> int) = 
        let casted = Array.map(fun x -> int x) initial
        SimpleInputGraph<_>(casted, casted, tagToToken)
 
    interface IParserInput with
        member this.InitialPositions = 
            Array.map(fun x -> x * 1<positionInInput>) this.InitStates

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput pFun =
            let outEdges = int curPosInInput |> this.OutEdges
            outEdges |> Seq.iter
                (fun e -> pFun ((this.TagToToken e.Tag) * 1<token>) (e.Target * 1<positionInInput>))

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

type LinearIputWithErrors(input: int<token> array, errorTag) = 
    interface IParserInput with
        member x.PositionToString(pos: int): string = 
            sprintf "%i" pos

        member this.InitialPositions = [|0<positionInInput>|]
        
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput pFun =
            if int curPosInInput < input.Length
            then 
                pFun input.[int curPosInInput] (curPosInInput + 1<positionInInput>)
                pFun errorTag (curPosInInput + 1<positionInInput>)

    member this.Input = input

type BioParserEdge(s : int, e : int, l : int, t : int<token>[]) =
    member this.Start = s
    member this.End = e
    member this.RealLenght = l
    member this.Tokens = t 
    override this.ToString () = (this.Start.ToString()) + "->" + (this.End.ToString())

type BioParserInputGraph(edges : BioParserEdge[]) =
//    inherit AdjacencyGraph<int, TaggedEdge<int, int[]>>()
//    do
//        edges |> Array.map (fun e -> new TaggedEdge<_,_>(e.s, e.e, e.Tokens))
    let pack2to32 edge position : int<positionInInput> = 
        if (edge < 65536) && (position < 65536) then LanguagePrimitives.Int32WithMeasure((int position <<< 16) ||| int edge)
        else failwith "Edge or position is greater then 65535!!"
    let edgs = Array.zeroCreate edges.Length
    let shift = ref -1
    let vertexCount = ref 0
    let chainLen = Array.zeroCreate edges.Length
    let initialVertices = new ResizeArray<_>()
    let finalVertex = ref 0
    do        
        let cnt = ref 0
        let vMap = new System.Collections.Generic.Dictionary<_,_>()
        let getV x = 
            let f,v = vMap.TryGetValue x
            if f 
            then v
            else       
                let newV = !cnt                
                vMap.Add(x, newV)
                incr cnt
                newV
        edges
        |> Array.iteri (fun i e -> 
            let edg = new BioParserEdge(getV e.Start, getV e.End, e.RealLenght, e.Tokens)
            edgs.[i] <- edg
            chainLen.[i] <- e.Tokens.Length + 1
            shift := 0//max !shift (e.Tokens.Length - e.RealLenght)
            for j in 0..e.Tokens.Length - 1 do
                initialVertices.Add(pack2to32 i (j - !shift)))
        vertexCount := vMap.Count
    
    member this.Edges  with get () = edgs
    member this.InitialVertices with get () = initialVertices.ToArray()
    member this.FinalVertex with get () = !finalVertex
    member this.ChainLength with get () = chainLen
    member this.EdgeCount with get () = edgs.Length
    member this.VertexCount with get () = !vertexCount
    member this.Shift with get () = !shift