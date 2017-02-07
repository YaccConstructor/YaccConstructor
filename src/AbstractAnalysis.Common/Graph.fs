namespace AbstractAnalysis.Common

open QuickGraph

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
        member this.ForAllOutgoingEdges curPosInInput pFun =
            if int curPosInInput < input.Length
            then pFun input.[int curPosInInput] (curPosInInput + 1<positionInInput>)

    member this.Input = input

    new (input:array<int<token>>) = LinearInput ([|0<positionInInput>|], input)

type SimpleGraphInput<'tagType> (initialPositions, getTokenFromTag:'tagType -> int<token>) =
    inherit AdjacencyGraph<int, TaggedEdge<int, 'tagType>>()
    interface IParserInput with
        member this.InitialPositions = initialPositions
        member this.ForAllOutgoingEdges curPosInInput pFun =
            let outEdges = int curPosInInput |> this.OutEdges
            outEdges
            |> Seq.iter 
                (fun e ->
                    pFun (getTokenFromTag e.Tag) (e.Target * 1<positionInInput>)
                )

//type EdgeCompressedGraphInput<'tagType> (initialPositions, getTokensFromTag:'tagType -> int<token>) =
//    inherit AdjacencyGraph<int, TaggedEdge<int, 'tagType>>()
//    interface IParserInput with
//        member this.InitialPositions = initialPositions
//        member this.ForAllOutgoingEdges curPosInInput pFun =
//            let pack2to32 edge position =
//                if (edge < 65536) && (position < 65536) then ((int position <<< 16) ||| int edge)
//                else failwith "Edge or position is greater then 65535!!"
//            let outEdges = int curPosInInput |> this.OutEdges
//            outEdges
//            |> Seq.iter 
//                (fun e ->
//                    pFun (getTokenFromTag e.Tag) (e.Target *1<positionInInput>)
//                )


//type BioParserEdge(s : int, e : int, l : int, t : int[], id : int, startPos : int) =
//    member this.Start = s
//    member this.End = e
//    member this.RealLength = l
//    member this.Tokens = t 
//    member this.SourceId = id
//    member this.SourceStartPos = startPos
//    override this.ToString () = (this.Start.ToString()) + "- "+ (this.Tokens.[0].ToString()) + " ->" + (this.End.ToString()) 
//      
//type BioParserInputGraph(edges : BioParserEdge[], initialEdges : Set<int>) =
////    inherit AdjacencyGraph<int, TaggedEdge<int, int[]>>()
////    do
////        edges |> Array.map (fun e -> new TaggedEdge<_,_>(e.s, e.e, e.Tokens))
//    let pack2to32 edge position =
//        if (edge < 65536) && (position < 65536) then ((int position <<< 16) ||| int edge)
//        else failwith "Edge or position is greater then 65535!!"
//    let edgs = Array.zeroCreate edges.Length
//    let vertexCount = ref 0
//    /// Length of each edge
//    let chainLen = Array.zeroCreate edges.Length
//    let initialPositions = new ResizeArray<_>()
//    let finalVertex = ref 0
//    do        
//        let cnt = ref 0
//        let initialVertCnt = ref 0
//        let vMap = new System.Collections.Generic.Dictionary<_,_>()
//        let getV x = 
//            let f,v = vMap.TryGetValue x
//            if f 
//            then v
//            else       
//                let newV = !cnt                
//                vMap.Add(x, newV)
//                incr cnt
//                newV
//        edges
//        |> Array.iteri (fun i e -> 
//            let edg = new BioParserEdge(getV e.Start, getV e.End, e.RealLength, e.Tokens, e.SourceId, e.SourceStartPos)
//            edgs.[i] <- edg
//            chainLen.[i] <- e.Tokens.Length
//            //shift := 0//max !shift (e.Tokens.Length - e.RealLenght)
//            if initialEdges.IsEmpty || initialEdges.Contains(i)
//            then
//                for j in 0..e.Tokens.Length - 1 do
//                    initialPositions.Add(pack2to32 i j))
//        vertexCount := vMap.Count
//    
//    member this.Edges  with get () = edgs
//    member this.InitialPositions with get () = initialPositions.ToArray()
//    member this.FinalVertex with get () = !finalVertex
//    /// Lengths of edges.
//    member this.ChainLength with get () = chainLen
//    member this.EdgeCount with get () = edgs.Length
//    member this.VertexCount with get () = !vertexCount
//    member this.Shift with get () = 0
//
//
//
