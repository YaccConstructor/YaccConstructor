module YC.Bio.GraphLoader

open AbstractAnalysis.Common
open QuickGraph

open System.IO
open Microsoft.FSharp.Collections
open System.Collections.Generic
open InfernalApi
open System.Runtime.CompilerServices

module Array =
    /// Returns a sequence that yields chunks of length n.
    /// Each chunk is returned as a list.
    let split length (a: array<'T>) =
        let l = ref 0
        [|
            while !l < a.Length do
                yield Array.sub a !l (min length (a.Length - !l))
                l := !l + length 
        |]

[<Struct>]
type BioGraphEdgeLbl=
    val str: array<int<token>>
    val length : int
    val id : int
    val sourceStartPos : int
    new (_s,_l, _sourceId, _sourceStartPos) =
        {str=_s; length=_l; id = _sourceId; sourceStartPos = _sourceStartPos}
    //new (_s, _id) = {str=_s; length=_s.Length; id=_id}

[<Measure>] type vNumInOriginalGraph
[<Measure>] type posInSubgraph

type EdgeCompressedGraphInput (edges: array<TaggedEdge<int<vNumInOriginalGraph>, BioGraphEdgeLbl>>, tokenizer:char -> int<token>) as this =
    inherit AdjacencyGraph<int<vNumInOriginalGraph>,TaggedEdge<int<vNumInOriginalGraph>,BioGraphEdgeLbl>>()
    let vMap = new Dictionary<_,_>()
    let vBackMap = new ResizeArray<_>()
    do 
        this.AddVerticesAndEdgeRange edges |> ignore
        this.Vertices
        |> Seq.iteri (fun i v ->
            vMap.Add(v, i * 1<posInSubgraph>)
            vBackMap.Add v
        )
//    let eTokens =
//        edges
//        |> Array.map (fun e -> e.Tag.str.ToCharArray() |> Array.map tokenizer)
    let packPosition edge (position: int<posInSubgraph>) =
        if (edge < 65536) && (int position < 65536) then ((int edge <<< 16) ||| int position) * 1<positionInInput>
        else failwithf "Edge or position is greater then 65535: edge: %A; pos: %A" edge position
    let initialPositions = 
        let buf = new ResizeArray<_>()
        edges
        |> Array.iteri (fun i e ->
            buf.Add(packPosition -1 vMap.[e.Source])
            buf.Add(packPosition -1 vMap.[e.Target])
            for j in 1 .. e.Tag.str.Length - 1 do
                buf.Add (packPosition i (j * 1<posInSubgraph>))
            )
        buf.ToArray()

    interface IParserInput with
        member this.InitialPositions = initialPositions
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ForAllOutgoingEdges curPosInInput pFun =
            let inline getPosOnEdge (packedValue : int<positionInInput>) = int (int packedValue &&& 0xffff) * 1<posInSubgraph>
            let inline getEdge (packedValue : int<positionInInput>) = 
                int (int packedValue >>> 16) 
            let eId = getEdge curPosInInput
            if  eId = -1
            then 
                let vId = getPosOnEdge curPosInInput
                this.OutEdges vBackMap.[int vId]
                |> Seq.iter (fun e -> 
                    let nextPos =
                        if e.Tag.str.Length = 1
                        then packPosition -1 vMap.[e.Target]
                        else packPosition (Array.findIndex ((=)e) edges) 1<posInSubgraph>
                    pFun e.Tag.str.[0] nextPos
                    )
            else
                let e = edges.[eId]
                let posOnEdge = getPosOnEdge curPosInInput
                let tokens = e.Tag.str
                let nextPos =
                    if posOnEdge = (tokens.Length - 1) * 1<posInSubgraph>
                    then packPosition -1 vMap.[e.Target]
                    else packPosition eId (posOnEdge + 1<posInSubgraph>)
                pFun tokens.[int posOnEdge] nextPos

let undirectedToBioEdge (e:TaggedEdge<int,BioGraphEdgeLbl>) = 
    new TaggedEdge<int,BioGraphEdgeLbl>(e.Source, e.Target, e.Tag)

let loadGraphFormFileToQG fileWithoutExt templateLengthHighLimit tokenizer =
    let newEdge source target str len sourceId sourceStartPos = 
        new TaggedEdge<int<vNumInOriginalGraph>,_>(source * 1<vNumInOriginalGraph>, target * 1<vNumInOriginalGraph>, new BioGraphEdgeLbl(str, len, sourceId, sourceStartPos))

    let lblsExt = ".sqn"
    let graphStrauctureExt = ".grp"
    let maxComponentSize = 90000
    let edgesСontent = 
        File.ReadAllLines(fileWithoutExt + lblsExt)
        |> Array.split 2
        |> Array.Parallel.map (fun a -> a.[0].Trim().TrimStart('>') |> int,a.[1].Trim())
        |> dict

    let loadGraphStrucureFile repl =
        File.ReadAllLines(fileWithoutExt + graphStrauctureExt)
        |> Array.filter (fun s -> s.StartsWith repl)
        |> Array.map (fun s -> s.Replace(repl,"").Trim([|' '; '.'|]).Split([|':'; '~'; ','|]))
    let vertices = 
        loadGraphStrucureFile "Vertex"
        |> Array.collect (fun a -> [|a.[0].Trim() |> int; a.[1].Trim() |> int|])
        |> Set.ofSeq
        |> Array.ofSeq
    printfn "Vertices loaded: %A" vertices.Length
    let edges = 
        loadGraphStrucureFile "Edge"
        |> Array.map (fun a -> 
            let x = (a.[1].Split '>')
            a.[0].Trim() |> int
            , x.[0].Trim([|' '; '-'|]) |> int
            , x.[1].Trim() |> int
            , (a.[2].Split '=').[1].Trim() |> int)
    printfn "Edges loaded: %A" edges.Length
    let cnt = ref (vertices |> Array.max |> ((+)1))
    let leCount = ref 0
    let longEdges = ResizeArray<_>()
    //let deletedEdges = ResizeArray<_>()
    (*
    let max = 
        edges
        |> Array.maxBy (fun (id,s,e,l) -> s)
        |> (fun (id,s,e,l) -> s)
    printfn "Max number of vert: %A"  max
    let outDegree = Array.init (max+1) (fun s -> ref 0)
    edges
    |> Array.iter (fun (id,s,e,l) -> incr outDegree.[s])
    printfn "Number of vertices with outDegree greater than 4 %A" (outDegree |> Array.sumBy (fun n -> if !n > 4 then 1 else 0))
    *)
    let edgs = 
        edges       
        |> Array.Parallel.map (fun (id,start,ending,length) -> 
            newEdge start ending (edgesСontent.[id].ToCharArray() |> Array.map tokenizer) length id 0)
        |> Array.collect (fun e -> 
            let shift = e.Tag.str.Length - e.Tag.length
            if shift <> 0 
            then
                incr cnt 
                let newV = !cnt
                [| newEdge (int e.Source) (int e.Target) e.Tag.str.[shift..] e.Tag.length e.Tag.id shift |]
            else [|e|])
        
        |> Array.collect 
            (fun e -> 
                if e.Tag.length <= templateLengthHighLimit
                then 
                    //printfn "%A" e.Tag.length
                    //if e.Tag.length <= 100//templateLengthHighLimit - 100
                    (*then*) [|e|]
                    (*else deletedEdges.Add e
                         [||]*)
                else 
                    longEdges.Add e
                    incr leCount
                    let str1 = 
                        e.Tag.str.[0 .. templateLengthHighLimit]
                        
                    let startOfSecondEdge= e.Tag.str.Length - templateLengthHighLimit
                    let str2 = e.Tag.str.[startOfSecondEdge ..]
                    incr cnt
                    let newEnd = !cnt
                    incr cnt
                    let newStart = !cnt
                    [|newEdge (int e.Source) newEnd str1 templateLengthHighLimit e.Tag.id e.Tag.sourceStartPos;
                      newEdge newStart (int e.Target) str2 templateLengthHighLimit e.Tag.id startOfSecondEdge|]  
            )
                    
    printfn "long edges %A" !leCount

    let filterEdges edges = 
        let toPrint =
            edges
            |> Array.filter(fun (edge : TaggedEdge<_,BioGraphEdgeLbl>) -> edge.Tag.str.Length > 30)
            |> Array.map (fun (edge : TaggedEdge<_,BioGraphEdgeLbl>) ->
                sprintf ">%i\n%A" edge.Tag.id edge.Tag.str)

        File.WriteAllLines("edgesToFilter.fa", toPrint)
        
        let newEdgesSet = 
            filterWithInfernal (System.AppDomain.CurrentDomain.BaseDirectory + "edgesToFilter.fa")
            |> Array.map (fun (s, _,_) -> int s)
            |> Set

        edges
        |> Array.filter (fun x -> 
            newEdgesSet.Contains x.Tag.id || x.Tag.str.Length <= 30)

    //let edgs = edgs |> filterEdges

    let uGraph = new UndirectedGraph<_,_>(true)
    uGraph.AddVerticesAndEdgeRange edgs
    |> ignore

    let divisionOnComponents () =
        let algo = Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(uGraph)
        algo.Compute()    
        algo.ComponentCount |> printfn "Connected components count=%A"
        let components = 
            let x =
                algo.Components
                |> Seq.groupBy(fun kvp -> kvp.Value)
                |> Array.ofSeq
                |> Array.map (fun (x,s) ->
                    s
                    |> Array.ofSeq
                    |> Array.map (fun kvp -> kvp.Key))
                |> Array.filter(fun a -> a.Length > 1)
                |> Array.sortBy (Array.length >> ((*)-1))
            
            x
            |> Array.Parallel.map (fun vs ->
                vs
                |> Array.collect (uGraph.AdjacentEdges >> Array.ofSeq)
                |> (fun c -> new HashSet<_>(c))
                |> Array.ofSeq )
        components
    
    let components = divisionOnComponents ()
    (*
    let clustered1 = cluster edgs

    let index = ref 1000000

    let toCluster1 = 
        clustered1.[0]
        |> Array.collect (fun e -> if e.Tag.length <= 100
                                   then [|e|]
                                   else deletedEdges.Add e
                                        let edge1 = new BioGraphEdge(e.Source, !index, e.Tag.str, e.Tag.length, !index)
                                        incr index
                                        let edge2 = new BioGraphEdge(!index, e.Target, e.Tag.str, e.Tag.length, !index)
                                        incr index
                                        [|(*edge1; edge2*)|])
    let components = cluster toCluster1
    *)
    //print deleted edges 
    (*
    let printDeleted = 
        let maxLineLength = 80
        let resultPath = ".\\result.fa"

        let rec splitLine (line:string) =
            if line.Length <= maxLineLength then [line] else
            (line.Substring (0, maxLineLength))::(splitLine (line.Substring maxLineLength))
        

        let info = ">Deleted"
        
        let index = ref 0
        let toPrint = 
            deletedEdges
            |> Seq.collect (fun e -> let line = e.Tag.str
                                     let header = info + (!index).ToString()
                                     index := !index + 1
                                     header::(splitLine line))
        File.AppendAllLines(resultPath, toPrint)
        *)

//    let avgl = components |> Array.map (fun c -> c |> Array.averageBy (fun x -> float x.Tag.length))
//    let suml = components |> Array.map (fun c -> c |> Array.sumBy (fun x -> x.Tag.length))
//            
//    printfn "Avg %A" (avgl)
//    printfn "Sum %A" (suml)
    let longEdges = 
        longEdges
        |> Seq.map (fun e -> e.Tag.str)
        |> Array.ofSeq
    

    printfn "L %A" (Seq.length components)

    let getInfo prefix index = 
        ">" + prefix + index.ToString() + "\n"

    let printStringsToFASTA path prefix lines =
        lines
        |> Seq.mapi (fun i line -> (getInfo prefix i) + line)
        |> (fun x -> File.AppendAllLines(path, x))

    edgs,
    components
    |> Array.Parallel.map
       (fun edges -> new EdgeCompressedGraphInput(edges, tokenizer))
    , longEdges
    
let loadGraph fileWithoutExt templateLengthHighLimit tokenizer =
    let sourceDraph, gs, longEdges = loadGraphFormFileToQG fileWithoutExt templateLengthHighLimit tokenizer
    sourceDraph
    , gs // |> Array.Parallel.map convert
    ,longEdges