module YC.BIO.BioGraphLoader

open System.IO
open AbstractAnalysis.Common

module Seq =

    /// Returns a sequence that yields chunks of length n.
    /// Each chunk is returned as a list.
    let split length (xs: seq<'T>) =
        let l = ref <| Seq.length xs
        let rec loop xs =
            seq{
                yield Seq.truncate length xs |> Seq.toList                
                if not (!l <= length)
                then 
                    l := !l - length
                    yield! loop (Seq.skip length xs)
            }
        loop xs

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
    val str: string
    val length : int
    new (_s,_l) = {str=_s;length=_l}

type BioGraphEdge(s,e,t) =
    inherit QuickGraph.TaggedEdge<int, BioGraphEdgeLbl>(s,e,t)
    new (s,e,str,l) = BioGraphEdge(s,e,new BioGraphEdgeLbl(str,l))

let loadGraphFormFileToQG fileWithoutExt templateLengthHightLimit =
    let lblsExt = ".sqn"
    let graphStrauctureExt = ".grp"
    let lbls = 
        File.ReadAllLines(fileWithoutExt + lblsExt)
        |> Array.ofSeq
        |> Array.split 2
        |> Array.Parallel.map (fun a -> a.[0].Trim().TrimStart('>') |> int,a.[1].Trim())
        |> dict

    let loadGraphStrucureFile repl =
        File.ReadAllLines(fileWithoutExt + graphStrauctureExt)
        |> Seq.filter (fun s -> s.StartsWith repl)
        |> Seq.map (fun s -> s.Replace(repl,"").Trim([|' '; '.'|]).Split([|':'; '~'; ','|]))
    let vertices = 
        loadGraphStrucureFile "Vertex"
        |> Seq.collect (fun a -> [a.[0].Trim() |> int; a.[1].Trim() |> int])
        |> Set.ofSeq
        |> Array.ofSeq
    printfn "Vertices load: %A" vertices.Length
    let edges = 
        loadGraphStrucureFile "Edge"
        |> Seq.map (fun a -> 
            let x = (a.[1].Split '>')
            a.[0].Trim() |> int
            , x.[0].Trim([|' '; '-'|]) |> int
            , x.[1].Trim() |> int
            , (a.[2].Split '=').[1].Trim() |> int)
        |> Array.ofSeq
    printfn "Edges load: %A" edges.Length
    let qGraph = new QuickGraph.AdjacencyGraph<_,_>()
    let cnt = ref (vertices |> Array.max |> ((+)1))
    let leCount = ref 0
    let edgs = 
        edges 
        |> Array.Parallel.map (fun (id,s,e,l) -> new BioGraphEdge(s,e,lbls.[id],l))
        |> Array.collect 
            (fun e -> 
                if e.Tag.length <= templateLengthHightLimit
                then 
                    //printfn "%A" e.Tag.length
                    [|e|]
                else 
                    incr leCount
                    let str1 = e.Tag.str.Substring(0,templateLengthHightLimit)
                    let str2 = e.Tag.str.Substring(e.Tag.str.Length-1-templateLengthHightLimit)
                    incr cnt
                    let newE = !cnt
                    incr cnt
                    let newS = !cnt
                    [|new BioGraphEdge(e.Source,newE,str1,templateLengthHightLimit);BioGraphEdge(newS,e.Target,str2,templateLengthHightLimit)|]
            )
        |> Array.ofSeq
    //|> qGraph.AddVerticesAndEdgeRange
    //|> ignore    

    printfn "long edges %A" !leCount

    let ug = new QuickGraph.UndirectedGraph<_,_>(true)
    ug.AddVerticesAndEdgeRange edgs
    |> ignore
    let a = QuickGraph.Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(ug)
    a.Compute()    
    a.ComponentCount |> printfn "Connected=%A"
    let components = 
        let x =
            a.Components
            |> Seq.groupBy(fun kvp -> kvp.Value)
            |> Array.ofSeq
            |> Array.Parallel.map (fun (x,s) -> s |> Seq.map (fun kvp -> kvp.Key) |> Array.ofSeq)
            |> Array.filter(fun a -> a.Length > 1)
            |> Array.sortBy (Array.length >> ((*)-1))
            
        x
        |> Array.Parallel.map (fun vs -> vs |> Array.collect (fun v -> ug.AdjacentEdges v |> Array.ofSeq) |> (fun c -> new System.Collections.Generic.HashSet<_>(c)) |> Array.ofSeq )
        //|> Array.Parallel.map (fun vs -> edgs |> Array.filter (fun e -> Array.exists (fun v -> e.Source = v || e.Target = v) vs))
    
    let avgl = components |> Array.map (fun c -> c |> Array.averageBy (fun x -> float x.Tag.length))
    let suml = components |> Array.map (fun c -> c |> Array.sumBy (fun x -> x.Tag.length))
            
    printfn "Avg %A" (avgl)
    printfn "Sum %A" (suml)

    printfn "L %A" (Seq.length components)
    components
    |> Seq.head
    |> qGraph.AddVerticesAndEdgeRange 
    |> ignore
    qGraph


let loadGraphFormFileToOarserInputGraph fileWithoutExt templateLengthHightLimit tokenizer eof =
    let edg f t l = new ParserEdge<_>(f,t,l)    
    let g = loadGraphFormFileToQG fileWithoutExt templateLengthHightLimit    
    let cnt = ref (g.Vertices |> Seq.max |> ((+)1))
    let edgs = 
        g.Edges    
        |> Seq.collect (fun e -> e.Tag.str.ToCharArray()
                                 |> Array.mapi (fun i ch -> edg (if i = 0 then e.Source else (!cnt)) (if i = e.Tag.str.Length - 1 then e.Target else (incr cnt ; !cnt)) (tokenizer ch) ))
        |> Array.ofSeq

    let newVMap =    
        edgs 
        |> Array.fold
            (fun l e ->
              e.Source :: e.Target :: l  )
            []
        |> Set.ofList
        |> Set.toArray
        |> Array.mapi(fun i s -> (s,i))
        |> dict

    let finalV = newVMap |> Seq.length
    //53072
    let parserInputGraph = new ParserInputGraph<_>(0, finalV)

    edgs
    |> Array.map
        (fun e -> edg newVMap.[e.Source] newVMap.[e.Target] e.Tag)
    |> parserInputGraph.AddVerticesAndEdgeRange
    |> ignore

    g.Vertices 
    |> Seq.map (fun v -> edg v finalV eof)
    |> parserInputGraph.AddVerticesAndEdgeRange
    |> ignore    

    printfn "Vert Count = %A" (!cnt)
    parserInputGraph
    
