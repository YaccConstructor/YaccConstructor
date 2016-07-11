module YC.BIO.BioGraphLoader

open System.IO
open AbstractAnalysis.Common

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
    val id: int
    new (_s,_l, _id) = {str=_s;length=_l;id=_id}

type BioGraphEdge(s,e,t) =
    inherit QuickGraph.TaggedEdge<int, BioGraphEdgeLbl>(s,e,t)
    new (s,e,str,l,id) = BioGraphEdge(s,e,new BioGraphEdgeLbl(str,l,id))

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
    let cnt = ref (vertices |> Array.max |> ((+)1))
    let leCount = ref 0
    let longEdges = new ResizeArray<_>()
    let edgs = 
        edges         
        |> Array.Parallel.map (fun (id,s,e,l) -> new BioGraphEdge(s,e,lbls.[id],l,id))
        |> Array.collect (fun e -> 
            let shift = e.Tag.str.Length - e.Tag.length
            if shift <> 0 
            then
                incr cnt 
                let newV = !cnt
                [| (*new BioGraphEdge(newV, e.Source, e.Tag.str.[0..shift - 1], shift, e.Tag.id)*)
                  new BioGraphEdge(e.Source, e.Target, e.Tag.str.[shift..], e.Tag.length, e.Tag.id) |]
            else [|e|])
        |> Array.collect 
            (fun e -> 
                if e.Tag.length <= templateLengthHightLimit
                then 
                    //printfn "%A" e.Tag.length
                    [|e|]
                else 
                    longEdges.Add e
                    incr leCount
                    let str1 = e.Tag.str.Substring(0, templateLengthHightLimit)
                    let str2 = e.Tag.str.Substring(e.Tag.str.Length - templateLengthHightLimit)
                    incr cnt
                    let newE = !cnt
                    incr cnt
                    let newS = !cnt
                    [|new BioGraphEdge(e.Source, newE, str1, templateLengthHightLimit, e.Tag.id);BioGraphEdge(newS, e.Target, str2, templateLengthHightLimit, e.Tag.id)|]
            )
        |> Array.ofSeq
   
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
            |> Array.map (fun (x,s) -> s |> Array.ofSeq |> Array.map (fun kvp -> kvp.Key) )
            |> Array.filter(fun a -> a.Length > 1)
            |> Array.sortBy (Array.length >> ((*)-1))
            
        x
        |> Array.Parallel.map (fun vs -> vs |> Array.collect (fun v -> ug.AdjacentEdges v |> Array.ofSeq) |> (fun c -> new System.Collections.Generic.HashSet<_>(c)) |> Array.ofSeq )
        //|> Array.filter (fun x -> x.Length > 1)
    
//    let avgl = components |> Array.map (fun c -> c |> Array.averageBy (fun x -> float x.Tag.length))
//    let suml = components |> Array.map (fun c -> c |> Array.sumBy (fun x -> x.Tag.length))
//            
//    printfn "Avg %A" (avgl)
//    printfn "Sum %A" (suml)

    printfn "L %A" (Seq.length components)
    
    components
    |> Array.Parallel.map
       (fun edges -> 
         let qGraph = new QuickGraph.AdjacencyGraph<_,_>()
         qGraph.AddVerticesAndEdgeRange edges
         |> ignore
         //printfn "V = %A E = %A" qGraph.VertexCount qGraph.EdgeCount
         qGraph)
    , longEdges
            
let loadGraphFormFileToBioParserInputGraph fileWithoutExt templateLengthHightLimit tokenizer eof =
    let convert (g:QuickGraph.AdjacencyGraph<_,BioGraphEdge>) =
        let edges = 
            g.Edges 
            |> Array.ofSeq
            |> Array.Parallel.map(
                fun e -> 
                    let tag = e.Tag.str.ToCharArray() |> Array.map tokenizer
                    new BioParserEdge(e.Source, e.Target, e.Tag.length, tag)
            )
            
        new BioParserInputGraph(edges)

    let gs,longEdges = loadGraphFormFileToQG fileWithoutExt templateLengthHightLimit
    gs |> Array.Parallel.map convert
    ,longEdges