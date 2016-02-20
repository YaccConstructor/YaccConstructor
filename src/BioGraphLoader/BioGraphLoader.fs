module YC.BIO.BioGraphLoader

open System.IO
open AbstractAnalysis.Common

module Seq =

    /// Returns a sequence that yields chunks of length n.
    /// Each chunk is returned as a list.
    let split length (xs: seq<'T>) =
        let rec loop xs =
            [
                yield Seq.truncate length xs |> Seq.toList
                match Seq.length xs <= length with
                | false -> yield! loop (Seq.skip length xs)
                | true -> ()
            ]
        loop xs

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
        |> Seq.split 2
        |> Seq.map (fun a -> a.[0].Trim().TrimStart('>') |> int,a.[1].Trim())
        |> dict

    let loadGraphStrucureFile repl =
        File.ReadAllLines(fileWithoutExt + graphStrauctureExt)
        |> Seq.filter (fun s -> s.StartsWith repl)
        |> Seq.map (fun s -> s.Replace(repl,"").Trim([|' '; '.'|]).Split([|':'; '~'; ','|]))
    let vertices = 
        loadGraphStrucureFile "Vertex"
        |> Seq.collect (fun a -> [a.[0].Trim() |> int; a.[1].Trim() |> int])
    let edges = 
        loadGraphStrucureFile "Edge"
        |> Seq.map (fun a -> 
            let x = (a.[1].Split '>')
            a.[0].Trim() |> int
            , x.[1].Trim([|' '; '-'|]) |> int
            , x.[1].Trim() |> int
            , (a.[2].Split '=').[1].Trim() |> int)
    let qGraph = new QuickGraph.AdjacencyGraph<_,_>()
    let cnt = ref (vertices |> Seq.max |> ((+)1))
    let edgs = 
        edges 
        |> Seq.map (fun (id,s,e,l) -> new BioGraphEdge(s,e,lbls.[id],l))
        |> Seq.collect 
            (fun e -> 
                if e.Tag.length <= 2 * templateLengthHightLimit
                then 
                    printfn "%A" e.Tag.length
                    [e]
                else 
                    let str1 = e.Tag.str.Substring(0,templateLengthHightLimit)
                    let str2 = e.Tag.str.Substring(e.Tag.str.Length-1-templateLengthHightLimit)
                    incr cnt
                    let newE = !cnt
                    incr cnt
                    let newS = !cnt
                    [new BioGraphEdge(e.Source,newE,str1,templateLengthHightLimit);BioGraphEdge(newS,e.Target,str2,templateLengthHightLimit)]
            )
        |> Array.ofSeq
    //|> qGraph.AddVerticesAndEdgeRange
    //|> ignore    

    let ug = new QuickGraph.UndirectedGraph<_,_>(true)
    ug.AddVerticesAndEdgeRange edgs
    |> ignore
    let a = QuickGraph.Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(ug)
    a.Compute()    
    a.ComponentCount |> printfn "Connected=%A"
    let components = 
        a.Components
        |> Seq.groupBy(fun kvp -> kvp.Value)
        |> Seq.map (fun (x,s) -> x, s |> Seq.map (fun kvp -> kvp.Key))
        |> Seq.sortBy (snd >> Seq.length >> ((*)-1))
        |> Seq.map (fun (_,vs) -> edgs |> Array.filter (fun e -> Seq.exists (fun v -> e.Source = v || e.Target = v) vs))

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
    let parserInputGraph = new ParserInputGraph<_>(newVMap.[53072], finalV)

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
    
