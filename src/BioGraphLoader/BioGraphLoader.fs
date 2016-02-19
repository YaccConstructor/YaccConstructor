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

let loadGraphFormFileToQG fileWithoutExt =
    let lblsExt = ".sqn"
    let graphStrauctureExt = ".grp"
    let lbls = 
        File.ReadAllLines(fileWithoutExt + lblsExt) 
        |> Seq.split 2
        |> Seq.map (fun a -> a.[0].Trim().TrimStart('>') |> int,a.[1].Trim())
        |> dict
    let edges = 
        File.ReadAllLines(fileWithoutExt + graphStrauctureExt)
        |> Seq.filter (fun s -> s.StartsWith "Edge")
        |> Seq.map (fun s -> s.Replace("Edge","").Trim([|' '; '.'|]).Split([|':'; '~'; ','|]))
        |> Seq.map (fun a -> 
            let x = (a.[1].Split '>')
            a.[0].Trim() |> int
            , x.[1].Trim([|' '; '-'|]) |> int
            , x.[1].Trim() |> int
            , (a.[2].Split '=').[1].Trim() |> int)
    let qGraph = new QuickGraph.AdjacencyGraph<_,_>()
    edges 
    |> Seq.map (fun (id,s,e,l) -> new BioGraphEdge(s,e,lbls.[id],l))
    |> qGraph.AddVerticesAndEdgeRange
    |> ignore
    qGraph


let loadGraphFormFileToOarserInputGraph fileWithoutExt tokenizer eof =
    let edg f t l = new ParserEdge<_>(f,t,l)    
    let g = loadGraphFormFileToQG fileWithoutExt    
    let finalV = System.Int32.MaxValue
    let parserInputGraph = new ParserInputGraph<_>(g.Vertices |> Array.ofSeq, [|finalV|])
    let cnt = ref (g.Vertices |> Seq.max |> ((+)1))
    g.Edges
    |> Seq.collect (fun e -> e.Tag.str.ToCharArray()
                             |> Array.mapi (fun i ch -> edg (if i = 0 then e.Source else (incr cnt ; !cnt)) (if i = e.Tag.str.Length - 1 then e.Target else (incr cnt ; !cnt)) (tokenizer ch) ))
    |> parserInputGraph.AddVerticesAndEdgeRange
    |> ignore

    g.Vertices 
    |> Seq.map (fun v -> edg v finalV eof)
    |> parserInputGraph.AddVerticesAndEdgeRange
    |> ignore

    printfn "Vert Count = %A" (!cnt)
    parserInputGraph
    
