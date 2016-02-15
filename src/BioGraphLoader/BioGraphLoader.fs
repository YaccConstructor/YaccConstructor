module YC.BIO.BioGraphLoader

open System.IO

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

let lodGraph fileWithoutExt =
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


