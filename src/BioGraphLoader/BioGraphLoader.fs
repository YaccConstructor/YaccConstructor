module YC.BIO.BioGraphLoader

open AbstractAnalysis.Common
open QuickGraph
open QuickGraph.Algorithms.KernighanLinAlgoritm

open System.IO
open Microsoft.FSharp.Collections
open System.Collections.Generic

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
    //new (_s,_l, _id) = {str=_s;length=_l;id=_id}
    new (_s, _id) = {str=_s; length=_s.Length; id=_id}

type BioGraphEdge(s,e,t) =
    inherit TaggedEdge<int, BioGraphEdgeLbl>(s,e,t)
    //new (s,e,str,l,id) = BioGraphEdge(s,e,new BioGraphEdgeLbl(str,l,id))
    new (s,e,str,id) = BioGraphEdge(s,e,new BioGraphEdgeLbl(str,id))

let bioEdgeToUndirected (e:BioGraphEdge) = 
    new TaggedUndirectedEdge<int,double>(e.Source, e.Target, 1.0)

let undirectedToBioEdge (e:TaggedEdge<int,BioGraphEdgeLbl>) = 
    new TaggedEdge<int,BioGraphEdgeLbl>(e.Source, e.Target, e.Tag)

let loadGraphFormFileToQG fileWithoutExt templateLengthHightLimit =
    let lblsExt = ".sqn"
    let graphStrauctureExt = ".grp"
    let maxComponentSize = 90000
    let lbls = 
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
        |> Array.Parallel.map (fun (id,s,e,l) -> //new BioGraphEdge(s,e,lbls.[id],l,id))
                                                   new BioGraphEdge(s,e,lbls.[id],id))
        // ЗАЧЕМ??????
        (*|> Array.collect (fun e -> 
            let shift = e.Tag.str.Length - e.Tag.length
            if shift <> 0 
            then
                incr cnt 
                let newV = !cnt
                [| new BioGraphEdge(newV, e.Source, e.Tag.str.[0..shift - 1], shift, e.Tag.id)
                ;  new BioGraphEdge(e.Source, e.Target, e.Tag.str.[shift..], e.Tag.length, e.Tag.id) |]
            else [|e|])*)
        |> Array.collect 
            (fun e -> 
                if e.Tag.length <= templateLengthHightLimit
                then 
                    //printfn "%A" e.Tag.length
                    //if e.Tag.length <= 100//templateLengthHightLimit - 100
                    (*then*) [|e|]
                    (*else deletedEdges.Add e
                         [||]*)
                else 
                    longEdges.Add e
                    incr leCount
                    let str1 = e.Tag.str.Substring(0, templateLengthHightLimit)
                    let str2 = e.Tag.str.Substring(e.Tag.length - templateLengthHightLimit)
                    incr cnt
                    let newE = !cnt
                    incr cnt
                    let newS = !cnt
                    //[|new BioGraphEdge(e.Source, newE, str1, templateLengthHightLimit, e.Tag.id);
                    //  new BioGraphEdge(newS, e.Target, str2, templateLengthHightLimit, e.Tag.id)|]
                    [|new BioGraphEdge(e.Source, newE, str1, e.Tag.id);
                      new BioGraphEdge(newS, e.Target, str2, e.Tag.id)|]
            )
   
    printfn "long edges %A" !leCount

    let divisionOnComponents (edgs1:BioGraphEdge []) =
        let ug = new UndirectedGraph<_,_>(true)
        ug.AddVerticesAndEdgeRange edgs1
        |> ignore
        let a = Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(ug)
        a.Compute()    
        a.ComponentCount |> printfn "Connected components count=%A"
        let components = 
            let x =
                a.Components
                |> Seq.groupBy(fun kvp -> kvp.Value)
                |> Array.ofSeq
                |> Array.map (fun (x,s) ->
                    s
                    |> Array.ofSeq
                    |> Array.map (fun kvp -> kvp.Key))
                |> Array.filter(fun a -> a.Length > 1)
                //|> Array.sortBy (Array.length >> ((*)-1))
                |> Array.sortBy (Array.length)
            
            x
            |> Array.Parallel.map (fun vs ->
                vs
                |> Array.collect (fun v ->
                    ug.AdjacentEdges v
                    |> Array.ofSeq)
                |> (fun c -> new HashSet<_>(c))
                |> Array.ofSeq )
            //|> Array.filter (fun x -> x.Length > 1)
        components
    
    let components = divisionOnComponents edgs
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

    components
    |> Array.Parallel.map
       (fun edges -> 
         let qGraph = new QuickGraph.AdjacencyGraph<_,_>()
         qGraph.AddVerticesAndEdgeRange edges
         |> ignore
         qGraph)
    , longEdges
    
            
let loadGraphFormFileToBioParserInputGraph fileWithoutExt templateLengthHightLimit tokenizer =
    let convert (g:AdjacencyGraph<_,BioGraphEdge>) =
        let edges =
            let EList = new ResizeArray<_>()
            for e in g.Edges do
                let tag = e.Tag.str.ToCharArray() |> Array.map tokenizer
                EList.Add(new BioParserEdge(e.Source, e.Target, e.Tag.length, tag))
            EList.ToArray()
            
        new BioParserInputGraph(edges)

    let gs,longEdges = loadGraphFormFileToQG fileWithoutExt templateLengthHightLimit
    gs |> Array.Parallel.map convert
    ,longEdges