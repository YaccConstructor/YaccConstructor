module YC.Bio.RNA.IO

open QuickGraph
open YC.Bio.GraphLoader
open AbstractAnalysis.Common

open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic

//let edgeToString (e:TaggedEdge<_,BioGraphEdgeLbl>) = 
//    Array.fold (fun string x -> string + numToString x) "" e.Tokens

///Seq of edges to one string 
let pathToString (path : ResizeArray<TaggedEdge<_,BioGraphEdgeLbl>>)=
    path
    |> ResizeArray.fold (fun string e -> string + e.Tag.str) ""

let getInfo prefix index = 
    ">" + prefix + index.ToString()

let printStringsToFASTA path prefix lines =
    lines
    |> Seq.mapi (fun i (sourceInfo, line) -> (getInfo prefix i) + " " + sourceInfo + "\n" + line)
    |> (fun x -> File.AppendAllLines(path, x))

let printEdgesToFASTA path numToString prefix (edges : TaggedEdge<_,BioGraphEdgeLbl> list) =
    edges
    |> List.map (fun e -> e.Tag.str)
    |> List.mapi (fun i line -> (getInfo prefix i) + line)
    |> (fun x -> File.AppendAllLines(path, x))

let printPathsToFASTA (graph : AdjacencyGraph<int,TaggedEdge<_,BioGraphEdgeLbl>>) path (res:(List<List<TaggedEdge<_,BioGraphEdgeLbl>>> * (int* int * (int * int * int) [])) []) i (shift : int) = 
    //let rec splitLine (line:string) =
    //    if line.Length <= maxLineLength then [line] else
    //    (line.Substring (0, maxLineLength))::(splitLine (line.Substring maxLineLength))
    let graphEdges = graph.Edges |> Array.ofSeq

    let printResult j ((paths:List<List<TaggedEdge<_,BioGraphEdgeLbl>>>), (startE : int, finalE : int, poss)) = 
        let prefix = "Graph" + i.ToString() + ".Subgraph" + j.ToString()+ "."
        let lenToBegEnd = new Dictionary<int,List<_>>()
        
        poss
        |> Array.iter (fun (startPos, finalPos, length) -> 
            let prefixPostfix = 
                if startE = finalE && length = 0 && startPos < finalPos
                then
                    sprintf "%i:%i -> %i:%i"
                        (startE+shift) (graphEdges.[startE].Tag.sourceStartPos + startPos)
                        (startE+shift) (graphEdges.[startE].Tag.sourceStartPos + finalPos)
                    , graphEdges.[startE].Tag.str.Substring (startPos, (finalPos - startPos + 1))
                    , ""
                else
                    sprintf "%i:%i -> %i:%i"
                        (startE+shift) (graphEdges.[startE].Tag.sourceStartPos + startPos)
                        (finalE+shift) (graphEdges.[finalE].Tag.sourceStartPos + finalPos)
                    , graphEdges.[startE].Tag.str.Substring startPos
                    , graphEdges.[finalE].Tag.str.Substring (0, finalPos + 1)

            let cond, value = lenToBegEnd.TryGetValue length
            if cond
            then
                value.Add prefixPostfix
            else
                lenToBegEnd.Add (length, new List<_>([prefixPostfix]))
            )
        let lines =
            if paths.Count = 0 then
                lenToBegEnd.Values
                |> Seq.collect (fun prefixPostfix -> 
                    prefixPostfix
                    |> Seq.collect (fun (sourceInfo, prefix, postfix) -> [sourceInfo, prefix + postfix]))
                |> Array.ofSeq
            else
                paths
                |> Seq.collect (fun path ->
                    let line = pathToString path
                    let cond, value = lenToBegEnd.TryGetValue line.Length
                    if cond
                    then
                        value
                        |> Seq.map (fun (sourceInfo, prefix, postfix) -> sourceInfo, prefix + line + postfix)
                    else
                        failwith "some length is not eq res length")
                |> Array.ofSeq

        printStringsToFASTA path prefix lines
    res
    |> Array.iteri printResult

(*
let printResult results graphNumber =
    let maxLineLength = 80
    let resultPath = ".\\result.fa"
    let rec splitLine (line:string) =
        if line.Length <= maxLineLength then [line] else
        let buff = line.Substring (0, maxLineLength)
        buff::(splitLine buff)
    results
    |> ResizeArray.iteri (fun subGraphNumber (time,strings) -> let info = ">Graph" + graphNumber.ToString() + ".Subgraph" + subGraphNumber.ToString()+ ". Time: "+ time.ToString()
                                                                let toPrint = strings |> List.collect (fun (line:string) -> splitLine line)
                                                                File.AppendAllLines(resultPath, info::toPrint))
    *)