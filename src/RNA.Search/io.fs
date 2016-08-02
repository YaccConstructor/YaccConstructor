module YC.Bio.RNA.IO

open YC.BIO.BioGraphLoader
open AbstractAnalysis.Common

open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic

let edgeToString numToString (e:BioParserEdge) = 
    Array.fold (fun string x -> string + numToString x) "" e.Tokens

///Seq of edges to one string 
let pathToString numToString path =
    path
    |> ResizeArray.fold (fun string e -> string + edgeToString numToString e) ""

let getInfo prefix index = 
    ">" + prefix + index.ToString() + "\n"

let printStringsToFASTA path prefix lines =
    lines
    |> Seq.mapi (fun i line -> (getInfo prefix i) + line)
    |> (fun x -> File.AppendAllLines(path, x))

let printEdgesToFASTA path numToString prefix edges =
    edges
    |> List.map (fun e -> edgeToString numToString e)
    |> List.mapi (fun i line -> (getInfo prefix i) + line)
    |> (fun x -> File.AppendAllLines(path, x))

let printPathsToFASTA path (res:(List<List<BioParserEdge>> * (BioParserEdge * BioParserEdge * (int * int * uint16) [])) []) i numToString = 
    //let rec splitLine (line:string) =
    //    if line.Length <= maxLineLength then [line] else
    //    (line.Substring (0, maxLineLength))::(splitLine (line.Substring maxLineLength))

    let printResult j ((paths:List<List<BioParserEdge>>), ((startE:BioParserEdge), finalE, poss)) = 
        let prefix = "Graph" + i.ToString() + ".Subgraph" + j.ToString()+ "."
        let startfinal =
            poss
            |> Array.map (fun (sp,fp,(l:uint16)) -> 
                            if startE = finalE then 
                                (edgeToString numToString startE).Substring (sp,(fp-sp+1)),
                                "",l
                            else 
                                (edgeToString numToString startE).Substring sp,
                                (if fp < 0 then "" else (edgeToString numToString finalE).Substring (0,fp+1)),
                                l)
        let lines =
            if paths.Count = 0 then
                startfinal |> Array.map (fun (start,final,l) -> start + final)
            else
                paths
                |> Seq.collect (fun path ->
                    startfinal
                    |> Array.map (fun (start,final,l) -> 
                        let line = pathToString numToString path
                        if line.Length = int l then 
                            start + line + final
                        else failwith "some length is not eq res length")
                    )
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