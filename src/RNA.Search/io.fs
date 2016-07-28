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
    Seq.fold (fun string e -> string + edgeToString numToString e) "" path

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

let printPathsToFASTA path (res:seq<seq<seq<BioParserEdge>> * (BioParserEdge * BioParserEdge * _)>) i numToString = 
    //let rec splitLine (line:string) =
    //    if line.Length <= maxLineLength then [line] else
    //    (line.Substring (0, maxLineLength))::(splitLine (line.Substring maxLineLength))

    let printResult j (paths, ((startE:BioParserEdge), finalE, poss)) = 
        let prefix = "Graph" + i.ToString() + ".Subgraph" + j.ToString()+ "."
        let startfinal =
            poss
            |> Seq.map (fun (sp,fp,(l:uint16)) -> 
                            if startE = finalE then 
                                (edgeToString numToString startE).Substring (sp,(fp-sp+1)),
                                "",l
                            else 
                                (edgeToString numToString startE).Substring sp,
                                (if fp < 0 then "" else (edgeToString numToString finalE).Substring (0,fp+1)),
                                l)
        let lines =
            if Seq.length paths = 0 then
                startfinal
                |> Seq.map (fun (start,final,l) -> start + final)
                |> List.ofSeq
            else
                paths
                |> Seq.collect (fun path ->
                    startfinal
                    |> Seq.choose (fun (start,final,l) -> 
                    let line = pathToString numToString path
                    if line.Length = int l then 
                        start + line + final |> Some
                    else None))
                |> List.ofSeq

        printStringsToFASTA path prefix lines
    res
    |> Seq.iteri printResult

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