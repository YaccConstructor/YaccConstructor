module YC.Bio.RNA.IO

open QuickGraph
open YC.Bio.GraphLoader
open AbstractAnalysis.Common

open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic

[<Struct>]
type InfernalData =
    val TargetName : string
    val Accession1 : Option<string>
    val QueryName : string
    val Accession2 : Option<string>
    val ModelType : string
    val ModelFrom : int 
    val ModelTo : int
    val SeqFrom : int
    val SeqTo : int
    val Strand : bool
    val Trunc : string
    val Pass : int
    val Gc : float
    val Bias : float 
    val Score : float
    val E_value : double
    val Inc : bool
    val DescriptionOfTarget : Option<string>

    new (tName, accession1, queryName, accession2, mdl, mdlFrom, mdlTo, seqFrom, seqTo, strand, trunc, pass, gc, bias, score, e_value, inc, descr) =
        {
            TargetName = tName
            Accession1 = accession1
            QueryName = queryName
            Accession2 = accession2
            ModelType = mdl
            ModelFrom = mdlFrom
            ModelTo = mdlTo
            SeqFrom = seqFrom
            SeqTo = seqTo
            Strand = strand
            Trunc = trunc
            Pass = pass
            Gc = gc
            Bias = bias
            Score = score
            E_value = e_value
            Inc = inc
            DescriptionOfTarget = descr
        }

let readInfernalout file =
    let strToOpt s = if s = "-" then None else Some s
    System.IO.File.ReadAllLines file
    |> Seq.filter (fun s -> s.StartsWith "#" |> not)
    |> Seq.map (fun s -> s.Split ' '|> Array.filter ((<>)""))
    |> Seq.map (fun a -> 
        new InfernalData (
            a.[0],
            strToOpt a.[1],
            a.[2],
            strToOpt a.[3],
            a.[4],
            int a.[5],
            int a.[6],
            int a.[7],
            int a.[8],
            a.[9] = "+",
            a.[10],
            int a.[11],
            float a.[12],
            float a.[13],
            float a.[14],
            double a.[15],
            a.[16] = "!",
            strToOpt (a.[17..] |> String.concat " ")
            ) )
        |> Array.ofSeq
