module YC.GraphParsing.Tests.AliasAndTaintPerformance

open System.IO
open System.Collections.Generic

open QuickGraph
open AbstractAnalysis.Common
open CYKMatrix
open GraphParsing
open MatrixKernels
open MathNet.Numerics.LinearAlgebra.Double
open ImplementationTests
open Yard.Core
open Yard.Core.Conversions
open Yard.Core.Conversions.CNFandBNF
open Yard.Core.IL
open Yard.Core.Helpers

let PBtokenizer (str:string) =
    let bracketKind = str.Substring(0, 2).ToUpper()
    let bracketIndex = str.Substring(2) |> int
    match str with
    | "OP" -> 2*bracketIndex + 1
    | "CP" -> -2*bracketIndex - 1
    | "OB" -> 2*bracketIndex
    | "CB" -> -2*bracketIndex
    | _ -> 0


let getTriplesFromFile file =
    let triples = new ResizeArray<int*string*int>()  
    let mutable maxParenthesis = 0
    let mutable maxBracket = 0 
    for line in System.IO.File.ReadLines(file) do
        let index = line.IndexOf('>')
        match index with
        | -1 -> ignore()
        | index ->
            let f = line.Substring(0, index - 1) |> int
            let rest = line.Substring(index + 1)
            let restIndex = rest.IndexOf('-')
            match restIndex with
            | -1 -> ignore()
            | restIndex ->
                let t = rest.Substring(0, restIndex - 10) |> int
                let edge = rest.Substring(restIndex - 2)
                let l = edge.Substring(0, String.length edge - 2)
                let bracketKind = l.Substring(0, 2).ToUpper()
                let bracketIndex = l.Substring(4) |> int
                match bracketKind with
                    | "OP" -> maxParenthesis <- max maxParenthesis bracketIndex
                    | "CP" -> maxParenthesis <- max maxParenthesis bracketIndex
                    | "OB" -> maxBracket <- max maxBracket bracketIndex
                    | "CB" -> maxBracket <- max maxBracket bracketIndex
                    | _ -> ignore()
                
                triples.Add(f,bracketKind + l.Substring(4),t)
    triples, maxParenthesis, maxBracket

let getInputGraph file =
    let triples, pMax, bMax = getTriplesFromFile file
    let mutable maxParenthesis = 0
    let mutable maxBracket = 0
    let edg f t (l: string) =
        [| new ParserEdge<_>(f, t, PBtokenizer l) |]

    let allVs = triples.ToArray() |> Array.collect (fun (f,l,t) -> [|f * 1<positionInInput>; t * 1<positionInInput>|]) |> Set.ofArray |> Array.ofSeq

    let g = new SimpleInputGraph<_>(allVs, id)
    
    [|for (f,l,t) in triples -> edg f t l |]
    |> Array.concat
    |> g.AddVerticesAndEdgeRange
    |> ignore
    
    g, triples.Count, pMax, bMax

let conversionBNFconj = new Conversions.CNFandBNF.BNFconj()

let applyConversion (conversion:Conversion) loadIL = 
    {
        loadIL
            with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
    }

let processFile file grammarFile =
    let cnt = 1
    let graph, triples, pMax, bMax = 
        getInputGraph file

    //printfn("Graph loaded")

    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let pStr = "_p" + (pMax + 1 |> string)
    let bStr = "_b" + (bMax + 1 |> string)
    let loadIL = fe.ParseGrammar (grammarFile + pStr + bStr + ".yrd")
    Namer.initNamer loadIL.grammar
    let resultIL = loadIL |> applyConversion conversionBNFconj

    //let root1, time1, countOfPairs1 = testDenseCPU cnt graph resultIL PBtokenizer 1
    //let root2, time2, countOfPairs2 = testSparseCPU cnt graph resultIL PBtokenizer 1
    //let root3, time3, countOfPairs3 = testDenseGPU1 cnt graph resultIL PBtokenizer 1
    //let root4, time4, countOfPairs4 = testDenseGPU2 cnt graph resultIL PBtokenizer 1
    let root5, time5, countOfPairs5 = testSparseGPU cnt graph resultIL PBtokenizer 1
    //let root6, time6, countOfPairs6 = testSparseCPU cnt graph resultIL PBtokenizer 2

    System.IO.Path.GetFileNameWithoutExtension file, triples, (*time1, countOfPairs1, time2, countOfPairs2, time3, countOfPairs3,
                                                 time4, countOfPairs4,*) time5, countOfPairs5(*, time6, countOfPairs6*)

let performTests () =
    let basePath = @"../../../data/AliasAndTaint"
    let files = System.IO.Directory.GetFiles basePath 
    files 
    |> Array.map (fun graphfile -> processFile graphfile @"../../../GraphParsing.Test/ConjEvaluation/pb_grammar")
    |> Array.sortBy (fun (_,_,x,_(*,_,_,_,_,_,_,_,_*)) -> x)
    |> Array.iter (printfn "%A")