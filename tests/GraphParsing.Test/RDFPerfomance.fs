module YC.GraphParsing.Tests.RDFPerfomance

open VDS.RDF
open VDS.RDF.Parsing
open YC.GLL.Abstarct.Tests.RDFPerformance
open Yard.Core
open Util

open QuickGraph
open CYKMatrix
open GraphParsing
open MatrixKernels
open MatrixRealizations
open MathNet.Numerics.LinearAlgebra.Double
open RealizationTests

let RDFtokenizer str =
    match str with
    | "SCOR" -> 1
    | "TR" -> 2
    | "OTHER" -> 3
    | "SCO" -> 4
    | "T" -> 5
    | _ -> -1

let processFile file grammarFile =
    let cnt = 1
    let graph, triples = 
        getParseInputGraph RDFtokenizer file

//    printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile

//    let root1, time1, countOfPairs1 = testDenseCPU cnt loadIL graph RDFtokenizer
    let root2, time2, countOfPairs2 = testSparseCPU cnt loadIL graph RDFtokenizer
    let root3, time3, countOfPairs3 = testDenseGPU1 cnt loadIL graph RDFtokenizer
//    let root4, time4, countOfPairs4 = testDenseGPU2 cnt loadIL graph RDFtokenizer
    let root5, time5, countOfPairs5 = testSparseGPU cnt loadIL graph RDFtokenizer
//    let root6, time6, countOfPairs6 = testSparseCPUParallel1 cnt loadIL graph RDFtokenizer
//    let root7, time7, countOfPairs7 = testSparseCPUParallel2 cnt loadIL graph RDFtokenizer

    System.IO.Path.GetFileNameWithoutExtension file, triples, (*time1, countOfPairs1,*) time2, countOfPairs2, time3, countOfPairs3,
                                                 (*time4, countOfPairs4,*) time5, countOfPairs5(*, time6, countOfPairs6, time7, countOfPairs7*)

let performTests () =
    let basePath = @"..\..\..\data\RDF"
    let files = System.IO.Directory.GetFiles basePath 
    files 
    |> Array.map (fun rdffile -> processFile rdffile "..\..\..\GraphParsing.Test\GPPerf2_cnf.yrd")
    |> Array.sortBy (fun (_,_,x,_,_,_,_,_) -> x)
    |> Array.iter (printfn "%A")
