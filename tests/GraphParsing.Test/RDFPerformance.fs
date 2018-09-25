module YC.GraphParsing.Tests.RDFPerformance

open VDS.RDF
open VDS.RDF.Parsing
open YC.GLL.Abstarct.Tests.RDFPerformance
open Yard.Core
open Util
open AbstractAnalysis.Common

open QuickGraph
open CYKMatrix
open GraphParsing
open MatrixKernels
open MathNet.Numerics.LinearAlgebra.Double
open ImplementationTests

let RDFtokenizer str =
    match str with
    | "SCOR" -> 1<token>
    | "TR" -> 2<token>
    | "OTHER" -> 3<token>
    | "SCO" -> 4<token>
    | "T" -> 5<token>
    | _ -> -1<token>

let processFile file grammarFile =
    let cnt = 1
    let graph, triples = 
        getParseInputGraph RDFtokenizer file

    //printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile

    //let root1, time1, countOfPairs1 = testDenseCPU cnt graph loadIL RDFtokenizer 1
    //let root2, time2, countOfPairs2 = testSparseCPU cnt graph loadIL RDFtokenizer 1
    //let root3, time3, countOfPairs3 = testDenseGPU1 cnt graph loadIL RDFtokenizer 1
    //let root4, time4, countOfPairs4 = testDenseGPU2 cnt graph loadIL RDFtokenizer 1
    let root5, time5, countOfPairs5 = testSparseGPU cnt graph loadIL RDFtokenizer 1
    let root6, time6, countOfPairs6 = testFastSparseGPU cnt graph loadIL RDFtokenizer
    let root7, time7, countOfPairs7 = testFastSparseSemiNaiveGPU cnt graph loadIL RDFtokenizer

    System.IO.Path.GetFileNameWithoutExtension file, triples, (*time1, countOfPairs1, time2, countOfPairs2, time3, countOfPairs3,
                                                 time4, countOfPairs4,*) time5, countOfPairs5, time6, countOfPairs6, time7, countOfPairs7

let performTests () =
    let basePath = @"../../../data/RDF"
    let files = System.IO.Directory.GetFiles basePath 
    files 
    |> Array.map (fun rdffile -> processFile rdffile @"../../../GraphParsing.Test/GPPerf1_cnf.yrd")
    |> Array.sortBy (fun (_,_,x,_,_,_,_,_) -> x)
    |> Array.iter (printfn "%A")
