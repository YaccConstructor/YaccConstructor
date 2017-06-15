module YC.GraphParsing.Tests.BioPerfomance

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
open RealizationTests
open MathNet.Numerics.LinearAlgebra.Double
open BioDataPreproc
open BioDataPerformance

let BioTokenizer = (fun x -> (int) GLL.BioCFG.stringToToken.[x])

let processFile file grammarFile =
    let cnt = 1

//    preprocBioData()

    let graph, triples = 
        BioDataPerformance.getParseInputGraph file

    printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile
    
//    let root1, time1, countOfPairs1 = testDenseCPU cnt loadIL graph BioTokenizer
//    let root2, time2, countOfPairs2 = testSparseCPU cnt loadIL graph BioTokenizer
//    let root3, time3, countOfPairs3 = testDenseGPU1 cnt loadIL graph BioTokenizer
//    let root4, time4, countOfPairs4 = testDenseGPU2 cnt loadIL graph BioTokenizer
    let root5, time5, countOfPairs5 = testSparseGPU cnt loadIL graph BioTokenizer
//    let root6, time6, countOfPairs6 = testSparseCPUParallel1 cnt loadIL graph BioTokenizer
//    let root7, time7, countOfPairs7 = testSparseCPUParallel2 cnt loadIL graph BioTokenizer

    System.IO.Path.GetFileNameWithoutExtension file, triples, (*time1, countOfPairs1, time2, countOfPairs2, time3, countOfPairs3,
                                                 time4, countOfPairs4,*) time5, countOfPairs5(*, time6, countOfPairs6, time7, countOfPairs7*)

let performTests () =
    let allTriplesFile = @"..\..\..\data\BioData\result\allTriples.txt"
    let simpleInputFile = @"..\..\..\data\BioData\result\simpleInput.txt"    
    processFile allTriplesFile "..\..\..\GraphParsing.Test\GPPerf_Bio.yrd" |> printfn "%A"

