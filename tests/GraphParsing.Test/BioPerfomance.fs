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
open ImplementationTests
open MathNet.Numerics.LinearAlgebra.Double
open BioDataPreproc
open BioDataPerformance

let BioTokenizer = (fun x -> (int) GLL.BioCFG.stringToToken.[x])

let processFile file grammarFile =
    let cnt = 1

    let graph, triples = 
        BioDataPerformance.getParseInputGraph file

    printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile
    
    //let root1, time1, countOfPairs1 = testDenseCPU cnt graph loadIL BioTokenizer
    //let root2, time2, countOfPairs2 = testSparseCPU cnt graph loadIL BioTokenizer
    //let root3, time3, countOfPairs3 = testDenseGPU1 cnt graph loadIL BioTokenizer
    //let root4, time4, countOfPairs4 = testDenseGPU2 cnt graph loadIL BioTokenizer
    let root5, time5, countOfPairs5 = testSparseGPU cnt graph loadIL BioTokenizer
    //let root6, time6, countOfPairs6 = testSparseCPUParallel cnt graph loadIL BioTokenizer 4

    System.IO.Path.GetFileNameWithoutExtension file, triples, (*time1, countOfPairs1, time2, countOfPairs2, time3, countOfPairs3,
                                                 time4, countOfPairs4,*) time5, countOfPairs5(*, time6, countOfPairs6, time7, countOfPairs7*)

let performTests () =
    //preprocBioData()
    let allTriplesFile = @"..\..\..\data\BioData\result\allTriples.txt"
    //let simpleInputFile = @"..\..\..\data\BioData\result\simpleInput.txt"    
    processFile allTriplesFile "..\..\..\GraphParsing.Test\GPPerf_Bio.yrd" |> printfn "%A"

