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


let tokenizer str =
    match str with
    | "SCOR" -> 1
    | "TR" -> 2
    | "OTHER" -> 3
    | "SCO" -> 4
    | "T" -> 5
    | _ -> -1

let probabilityAnalyzer (matrix:Util.ProbabilityMatrix.T) =
    let mutable counter = 0
    let dataSize = matrix.Size * matrix.Size
    for ind in 0..dataSize - 1 do
        if matrix.InnerValue.[ind] > 0.0
        then
            counter <- counter + 1
    counter

let sparseAnalyzer (matrix:SparseMatrix) = matrix.NonZerosCount

let mySparseAnalyzer (matrix:MySparseMatrix) = matrix.Nnz

let processFile file grammarFile =
    let cnt = 1
    let g1, triples1 = 
        getParseInputGraph tokenizer file

//    printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile
    
    //DenseCPU --- naive realization
    (*let start = System.DateTime.Now
    let root1 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float> g1 initMatrixProbability naiveSquareFunction loadIL tokenizer
            parsingMatrix]
    
    let time1 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs1 = probabilityAnalyzer root1.[0]*)

    //SparseCPU --- Math.Net Numerics
    let start = System.DateTime.Now
    let root2 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<SparseMatrix, float> g1 initMatrixSparse sparseSquareMatrix2 loadIL tokenizer
            parsingMatrix]
    let time2 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs2 = sparseAnalyzer root2.[0]

    //DenseGPU --- Alea Cuda
    (*let start = System.DateTime.Now
    let root3 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float> g1 initMatrixProbability cudaSquareFunction loadIL tokenizer
            parsingMatrix]
    let time3 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs3 = probabilityAnalyzer root3.[0]*)

    //SparseGPU --- managedCuda
    let start = System.DateTime.Now
    let root4 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<MySparseMatrix, float> g1 initMatrixMySparse sparseCudaSquareMatrix  loadIL tokenizer
            parsingMatrix]
    let time4 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs4 = mySparseAnalyzer root4.[0]

    //SparseCPUParallel1 --- System.Threading (2 Threads)
    (*let start = System.DateTime.Now
    let root5 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<SparseMatrix, float> g1 initMatrixSparse sparseParallelSquareMatrix loadIL tokenizer
            parsingMatrix]
    let time5 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs5 = sparseAnalyzer root5.[0]*)
    
    //SparseCPUParallel2 --- MailBoxProcessors (any number of threads)
    (*let start = System.DateTime.Now
    let root6 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParseParallel<float> g1 initMatrixSparse loadIL tokenizer
            parsingMatrix]
    let time6 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs6 = sparseAnalyzer root6.[0]*)

    //DenseGPU2 --- managedCuda
    let start = System.DateTime.Now
    let root7 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float> g1 initMatrixProbability managedCudaSquareFunction loadIL tokenizer
            parsingMatrix]
    let time7 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs7 = probabilityAnalyzer root7.[0]

    System.IO.Path.GetFileNameWithoutExtension file, triples1, (*time1, countOfPairs1,*) time2, countOfPairs2, (*time3, countOfPairs3,*)
                                                 time4, countOfPairs4, (*time5, countOfPairs5, time6, countOfPairs6,*) time7, countOfPairs7

let performTests () =
    let basePath = @"..\..\..\data\RDF"
    let files = System.IO.Directory.GetFiles basePath 
    files 
    |> Array.map (fun rdffile -> processFile rdffile "..\..\..\GraphParsing.Test\GPPerf2_cnf.yrd")
    |> Array.sortBy (fun (_,_,x,_,_,_,_,_) -> x)
    |> Array.iter (printfn "%A")
