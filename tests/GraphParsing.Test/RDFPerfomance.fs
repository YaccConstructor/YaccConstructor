module YC.GraphParsing.Tests.RDFPerfomance

open VDS.RDF
open VDS.RDF.Parsing
open YC.GLL.Abstarct.Tests.RDFPerformance
open Yard.Core
open Util

open QuickGraph
open CYKMatrix
open GraphParsing
open MathNet.Numerics.LinearAlgebra.Double

//ProbabilityMatrix<float> functions
let createEmptyMatrixProbability = ProbabilityMatrix.empty
let matrixSetValueProbability (matrix: ProbabilityMatrix.T) (i: int) (j: int) (value: float) = matrix.InnerValue.[i*matrix.Size + j] <- value
let toArrayProbability (matrix: ProbabilityMatrix.T) (isTranspose: bool) = matrix.GetSubArray id isTranspose matrix.WholeMatrix
let innerSumFloat f1 f2 = f1 + f2
let innerMultFloat f1 f2 = f1 * f2
let innerZeroFloat = 0.0
let innerOneFloat = 1.0
//let naiveSquareFunction = naiveSquareMatrix<ProbabilityMatrix.T, float> matrixSetValueProbability
//                             <| toArrayProbability <| innerSumFloat <| innerMultFloat <| innerZeroFloat <| innerOneFloat
//let cudaSquareFunction = cudaSquareMatrix<ProbabilityMatrix.T> <| matrixSetValueProbability <| toArrayProbability

//Math.Net SparseMatrix<float> functions
let createEmptyMatrixSparse size = SparseMatrix.Create(size, size, 0.0)
let matrixSetValueSparse (matrix: SparseMatrix) (i: int) (j: int) (value: float) = matrix.At(i, j, value)

//CuSparse MySparseMatrix<float> functions
let createEmptyMatrixMySparse size = new MySparseMatrix(size, 0, Array.init 0 (fun x -> 0.0), Array.init 0 (fun x -> 0), Array.init 0 (fun x -> 0))
let matrixSetValueMySparse (matrix: MySparseMatrix) (i: int) (j: int) (value: float) =
    let csrVal = Array.init 1 (fun x -> 1.0)
    let csrRow = Array.init (matrix.Size + 1) (fun x -> if x < i + 1 then 0 else 1)
    let csrColInd = Array.init 1 (fun x -> j)

    let oneCellMatrix = new MySparseMatrix(matrix.Size, 1, csrVal, csrRow, csrColInd)

    let newMatrix = sparseCudaGeam matrix oneCellMatrix matrix.Size

    matrix.Update(newMatrix.Nnz, newMatrix.CsrVal, newMatrix.CsrRow, newMatrix.CsrColInd)

let tokenizer str =
    match str with
    | "SCOR" -> 1<AbstractAnalysis.Common.token>
    | "TR" -> 2<AbstractAnalysis.Common.token>
    | "OTHER" -> 3<AbstractAnalysis.Common.token>
    | "SCO" -> 4<AbstractAnalysis.Common.token>
    | "T" -> 5<AbstractAnalysis.Common.token>
    | _ -> -1<AbstractAnalysis.Common.token>

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
        getParseInputGraph tokenizer file (fun _ -> new AdjacencyGraph<_,_>())

//    printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile
    (*let cnfConv = new Conversions.ToCNF.ToCNF()
    let cnfIL = 
        {
            loadIL
                with grammar = cnfConv.ConvertGrammar (loadIL.grammar, [||])
        }*)

    
    //DenseCPU --- naive realization
    (*let start = System.DateTime.Now
    let root1 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float> g1 naiveSquareFunction loadIL
                                          tokenizer createEmptyMatrixProbability matrixSetValueProbability innerOneFloat
            parsingMatrix]
    
    let time1 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs1 = probabilityAnalyzer root1.[0]*)

    //SparseCPU --- Math.Net Numerics
    let start = System.DateTime.Now
    let root2 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<SparseMatrix, float> g1 sparseSquareMatrix2 loadIL
                                          tokenizer createEmptyMatrixSparse matrixSetValueSparse innerOneFloat
            parsingMatrix]
    let time2 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs2 = sparseAnalyzer root2.[0]

    //DenseGPU --- Alea Cuda
    (*let start = System.DateTime.Now
    let root3 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float>  g1  cudaSquareFunction  loadIL
                                          tokenizer createEmptyMatrixProbability matrixSetValueProbability innerOneFloat
            parsingMatrix]
    let time3 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs3 = probabilityAnalyzer root3.[0]*)

    //SparseGPU --- managedCuda
    (*let start = System.DateTime.Now
    let root4 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<MySparseMatrix, float>  g1  sparseCudaSquareMatrix  loadIL
                                          tokenizer createEmptyMatrixMySparse matrixSetValueMySparse innerOneFloat
            parsingMatrix]
    let time4 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs4 = mySparseAnalyzer root4.[0]*)

    //SparseCPUParallel1 --- System.Threading (2 Threads)
    (*let start = System.DateTime.Now
    let root5 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<SparseMatrix, float> g1 sparseParallelSquareMatrix loadIL
                                          tokenizer createEmptyMatrixSparse matrixSetValueSparse innerOneFloat
            parsingMatrix]
    let time5 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs5 = sparseAnalyzer root5.[0]*)
    
    //SparseCPUParallel2 --- MailBoxProcessors (any number of threads)
    let start = System.DateTime.Now
    let root6 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParseParallel<float> g1 loadIL
                                          tokenizer createEmptyMatrixSparse matrixSetValueSparse innerOneFloat
            parsingMatrix]
    let time6 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs6 = sparseAnalyzer root6.[0]

    System.IO.Path.GetFileNameWithoutExtension file, triples1, (*time1, countOfPairs1,*) time2, countOfPairs2, (*time3, countOfPairs3, time4, countOfPairs4, time5, countOfPairs5,*) time6, countOfPairs6

let performTests () =
    let basePath = @"..\..\..\data\RDF"
    let files = System.IO.Directory.GetFiles basePath 
    files 
    |> Array.map (fun rdffile -> processFile rdffile "..\..\..\GraphParsing.Test\GPPerf_Parallel1.yrd")
    |> Array.sortBy (fun (_,_,x,_,_,_(*,_,_*)) -> x)
    |> Array.iter (printfn "%A")
