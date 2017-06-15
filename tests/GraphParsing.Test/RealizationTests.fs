module RealizationTests
    open MathNet.Numerics.LinearAlgebra.Double
    open MatrixKernels
    open MatrixRealizations
    open GraphParsing
    open Util

//ProbabilityMatrix.T

    let graphParsingPrint (matrix: ProbabilityMatrix.T) =
        let rowLength = matrix.Nrow
        let colLength = matrix.Ncol
        for i in [ 0..rowLength - 1 ] do
            for j in [ 0..colLength - 1 ] do
                let cell = Cell.create i j
                printf "%.1f  " <| Probability.unwrap matrix.[cell]
            printfn ""
        printfn ""

    let probabilityAnalyzer (matrix:Util.ProbabilityMatrix.T) =
        let mutable counter = 0
        let dataSize = matrix.Size * matrix.Size
        for ind in 0..dataSize - 1 do
            if matrix.InnerValue.[ind] > 0.0
            then
                counter <- counter + 1
        counter

    //DenseCPU --- naive realization
    let testDenseCPU cnt loadIL graph tokenizer =
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float> graph initMatrixProbability naiveSquareFunction loadIL tokenizer
                parsingMatrix]
    
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = probabilityAnalyzer root.[0]
        root, time, countOfPairs

    //DenseGPU1 --- managedCuda
    let testDenseGPU1 cnt loadIL graph tokenizer=
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float> graph initMatrixProbability managedCudaSquareFunction loadIL tokenizer
                parsingMatrix]
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = probabilityAnalyzer root.[0]
        root, time, countOfPairs

    //DenseGPU2 --- Alea Cuda
    let testDenseGPU2 cnt loadIL graph tokenizer =
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float> graph initMatrixProbability cudaSquareFunction loadIL tokenizer
                parsingMatrix]
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = probabilityAnalyzer root.[0]
        root, time, countOfPairs

//SparseMatrix (MathNet Numerics)

    let sparsePrint (matrix: SparseMatrix) =
        for i in 0..(matrix.RowCount - 1) do
            for j in 0..(matrix.ColumnCount - 1) do
                printf "%.1f  " <| matrix.At(i, j)
            printfn ""
        printfn ""

    let sparseAnalyzer (matrix:SparseMatrix) = matrix.NonZerosCount

    //SparseCPU --- Math.Net Numerics
    let testSparseCPU cnt loadIL graph tokenizer =
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, _, _) = graphParse<SparseMatrix, float> graph initMatrixSparse sparseSquareMatrix2 loadIL tokenizer
                parsingMatrix]
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = sparseAnalyzer root.[0]
        root, time, countOfPairs

    //SparseCPUParallel1 --- System.Threading (2 Threads)
    let testSparseCPUParallel1 cnt loadIL graph tokenizer =
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, _, _) = graphParse<SparseMatrix, float> graph initMatrixSparse sparseParallelSquareMatrix loadIL tokenizer
                parsingMatrix]
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = sparseAnalyzer root.[0]
        root, time, countOfPairs

    //SparseCPUParallel2 --- MailBoxProcessors (any number of threads)
    let testSparseCPUParallel2 cnt loadIL graph tokenizer =
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, _, _) = graphParseParallel<float> graph initMatrixSparse loadIL tokenizer
                parsingMatrix]
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = sparseAnalyzer root.[0]
        root, time, countOfPairs


    let MySparsePrint (matrix: MySparseMatrix) = 
        printfn "CsrVal: %A" matrix.CsrVal
        printfn "CsrColInd: %A" matrix.CsrColInd
        printfn "CsrRow: %A" matrix.CsrRow


//MySparseMatrix

    let mySparseAnalyzer (matrix:MySparseMatrix) = matrix.Nnz

    //SparseGPU --- managedCuda
    let testSparseGPU cnt loadIL graph tokenizer =
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, _, _) = graphParse<MySparseMatrix, float> graph initMatrixMySparse sparseCudaSquareMatrix loadIL tokenizer
                parsingMatrix]
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = mySparseAnalyzer root.[0]
        root, time, countOfPairs
