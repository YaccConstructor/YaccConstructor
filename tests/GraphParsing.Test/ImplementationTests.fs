module ImplementationTests
    open MathNet.Numerics.LinearAlgebra.Double
    open MatrixKernels
    open ProbabilityGraphParsingImpl
    open SparseGraphParsingImpl
    open MySparseGraphParsingImpl
    open GraphParsing
    open Util

    let graphParsingTest<'MatrixType, 'InnerType when 'InnerType : comparison> cnt graph mHandler loadIL tokenizer parallelProcesses resultAnalyzer =
        let S = ref (NonTerminal "")
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, StartNonTerm, _, _) = graphParse<'MatrixType, 'InnerType> graph mHandler loadIL tokenizer parallelProcesses
                S := StartNonTerm
                parsingMatrix]
    
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = resultAnalyzer root.[0] !S
        root.[0], time, countOfPairs

//ProbabilityMatrix.T

    let probabilityMatrixPrint (matrix: ProbabilityMatrix.T) =
        let rowLength = matrix.Nrow
        let colLength = matrix.Ncol
        for i in [ 0..rowLength - 1 ] do
            for j in [ 0..colLength - 1 ] do
                let cell = Cell.create i j
                printf "%.1f  " <| Probability.unwrap matrix.[cell]
            printfn ""
        printfn ""

    let probabilityAnalyzer (parsingMatrix:ParsingMatrix<Util.ProbabilityMatrix.T>) (S:NonTerminal) =
        let startMatrix = parsingMatrix.[S]
        let mutable counter = 0
        let dataSize = startMatrix.Size * startMatrix.Size
        for ind in 0..dataSize - 1 do
            if startMatrix.InnerValue.[ind] > 0.0
            then
                counter <- counter + 1
        counter

    //DenseCPU --- naive realization
    let testDenseCPU cnt graph loadIL tokenizer numberOfThreads =
        graphParsingTest<ProbabilityMatrix.T, float> cnt graph (new ProbabilityNaiveHandler(graph.VertexCount)) loadIL tokenizer numberOfThreads probabilityAnalyzer 

    //DenseGPU1 --- managedCuda
    let testDenseGPU1 cnt graph loadIL tokenizer numberOfThreads =
        graphParsingTest<ProbabilityMatrix.T, float> cnt graph (new ProbabilityManagedCudaHandler(graph.VertexCount)) loadIL tokenizer numberOfThreads probabilityAnalyzer 

    //DenseGPU2 --- Alea Cuda
    let testDenseGPU2 cnt graph loadIL tokenizer numberOfThreads =
        graphParsingTest<ProbabilityMatrix.T, float> cnt graph (new ProbabilityAleaCudaHandler(graph.VertexCount)) loadIL tokenizer numberOfThreads probabilityAnalyzer 

//SparseMatrix (MathNet Numerics)

    let sparseMatrixPrint (matrix: SparseMatrix) =
        for i in 0..(matrix.RowCount - 1) do
            for j in 0..(matrix.ColumnCount - 1) do
                printf "%.1f  " <| matrix.At(i, j)
            printfn ""
        printfn ""

    let sparseAnalyzer (parsingMatrix:ParsingMatrix<SparseMatrix>) (S:NonTerminal) = parsingMatrix.[S].NonZerosCount

    //SparseCPU --- Math.Net Numerics
    let testSparseCPU cnt graph loadIL tokenizer numberOfThreads =
        graphParsingTest<SparseMatrix, float> cnt graph (new SparseHandler(graph.VertexCount)) loadIL tokenizer numberOfThreads sparseAnalyzer

//MySparseMatrix

    let MySparsePrint (matrix: MySparseMatrix) = 
        printfn "CsrVal: %A" matrix.CsrVal
        printfn "CsrColInd: %A" matrix.CsrColInd
        printfn "CsrRow: %A" matrix.CsrRow

    let mySparseAnalyzer (parsingMatrix:ParsingMatrix<MySparseMatrix>) (S:NonTerminal) = parsingMatrix.[S].Nnz

    //SparseGPU --- managedCuda
    let testSparseGPU cnt graph loadIL tokenizer numberOfThreads =
        graphParsingTest<MySparseMatrix, float> cnt graph (new MySparseHandler(graph.VertexCount)) loadIL tokenizer numberOfThreads mySparseAnalyzer

    //FastSparseGPU --- managedCuda, minimum data transitions
    let testFastSparseGPU cnt graph loadIL tokenizer =
        let S = ref (NonTerminal "")
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, StartNonTerm, _, _) = graphParseGPU graph loadIL tokenizer
                S := StartNonTerm
                parsingMatrix]
    
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = mySparseAnalyzer root.[0] !S
        root.[0], time, countOfPairs