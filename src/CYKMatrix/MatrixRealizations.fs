module MatrixRealizations
    open MatrixKernels
    open Util
    open MathNet.Numerics.LinearAlgebra.Double

    //ProbabilityMatrix<float> functions
    let createEmptyMatrixProbability = ProbabilityMatrix.empty
    let matrixSetValueProbability (matrix: ProbabilityMatrix.T) (i: int) (j: int) (value: float) = matrix.InnerValue.[i*matrix.Size + j] <- value
    let toArrayProbability (matrix: ProbabilityMatrix.T) (isTranspose: bool) = matrix.GetSubArray id isTranspose matrix.WholeMatrix
    let innerSumFloat f1 f2 = f1 + f2
    let innerMultFloat f1 f2 = f1 * f2
    let innerZeroFloat = 0.0
    let innerOneFloat = 1.0
    let initMatrixProbability (graph:AbstractAnalysis.Common.SimpleInputGraph<int>) allRules nonterminals = 
        initParsingMatrix<ProbabilityMatrix.T, float> graph allRules nonterminals createEmptyMatrixProbability matrixSetValueProbability innerOneFloat
    let naiveSquareFunction matrix allRules isChanged matrixSize = naiveSquareMatrix<ProbabilityMatrix.T, float> matrixSetValueProbability toArrayProbability 
                                                                    innerSumFloat innerMultFloat innerZeroFloat innerOneFloat matrix allRules isChanged matrixSize
    let cudaSquareFunction matrix allRules isChanged matrixSize =
                    cudaSquareMatrix<ProbabilityMatrix.T> matrixSetValueProbability toArrayProbability matrix allRules isChanged matrixSize

    let managedCudaSquareFunction (matrix:ParsingMatrix<ProbabilityMatrix.T>) allRules isChanged matrixSize =
                    managedCudaSquareMatrix<ProbabilityMatrix.T> matrixSetValueProbability toArrayProbability matrix allRules isChanged matrixSize

    //Math.Net SparseMatrix<float> functions
    let createEmptyMatrixSparse size = SparseMatrix.Create(size, size, 0.0)
    let matrixSetValueSparse (matrix: SparseMatrix) (i: int) (j: int) (value: float) = matrix.At(i, j, value)
    let initMatrixSparse (graph:AbstractAnalysis.Common.SimpleInputGraph<int>) allRules nonterminals = 
        initParsingMatrix<SparseMatrix, float> graph allRules nonterminals createEmptyMatrixSparse matrixSetValueSparse innerOneFloat

    //CuSparse MySparseMatrix<float> functions
    let createEmptyMatrixMySparse size = new MySparseMatrix(size, 0, Array.init 0 (fun x -> 0.0), Array.init 0 (fun x -> 0), Array.init 0 (fun x -> 0))
    let matrixSetValueMySparse (matrix: MySparseMatrix) (i: int) (j: int) (value: float) =
        let csrVal = Array.init 1 (fun x -> 1.0)
        let csrRow = Array.init (matrix.Size + 1) (fun x -> if x < i + 1 then 0 else 1)
        let csrColInd = Array.init 1 (fun x -> j)
        let oneCellMatrix = new MySparseMatrix(matrix.Size, 1, csrVal, csrRow, csrColInd)
        let newMatrix = sparseCudaGeam matrix oneCellMatrix matrix.Size
        matrix.Update(newMatrix.Nnz, newMatrix.CsrVal, newMatrix.CsrRow, newMatrix.CsrColInd)
    let initMatrixMySparse (graph:AbstractAnalysis.Common.SimpleInputGraph<int>) allRules nonterminals =
        let initMatrix, vertexToInt = initMatrixSparse graph allRules nonterminals
        let mySparseDict = new ParsingMatrix<MySparseMatrix>()
        for nonterm in initMatrix.Keys do
            let matrix = initMatrix.[nonterm]    
            let storage = matrix.Storage :?> MathNet.Numerics.LinearAlgebra.Storage.SparseCompressedRowMatrixStorage<_>
            let csrVal = Array.copy storage.Values
            let csrRow = Array.copy storage.RowPointers
            let csrColInd = Array.copy storage.ColumnIndices
            let newMatrix = new MySparseMatrix(matrix.RowCount, matrix.NonZerosCount, csrVal, csrRow, csrColInd)   
            mySparseDict.Add(nonterm, newMatrix)

        mySparseDict, vertexToInt