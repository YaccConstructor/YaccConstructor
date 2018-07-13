module SparseGraphParsingImpl
    open MatrixKernels
    open MathNet.Numerics.LinearAlgebra.Double
    open AbstractAnalysis.Common
    
    let innerSumFloat f1 f2 = f1 + f2
    let innerMultFloat f1 f2 = f1 * f2
    let innerZeroFloat = 0.0
    let innerOneFloat = 1.0    

    let createEmptyMatrixSparse size = SparseMatrix.Create(size, size, innerZeroFloat)
    let matrixSetValueSparse (matrix: SparseMatrix) (i: int) (j: int) (value: float) = matrix.At(i, j, value)
    let initMatrixSparse (graph:AbstractAnalysis.Common.SimpleInputGraph<int<token>>) allRules nonterminals = 
        initParsingMatrix<SparseMatrix, float> graph allRules nonterminals createEmptyMatrixSparse matrixSetValueSparse innerOneFloat

    type SparseHandler(_matrixSize:int) =       
        interface IMatrixHandler<SparseMatrix, float> with
            member this.matrixSize = _matrixSize
            member this.createEmptyMatrix size = createEmptyMatrixSparse size
            member this.ParsingMatrixInitializator graph allRules nonterminals =
                initParsingMatrix<SparseMatrix, float> graph allRules nonterminals createEmptyMatrixSparse matrixSetValueSparse innerOneFloat
            member this.Multiply (matrix1: SparseMatrix) (matrix2: SparseMatrix) = (matrix1.Multiply(matrix2) :?> SparseMatrix)
            member this.Add matrix1 matrix2 = (matrix1.PointwiseMaximum(matrix2) :?> SparseMatrix)
            member this.Conj matrix1 matrix2 = (matrix1.PointwiseMinimum(matrix2) :?> SparseMatrix)
            member this.getNonZerosCount (matrix:SparseMatrix) = matrix.NonZerosCount
