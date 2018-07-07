module MySparseGraphParsingImpl
    open MatrixKernels
    open SparseGraphParsingImpl
    open AbstractAnalysis.Common
    
    let innerZeroFloat = 0.0
    let innerOneFloat = 1.0   

    let createEmptyMatrixMySparse size = new MySparseMatrix(size, 0, Array.init 0 (fun x -> innerZeroFloat), Array.init 0 (fun x -> 0), Array.init 0 (fun x -> 0))
    let matrixSetValueMySparse (matrix: MySparseMatrix) (i: int) (j: int) (value: float) =
        let csrVal = Array.init 1 (fun x -> innerOneFloat)
        let csrRow = Array.init (matrix.Size + 1) (fun x -> if x < i + 1 then 0 else 1)
        let csrColInd = Array.init 1 (fun x -> j)
        let oneCellMatrix = new MySparseMatrix(matrix.Size, 1, csrVal, csrRow, csrColInd)
        let newMatrix = sparseCudaGeam matrix oneCellMatrix matrix.Size
        matrix.Update(newMatrix.Nnz, newMatrix.CsrVal, newMatrix.CsrRow, newMatrix.CsrColInd)

    let initMatrixMySparse (graph:AbstractAnalysis.Common.SimpleInputGraph<int<token>>) allRules nonterminals =
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

    type MySparseHandler(_matrixSize:int) =       
        interface IMatrixHandler<MySparseMatrix, float> with
            member this.matrixSize = _matrixSize
            member this.createEmptyMatrix size = createEmptyMatrixMySparse size
            member this.ParsingMatrixInitializator graph allRules nonterminals = initMatrixMySparse graph allRules nonterminals
            member this.Multiply matrix1 matrix2 = sparseCudaGemm matrix1 matrix2 _matrixSize
            member this.Add matrix1 matrix2 = sparseCudaGeam matrix1 matrix2 _matrixSize
            member this.Conj matrix1 matrix2 = sparseCudaGeam matrix1 matrix2 _matrixSize //to do pointwiseMinimum
            member this.getNonZerosCount (matrix:MySparseMatrix) = matrix.Nnz

