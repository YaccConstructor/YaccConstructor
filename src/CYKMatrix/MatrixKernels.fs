module MatrixKernels
    open Util
    open TPMatrices
    open System
    open System.Collections.Generic
    open Yard.Core
    open Yard.Core.IL
    open Yard.Core.Helpers
    open Conversions.TransformAux
    open QuickGraph
    open Alea.CUDA
    open Alea.CUDA.CULib
    open Alea.CUDA.Utilities
    open MathNet.Numerics.LinearAlgebra.Double
    open ManagedCuda
    open ManagedCuda.CudaSparse
    open ManagedCuda.BasicTypes


    type ParsingMatrix<'MatrixType> = Dictionary<NonTerminal, 'MatrixType>

    type IMatrixHandler<'MatrixType, 'InnerType when 'InnerType : comparison> =
        abstract matrixSize : int
        abstract createEmptyMatrix : int -> 'MatrixType
        abstract ParsingMatrixInitializator : AbstractAnalysis.Common.SimpleInputGraph<int> -> BooleanRulesHolder -> ResizeArray<NonTerminal> -> (ParsingMatrix<'MatrixType> * Dictionary<int,int>)
        abstract Multiply : 'MatrixType -> 'MatrixType -> 'MatrixType
        abstract Add : 'MatrixType -> 'MatrixType -> 'MatrixType
        abstract Conj : 'MatrixType -> 'MatrixType -> 'MatrixType
        abstract getNonZerosCount : 'MatrixType -> int
         

    type MySparseMatrix(size : int, nnz : int, csrVal : float[], csrRow : int[], csrColInd : int[]) =
        let mutable _Nnz = nnz
        let mutable _CsrVal = csrVal
        let mutable _CsrRow = csrRow
        let mutable _CsrColInd = csrColInd
        member this.Size = size
        member this.Nnz = _Nnz
        member this.CsrVal = _CsrVal
        member this.CsrRow = _CsrRow
        member this.CsrColInd = _CsrColInd
        member this.Update (nnz_upd : int, csrVal_upd : float[], csrRow_upd : int[], csrColInd_upd : int[]) = 
            _Nnz <- nnz_upd
            _CsrVal <- csrVal_upd
            _CsrRow <- csrRow_upd
            _CsrColInd <- csrColInd_upd

    let initParsingMatrix<'MatrixType, 'InnerType when 'InnerType : comparison> (graph:AbstractAnalysis.Common.SimpleInputGraph<int>)
                  (allRules: BooleanRulesHolder)
                  nonterminals
                  createEmptyMatrix 
                  (matrixSetValue : 'MatrixType -> int -> int -> 'InnerType -> unit) 
                  (innerOne: 'InnerType) =
        let vertexToInt = new Dictionary<_,_>()
        let mutable procVertices = 0
        let parsingMatrix = new ParsingMatrix<'MatrixType> ()
        do 
            (
                nonterminals 
                |> Seq.map (fun x -> x, createEmptyMatrix (graph.VertexCount))
            )
            |> Seq.iter parsingMatrix.Add

        for vertex in graph.Vertices do
            vertexToInt.Add(vertex, procVertices)
            procVertices <- procVertices + 1

        for edg in graph.Edges do
            let label = edg.Tag
            if allRules.IsSimpleTail label
            then
                let simpleNonterminals = allRules.HeadsBySimpleTail label
                for (simpleNonterminal, _) in simpleNonterminals do
                    let row = vertexToInt.[edg.Source]
                    let col = vertexToInt.[edg.Target]
                    matrixSetValue parsingMatrix.[simpleNonterminal] row col innerOne
            //System.GC.Collect()

        parsingMatrix, vertexToInt
    
    let aleaCudaMultArrays (from1: float []) (from2: float []) matrixSize (mult1:DeviceMemory<float>) (mult2:DeviceMemory<float>) (result:DeviceMemory<float>) =
        let transa = cublasOperation_t.CUBLAS_OP_N
        let transb = cublasOperation_t.CUBLAS_OP_N

        let dalpha = 1.
        let dbeta = 0.

        mult1.Scatter(from1)
        mult2.Scatter(from2)

        CUBLAS.Default.Dgemm(transa, transb, matrixSize, matrixSize, matrixSize, 
                                dalpha, mult2.Ptr, matrixSize, mult1.Ptr, matrixSize, 
                                dbeta, result.Ptr, matrixSize) // mult1 and mult2 swaped because Dgemm expect column-major matrices

        let resultArr = result.Gather()  
        resultArr

    
    let managedCudaMultArrays (from1: float []) (from2: float []) (matrixSize:int) (refhandle:CudaBlas.CudaBlasHandle ref)(mult1:CudaDeviceVariable<float>) (mult2:CudaDeviceVariable<float>) (result:CudaDeviceVariable<float>) =

        let transa = CudaBlas.Operation.NonTranspose
        let transb = CudaBlas.Operation.NonTranspose

        let alpha_ref = ref 1.0
        let beta_ref = ref 0.0

        mult1.CopyToDevice(from1)
        mult2.CopyToDevice(from2)
        
        CudaBlas.CudaBlasNativeMethods.cublasDgemm_v2(!refhandle, transa, transb, matrixSize, matrixSize, matrixSize,
                                                            alpha_ref, mult2.DevicePointer, matrixSize,
                                                            mult1.DevicePointer, matrixSize,
                                                            beta_ref, result.DevicePointer, matrixSize) |> ignore
            
        let resultArr = Array.init (matrixSize * matrixSize) (fun x -> 0.0)   
        result.CopyToHost(resultArr)
        resultArr

    let sparseCudaGemm (matrix1 : MySparseMatrix) (matrix2 : MySparseMatrix) matrixSize =
        let nnzA = matrix1.Nnz
        let nnzB = matrix2.Nnz

        if (nnzA = 0 || nnzB = 0)
        then
            new MySparseMatrix(matrixSize, 0, Array.init 0 (fun x -> 0.0), Array.init 0 (fun x -> 0), Array.init 0 (fun x -> 0))
        else

            let csrValA = matrix1.CsrVal
            let csrRowA = matrix1.CsrRow
            let csrColIndA = matrix1.CsrColInd
            let csrValB = matrix2.CsrVal
            let csrRowB = matrix2.CsrRow
            let csrColIndB = matrix2.CsrColInd

            let sparsecntx = new cusparseContext()
            let mutable refcnt = ref sparsecntx
            CudaSparseNativeMethods.cusparseCreate(refcnt) |> ignore

            let transa = cusparseOperation.NonTranspose
            let transb = cusparseOperation.NonTranspose
            let descrA = new cusparseMatDescr()      
            let descrB = new cusparseMatDescr()       
            let descrC = new cusparseMatDescr()       
            let refdescrA = ref descrA
            CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrA) |> ignore
            let refdescrB = ref descrB
            CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrB) |> ignore
            let refdescrC = ref descrC
            CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrC) |> ignore

            let mutable csrValPtrA : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(nnzA))
            csrValPtrA.CopyToDevice(csrValA)
            let mutable csrRowPtrA : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
            csrRowPtrA.CopyToDevice(csrRowA)
            let mutable csrColIndPtrA : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(nnzA))
            csrColIndPtrA.CopyToDevice(csrColIndA)
            let mutable csrValPtrB : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(nnzB))
            csrValPtrB.CopyToDevice(csrValB)
            let mutable csrRowPtrB : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
            csrRowPtrB.CopyToDevice(csrRowB)
            let mutable csrColIndPtrB : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(nnzB))
            csrColIndPtrB.CopyToDevice(csrColIndB)

            let mutable csrRowPtrC : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
            let mutable nnzTotalDevHostPtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(1))

            let nnzC = ref 0

            let status1 = CudaSparseNativeMethods.cusparseXcsrgemmNnz(!refcnt, transa, transb, matrixSize, matrixSize, matrixSize,
                                                    !refdescrA, nnzA, csrRowPtrA.DevicePointer, csrColIndPtrA.DevicePointer,
                                                    !refdescrB, nnzB, csrRowPtrB.DevicePointer, csrColIndPtrB.DevicePointer,
                                                    !refdescrC, csrRowPtrC.DevicePointer, nnzTotalDevHostPtr.DevicePointer)


            nnzTotalDevHostPtr.CopyToHost(nnzC)

            if (!nnzC = 0)
            then               
                csrValPtrA.Dispose()
                csrRowPtrA.Dispose()
                csrColIndPtrA.Dispose()
                csrValPtrB.Dispose()
                csrRowPtrB.Dispose()
                csrColIndPtrB.Dispose()
                nnzTotalDevHostPtr.Dispose()
                let resultMatrix = new MySparseMatrix(matrixSize, 0, Array.init 0 (fun x -> 0.0), Array.init 0 (fun x -> 0), Array.init 0 (fun x -> 0))
                resultMatrix
            else
                let csrRowC = Array.init (matrixSize + 1) (fun x -> 0)
                csrRowPtrC.CopyToHost(csrRowC)

                let mutable csrColIndPtrC : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(!nnzC))
                let mutable csrValPtrC : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(!nnzC))
        
                let status2 = CudaSparseNativeMethods.cusparseDcsrgemm(!refcnt, transa, transb, matrixSize, matrixSize, matrixSize,
                                                        !refdescrA, nnzA, csrValPtrA.DevicePointer, csrRowPtrA.DevicePointer, csrColIndPtrA.DevicePointer,
                                                        !refdescrB, nnzB, csrValPtrB.DevicePointer, csrRowPtrB.DevicePointer, csrColIndPtrB.DevicePointer,
                                                        !refdescrC, csrValPtrC.DevicePointer, csrRowPtrC.DevicePointer, csrColIndPtrC.DevicePointer)

        
                let csrValC = Array.init !nnzC (fun x -> 0.0)        
                let csrColIndC = Array.init !nnzC (fun x -> 0)

                csrValPtrC.CopyToHost(csrValC)
                csrColIndPtrC.CopyToHost(csrColIndC)

                csrValPtrA.Dispose()
                csrRowPtrA.Dispose()
                csrColIndPtrA.Dispose()
                csrValPtrB.Dispose()
                csrRowPtrB.Dispose()
                csrColIndPtrB.Dispose()
                csrValPtrC.Dispose()
                csrRowPtrC.Dispose()
                csrColIndPtrC.Dispose()
                nnzTotalDevHostPtr.Dispose()

                let resultMatrix = new MySparseMatrix(matrixSize, !nnzC, csrValC, csrRowC, csrColIndC)
                resultMatrix

    let sparseCudaGeam (matrix1 : MySparseMatrix) (matrix2 : MySparseMatrix) matrixSize =
        let nnzA = matrix1.Nnz
        let nnzB = matrix2.Nnz

        if (nnzA = 0)
        then
            let resultMatrix = new MySparseMatrix(matrixSize, 0, Array.init 0 (fun x -> 0.0), Array.init 0 (fun x -> 0), Array.init 0 (fun x -> 0))
            resultMatrix.Update(matrix2.Nnz, matrix2.CsrVal, matrix2.CsrRow, matrix2.CsrColInd)
            resultMatrix
        else
            if (nnzB = 0)
            then
                let resultMatrix = new MySparseMatrix(matrixSize, 0, Array.init 0 (fun x -> 0.0), Array.init 0 (fun x -> 0), Array.init 0 (fun x -> 0))
                resultMatrix.Update(matrix1.Nnz, matrix1.CsrVal, matrix1.CsrRow, matrix1.CsrColInd)
                resultMatrix
            else
                let csrValA = matrix1.CsrVal
                let csrRowA = matrix1.CsrRow
                let csrColIndA = matrix1.CsrColInd
                let csrValB = matrix2.CsrVal
                let csrRowB = matrix2.CsrRow
                let csrColIndB = matrix2.CsrColInd

                let sparsecntx = new cusparseContext()
                let mutable refcnt = ref sparsecntx
                CudaSparseNativeMethods.cusparseCreate(refcnt) |> ignore

                let transa = cusparseOperation.NonTranspose
                let transb = cusparseOperation.NonTranspose
                let descrA = new cusparseMatDescr()      
                let descrB = new cusparseMatDescr()       
                let descrC = new cusparseMatDescr()       
                let refdescrA = ref descrA
                CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrA) |> ignore
                let refdescrB = ref descrB
                CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrB) |> ignore
                let refdescrC = ref descrC
                CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrC) |> ignore

                let mutable csrValPtrA : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(nnzA))
                csrValPtrA.CopyToDevice(csrValA)
                let mutable csrRowPtrA : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
                csrRowPtrA.CopyToDevice(csrRowA)
                let mutable csrColIndPtrA : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(nnzA))                
                csrColIndPtrA.CopyToDevice(csrColIndA)
                let mutable csrValPtrB : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(nnzB))
                csrValPtrB.CopyToDevice(csrValB)
                let mutable csrRowPtrB : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
                csrRowPtrB.CopyToDevice(csrRowB)
                let mutable csrColIndPtrB : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(nnzB))
                csrColIndPtrB.CopyToDevice(csrColIndB)

                let mutable csrRowPtrC : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
                let mutable nnzTotalDevHostPtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(1))
                

                let nnzC = ref 0

                let status1 = CudaSparseNativeMethods.cusparseXcsrgeamNnz(!refcnt, matrixSize, matrixSize,
                                                        !refdescrA, nnzA, csrRowPtrA.DevicePointer, csrColIndPtrA.DevicePointer,
                                                        !refdescrB, nnzB, csrRowPtrB.DevicePointer, csrColIndPtrB.DevicePointer,
                                                        !refdescrC, csrRowPtrC.DevicePointer, nnzTotalDevHostPtr.DevicePointer)

                nnzTotalDevHostPtr.CopyToHost(nnzC)

                let csrRowC = Array.init (matrixSize + 1) (fun x -> 0)
                csrRowPtrC.CopyToHost(csrRowC)

                let mutable csrColIndPtrC : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(!nnzC))
                let mutable csrValPtrC : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(!nnzC))

                let alpha_ref = ref 1.0
                let beta_ref = ref 1.0

                let status2 = CudaSparseNativeMethods.cusparseDcsrgeam(!refcnt, matrixSize, matrixSize, alpha_ref,
                                                        !refdescrA, nnzA, csrValPtrA.DevicePointer, csrRowPtrA.DevicePointer, csrColIndPtrA.DevicePointer,
                                                        beta_ref, !refdescrB, nnzB, csrValPtrB.DevicePointer, csrRowPtrB.DevicePointer, csrColIndPtrB.DevicePointer,
                                                        !refdescrC, csrValPtrC.DevicePointer, csrRowPtrC.DevicePointer, csrColIndPtrC.DevicePointer)

        
                let csrValC = Array.init !nnzC (fun x -> 0.0)        
                let csrColIndC = Array.init !nnzC (fun x -> 0)
                
                csrValPtrC.CopyToHost(csrValC)
                csrColIndPtrC.CopyToHost(csrColIndC)

                csrValPtrA.Dispose()
                csrRowPtrA.Dispose()
                csrColIndPtrA.Dispose()
                csrValPtrB.Dispose()
                csrRowPtrB.Dispose()
                csrColIndPtrB.Dispose()
                csrValPtrC.Dispose()
                csrRowPtrC.Dispose()
                csrColIndPtrC.Dispose()
                nnzTotalDevHostPtr.Dispose()

                let resultMatrix = new MySparseMatrix(matrixSize, !nnzC, csrValC, csrRowC, csrColIndC)
                resultMatrix 
                
    
    let fastSparseCudaGemm (matrix1:int * CudaDeviceVariable<float> * CudaDeviceVariable<int> * CudaDeviceVariable<int>)
                            (matrix2:int * CudaDeviceVariable<float> * CudaDeviceVariable<int> * CudaDeviceVariable<int>)
                            matrixSize refcnt fakeDeviceMatrix =
        let (nnzA, csrValPtrA, csrRowPtrA, csrColIndPtrA) = matrix1
        let (nnzB, csrValPtrB, csrRowPtrB, csrColIndPtrB) = matrix2
        if nnzA = 0 || nnzB = 0 then
            fakeDeviceMatrix
        else
            let transa = cusparseOperation.NonTranspose
            let transb = cusparseOperation.NonTranspose
            let descrA = new cusparseMatDescr()      
            let descrB = new cusparseMatDescr()       
            let descrC = new cusparseMatDescr()       
            let refdescrA = ref descrA
            CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrA) |> ignore
            let refdescrB = ref descrB
            CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrB) |> ignore
            let refdescrC = ref descrC
            CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrC) |> ignore

            let mutable csrRowPtrC : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
            let mutable nnzTotalDevHostPtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(1))

            let nnzC = ref 0

            let status1 = CudaSparseNativeMethods.cusparseXcsrgemmNnz(!refcnt, transa, transb, matrixSize, matrixSize, matrixSize,
                                                    !refdescrA, nnzA, csrRowPtrA.DevicePointer, csrColIndPtrA.DevicePointer,
                                                    !refdescrB, nnzB, csrRowPtrB.DevicePointer, csrColIndPtrB.DevicePointer,
                                                    !refdescrC, csrRowPtrC.DevicePointer, nnzTotalDevHostPtr.DevicePointer)


            nnzTotalDevHostPtr.CopyToHost(nnzC)

            if (!nnzC = 0)
            then               
                nnzTotalDevHostPtr.Dispose()
                fakeDeviceMatrix
            else
                let mutable csrColIndPtrC : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(!nnzC))
                let mutable csrValPtrC : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(!nnzC))
        
                let status2 = CudaSparseNativeMethods.cusparseDcsrgemm(!refcnt, transa, transb, matrixSize, matrixSize, matrixSize,
                                                        !refdescrA, nnzA, csrValPtrA.DevicePointer, csrRowPtrA.DevicePointer, csrColIndPtrA.DevicePointer,
                                                        !refdescrB, nnzB, csrValPtrB.DevicePointer, csrRowPtrB.DevicePointer, csrColIndPtrB.DevicePointer,
                                                        !refdescrC, csrValPtrC.DevicePointer, csrRowPtrC.DevicePointer, csrColIndPtrC.DevicePointer)

                nnzTotalDevHostPtr.Dispose()
                !nnzC, csrValPtrC, csrRowPtrC, csrColIndPtrC



    let fastSparseCudaGeam (matrix1:int * CudaDeviceVariable<float> * CudaDeviceVariable<int> * CudaDeviceVariable<int>)
                            (matrix2:int * CudaDeviceVariable<float> * CudaDeviceVariable<int> * CudaDeviceVariable<int>)
                            matrixSize refcnt =
        let (nnzA, csrValPtrA, csrRowPtrA, csrColIndPtrA) = matrix1
        let (nnzB, csrValPtrB, csrRowPtrB, csrColIndPtrB) = matrix2

        let transa = cusparseOperation.NonTranspose
        let transb = cusparseOperation.NonTranspose
        let descrA = new cusparseMatDescr()      
        let descrB = new cusparseMatDescr()       
        let descrC = new cusparseMatDescr()       
        let refdescrA = ref descrA
        CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrA) |> ignore
        let refdescrB = ref descrB
        CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrB) |> ignore
        let refdescrC = ref descrC
        CudaSparseNativeMethods.cusparseCreateMatDescr(refdescrC) |> ignore

        let mutable csrRowPtrC : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
        let mutable nnzTotalDevHostPtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(1))
        
        let nnzC = ref 0

        let status1 = CudaSparseNativeMethods.cusparseXcsrgeamNnz(!refcnt, matrixSize, matrixSize,
                                                !refdescrA, nnzA, csrRowPtrA.DevicePointer, csrColIndPtrA.DevicePointer,
                                                !refdescrB, nnzB, csrRowPtrB.DevicePointer, csrColIndPtrB.DevicePointer,
                                                !refdescrC, csrRowPtrC.DevicePointer, nnzTotalDevHostPtr.DevicePointer)

        nnzTotalDevHostPtr.CopyToHost(nnzC)

        let mutable csrColIndPtrC : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(!nnzC))
        let mutable csrValPtrC : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(!nnzC))

        let alpha_ref = ref 1.0
        let beta_ref = ref 1.0

        let status2 = CudaSparseNativeMethods.cusparseDcsrgeam(!refcnt, matrixSize, matrixSize, alpha_ref,
                                                !refdescrA, nnzA, csrValPtrA.DevicePointer, csrRowPtrA.DevicePointer, csrColIndPtrA.DevicePointer,
                                                beta_ref, !refdescrB, nnzB, csrValPtrB.DevicePointer, csrRowPtrB.DevicePointer, csrColIndPtrB.DevicePointer,
                                                !refdescrC, csrValPtrC.DevicePointer, csrRowPtrC.DevicePointer, csrColIndPtrC.DevicePointer)


        nnzTotalDevHostPtr.Dispose()

        !nnzC, csrValPtrC, csrRowPtrC, csrColIndPtrC

    let cusparseTransitiveClosure (parsingMatrix : ParsingMatrix<MySparseMatrix>) (allRules : BooleanRulesHolder) 
                                        (nonterminals:ResizeArray<NonTerminal>) matrixSize =

        let sparsecntx = new cusparseContext()
        let mutable refcnt = ref sparsecntx
        CudaSparseNativeMethods.cusparseCreate(refcnt) |> ignore

        let mutable csrValFakePtr : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(1))
        let mutable csrRowFakePtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(1))
        let mutable csrColIndFakePtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(1))            
        let fakeDeviceMatrix = (0, csrValFakePtr, csrRowFakePtr, csrColIndFakePtr)

        let deviceMatrices = new Dictionary<NonTerminal, int * CudaDeviceVariable<float> * CudaDeviceVariable<int> * CudaDeviceVariable<int>>()
        for nonterm in nonterminals do
            let curNNZ = parsingMatrix.[nonterm].Nnz
            if curNNZ = 0 then
                deviceMatrices.Add(nonterm, fakeDeviceMatrix)
            else
                let mutable csrValPtr : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(curNNZ))
                csrValPtr.CopyToDevice(parsingMatrix.[nonterm].CsrVal)
                let mutable csrRowPtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
                csrRowPtr.CopyToDevice(parsingMatrix.[nonterm].CsrRow)
                let mutable csrColIndPtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(curNNZ))            
                csrColIndPtr.CopyToDevice(parsingMatrix.[nonterm].CsrColInd)
                deviceMatrices.Add(nonterm, (curNNZ, csrValPtr, csrRowPtr, csrColIndPtr))

        let allConjuncts = allRules.AllConjuncts

        let conjDeviceMatrices = new Dictionary<NonTerminal*NonTerminal, int * CudaDeviceVariable<float> * CudaDeviceVariable<int> * CudaDeviceVariable<int>>()

        let isChanged = ref true
        let mutable multCount = 0

        while !isChanged do
            isChanged := false
            for (nt1, nt2) in allConjuncts do
                let resultDeviceMatrix = fastSparseCudaGemm deviceMatrices.[nt1] deviceMatrices.[nt2] matrixSize refcnt fakeDeviceMatrix
                conjDeviceMatrices.Add((nt1, nt2), resultDeviceMatrix)

            for (nonTerm,_), conjuncts in allRules.ComplexRules do
                
                let nonZ, csrVal, csrRow, csrColInd = deviceMatrices.[nonTerm]
                let resultMatrices = conjuncts |> Array.map (fun (n1,n2,_) -> conjDeviceMatrices.[n1,n2])
                if resultMatrices.Length = 1
                then
                    let resultConjMatrix = resultMatrices.[0]
                    let resultNnz,resultCsrVal,resultCsrRow,resultCsrColInd = resultConjMatrix
                    if (nonZ = 0) && (resultNnz <> 0) then
                        deviceMatrices.Remove(nonTerm) |> ignore

                        let mutable csrValPtr : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(resultNnz))
                        let csrValArray = Array.init resultNnz (fun x -> 0.0)        
                        resultCsrVal.CopyToHost(csrValArray)
                        csrValPtr.CopyToDevice(csrValArray)
                        let mutable csrRowPtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(matrixSize + 1))
                        let csrRowArray = Array.init (matrixSize + 1) (fun x -> 0)
                        resultCsrRow.CopyToHost(csrRowArray)
                        csrRowPtr.CopyToDevice(csrRowArray)
                        let mutable csrColIndPtr : CudaDeviceVariable<int> = new CudaDeviceVariable<int>(new SizeT(resultNnz))
                        let csrColIndArray = Array.init resultNnz (fun x -> 0)  
                        resultCsrColInd.CopyToHost(csrColIndArray)
                        csrColIndPtr.CopyToDevice(csrColIndArray)
                        deviceMatrices.Add(nonTerm, (resultNnz, csrValPtr, csrRowPtr, csrColIndPtr))
                    elif resultNnz <> 0 then
                        let updatedMatrix = fastSparseCudaGeam deviceMatrices.[nonTerm] resultConjMatrix matrixSize refcnt
                        csrVal.Dispose()
                        csrRow.Dispose()
                        csrColInd.Dispose()
                        deviceMatrices.Remove(nonTerm) |> ignore
                        deviceMatrices.Add(nonTerm, updatedMatrix)
                (*else //to do for conjunctive grammars
                    let resultConjMatrix = resultMatrices |> Array.fold (fun acc elem -> fastSparseCudaGeam acc elem matrixSize refcnt) resultMatrices.[0]
                    let resultNnz,_,_,_ = resultConjMatrix
                    if (nonZ = 0) && (resultNnz <> 0) then
                        csrVal.Dispose()
                        csrRow.Dispose()
                        csrColInd.Dispose()
                        deviceMatrices.Remove(nonTerm) |> ignore
                        deviceMatrices.Add(nonTerm, resultConjMatrix)
                    elif resultNnz <> 0 then
                        let updatedMatrix = fastSparseCudaGeam deviceMatrices.[nonTerm] resultConjMatrix matrixSize refcnt //need pointwise minimum instead of geam
                        csrVal.Dispose()
                        csrRow.Dispose()
                        csrColInd.Dispose()
                        deviceMatrices.Remove(nonTerm) |> ignore
                        deviceMatrices.Add(nonTerm, updatedMatrix)*)
                let newNnz,_,_,_ = deviceMatrices.[nonTerm]
                if (nonZ <> newNnz)
                then 
                    isChanged := true
                
            for (nt1, nt2) in allConjuncts do
                let curnnz, csrVal, csrRow, csrColInd = conjDeviceMatrices.[nt1, nt2]
                if curnnz <> 0 then
                    csrVal.Dispose()
                    csrRow.Dispose()
                    csrColInd.Dispose()
                conjDeviceMatrices.Remove(nt1,nt2) |> ignore
            //printfn "Iteration done"
            multCount <- multCount + 1
        
        let resultMatrix = new ParsingMatrix<MySparseMatrix>()
        for nonterm in nonterminals do
            let curnnz, csrVal, csrRow, csrColInd = deviceMatrices.[nonterm]
            if curnnz <> 0 then
                let csrValArray = Array.init curnnz (fun x -> 0.0)        
                csrVal.CopyToHost(csrValArray)
                csrVal.Dispose()
                let csrRowArray = Array.init (matrixSize + 1) (fun x -> 0)        
                csrRow.CopyToHost(csrRowArray)
                csrRow.Dispose()
                let csrColIndArray = Array.init curnnz (fun x -> 0)        
                csrColInd.CopyToHost(csrColIndArray)
                csrColInd.Dispose()
                deviceMatrices.Remove(nonterm) |> ignore
                resultMatrix.Add(nonterm, new MySparseMatrix(matrixSize, curnnz, csrValArray, csrRowArray, csrColIndArray))
            else
                let csrValArray = Array.init 0 (fun x -> 0.0)        
                let csrRowArray = Array.init (matrixSize + 1) (fun x -> 0)        
                let csrColIndArray = Array.init 0 (fun x -> 0)        
                deviceMatrices.Remove(nonterm) |> ignore
                resultMatrix.Add(nonterm, new MySparseMatrix(matrixSize, 0, csrValArray, csrRowArray, csrColIndArray))
        
        csrValFakePtr.Dispose()
        csrRowFakePtr.Dispose()
        csrColIndFakePtr.Dispose()

        (resultMatrix, multCount)


    
(*    let nontermLockFreeSplit (allRules: BooleanRulesHolder) nonterminals (splitCount: int) =        
        let tailsByHead = new Dictionary<NonTerminal, ResizeArray<NonTerminal*NonTerminal>>()
        for nontermPair in allRules.AllConjuncts do
            let heads = allRules.HeadsByComplexTail nontermPair
            for (head, _) in heads do
                if not <| tailsByHead.ContainsKey head
                then
                    tailsByHead.Add(head, new ResizeArray<NonTerminal * NonTerminal>())
                tailsByHead.[head].Add(nontermPair)
        let sortedPairs = tailsByHead |> Seq.sortByDescending (fun (KeyValue(k,v)) -> v.Count) |> Seq.map (fun (KeyValue(k,v)) -> (k,v)) |> List.ofSeq
        let splitedNontermPairs = Array.init splitCount (fun _ -> new ResizeArray<NonTerminal*NonTerminal>())
        let processedPairs = new ResizeArray<NonTerminal*NonTerminal>()
        let processedHeads = new ResizeArray<NonTerminal>()
        let currentPart = ref 0

        let rec addPairsForNonterm currentPart head =
            if not <| processedHeads.Contains head
            then
                processedHeads.Add(head)
                let tails = List.filter (fun (nont,tail) -> nont = head) sortedPairs |> List.head |> (fun (_, tail) -> tail)
                let newTails = Seq.filter (fun x -> not <| processedPairs.Contains x) tails |> List.ofSeq
                splitedNontermPairs.[!currentPart].AddRange(newTails)
                processedPairs.AddRange(newTails)
                for newTail in newTails do
                    let newHeads = allRules.HeadsByComplexTail newTail |> List.filter (fun (h,_) -> not <| processedHeads.Contains h)
                    for (newHead,_) in newHeads do
                        addPairsForNonterm currentPart newHead

        for (head,tails) in sortedPairs do
            addPairsForNonterm currentPart head
            if (processedPairs.Count >= (!currentPart + 1) * allRules.ComplexTails.Length / splitCount)
            then currentPart := !currentPart + 1
        splitedNontermPairs*)
