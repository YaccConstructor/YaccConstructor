module GraphParsing
    open Util
    open TPMatrices
    open System
    open System.Collections.Generic
    open Yard.Core
    open Yard.Core.IL
    open Yard.Core.IL.Production
    open Yard.Core.IL.Definition
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
    open Microsoft.FSharp.Core.Operators

    type ParsingMatrix<'MatrixType> = Dictionary<NonTerminal, 'MatrixType>

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
                  (allRules: RulesHolder)
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
    
    let naiveSquareMatrix<'MatrixType, 'InnerType when 'InnerType : comparison> matrixSetValue toArray (innerSum: 'InnerType -> 'InnerType -> 'InnerType)
                (innerMult: 'InnerType -> 'InnerType -> 'InnerType) (innerZero: 'InnerType) (innerOne: 'InnerType)
                (matrix: ParsingMatrix<'MatrixType>) (allRules: RulesHolder) isChanged matrixSize =
        let unionArrays (matrix: 'MatrixType) (curArr: 'InnerType []) (updArr: 'InnerType []) =
            for ind in 0..matrixSize*matrixSize - 1 do
                if curArr.[ind] = innerZero && updArr.[ind] > innerZero
                then
                    isChanged := true
                    let i = ind / matrixSize
                    let j = ind - i * matrixSize
                    matrixSetValue matrix i j innerOne

        let multArrays (from1: 'InnerType []) (from2: 'InnerType []) =        
                let calculateCell x =
                    let i = x / matrixSize
                    let j = x - i * matrixSize 
                    let skipRows = i * matrixSize
                    let skipColumns = j * matrixSize                
                    Array.fold2 (fun b v1 v2 -> innerSum b <| innerMult v1 v2)
                                innerZero
                                from1.[skipRows..skipRows + matrixSize - 1] 
                                from2.[skipColumns..skipColumns + matrixSize - 1]

                Array.init (matrixSize * matrixSize) (fun x -> calculateCell <| x)

        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let arr1 = toArray matrix.[nt1] false
            let arr2 = toArray matrix.[nt2] true
            let resultArray = multArrays arr1 arr2

            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                unionArrays matrix.[nonTerm] (toArray matrix.[nonTerm] false) resultArray

    let sparseSquareMatrix (matrix: ParsingMatrix<SparseMatrix>) (allRules: RulesHolder) isChanged matrixSize =
        let unionArrays (matrix: SparseMatrix) (updMatrix: MathNet.Numerics.LinearAlgebra.Matrix<float>) =            
            for i in 0..(matrixSize - 1) do
                for j in 0..(matrixSize - 1) do
                    if matrix.At(i, j) = 0.0 && updMatrix.At(i, j) > 0.0
                    then
                        isChanged := true
                        matrix.At(i, j, 1.0)

        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let matrix1 = matrix.[nt1]
            let matrix2 = matrix.[nt2]
            let resultMatrix = matrix1.Multiply(matrix2)
            
            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                unionArrays matrix.[nonTerm] resultMatrix

    let sparseSquareMatrix2 (matrix: ParsingMatrix<SparseMatrix>) (allRules: RulesHolder) isChanged matrixSize =
        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let matrix1 = matrix.[nt1]
            let matrix2 = matrix.[nt2]
            let resultMatrix = matrix1.Multiply(matrix2)
            
            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                let nonZ = matrix.[nonTerm].NonZerosCount
                matrix.[nonTerm].PointwiseMaximum(resultMatrix, matrix.[nonTerm])
                if (nonZ <> matrix.[nonTerm].NonZerosCount)
                then
                    isChanged := true

    let sparseParallelSquareMatrix (matrix: ParsingMatrix<SparseMatrix>) (allRules: RulesHolder) isChanged matrixSize =
        let nontermPairs = allRules.ComplexTails
        let nontermPairs1,nontermPairs2 = nontermPairs.[0..nontermPairs.Length/2 - 1],nontermPairs.[nontermPairs.Length/2 .. nontermPairs.Length-1]        
        let go nontermPairs =
            for (nt1, nt2) in nontermPairs do
                let matrix1 = matrix.[nt1]
                let matrix2 = matrix.[nt2]
                let resultMatrix = matrix1.Multiply(matrix2)          
                for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                        let nonZ = matrix.[nonTerm].NonZerosCount
                        lock matrix (fun () ->
                        matrix.[nonTerm].PointwiseMaximum(resultMatrix, matrix.[nonTerm])
                        )
                        if (nonZ <> matrix.[nonTerm].NonZerosCount)
                        then isChanged := true
        let t1 = System.Threading.Thread (fun () -> go nontermPairs1)
        let t2 = System.Threading.Thread (fun () -> go nontermPairs2)
        t1.Start()
        t2.Start()
        t1.Join()
        t2.Join()

    let worker = Worker.Default

    let cudaSquareMatrix<'MatrixType> matrixSetValue toArray (matrix: ParsingMatrix<'MatrixType>) (allRules: RulesHolder) isChanged matrixSize =
        
        let (mult1:DeviceMemory<float>) = worker.Malloc((int)(matrixSize * matrixSize)) //need to do malloc only once per graph parsing
        let (mult2:DeviceMemory<float>) = worker.Malloc((int)(matrixSize * matrixSize))
        let (result:DeviceMemory<float>) = worker.Malloc((int)(matrixSize * matrixSize))

        let unionArrays (matrix: 'MatrixType) (curArr: float []) (updArr: float []) =
            for ind in 0..matrixSize*matrixSize - 1 do
                if curArr.[ind] = 0.0 && updArr.[ind] > 0.0
                then
                    isChanged := true
                    let i = ind / matrixSize
                    let j = ind - i * matrixSize
                    matrixSetValue matrix i j 1.0

        let multArrays (from1: float []) (from2: float []) =

                let transa = cublasOperation_t.CUBLAS_OP_N
                let transb = cublasOperation_t.CUBLAS_OP_N

                let dalpha = 1.
                let dbeta = 0.

                let multiplicationResult =               
                    
                    mult1.Scatter(from1)
                    mult2.Scatter(from2)

                    CUBLAS.Default.Dgemm(transa, 
                                            transb, 
                                            matrixSize, 
                                            matrixSize, 
                                            matrixSize, 
                                            dalpha,
                                            mult2.Ptr, 
                                            matrixSize, 
                                            mult1.Ptr, 
                                            matrixSize, 
                                            dbeta, 
                                            result.Ptr, 
                                            matrixSize) // mult1 and mult2 swaped because Dgemm expect column-major matrices

                    let resultArr = result.Gather()  
                    resultArr

                multiplicationResult

        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let arr1 = toArray matrix.[nt1] false
            let arr2 = toArray matrix.[nt2] false
            let resultArray = multArrays arr1 arr2

            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                unionArrays matrix.[nonTerm] (toArray matrix.[nonTerm] false) resultArray

    
    let managedCudaSquareMatrix<'MatrixType> matrixSetValue toArray (matrix: ParsingMatrix<'MatrixType>) (allRules: RulesHolder) isChanged matrixSize =
        
        let unionArrays (matrix: 'MatrixType) (curArr: float []) (updArr: float []) =
            for ind in 0..matrixSize*matrixSize - 1 do
                if curArr.[ind] = 0.0 && updArr.[ind] > 0.0
                then
                    isChanged := true
                    let i = ind / matrixSize
                    let j = ind - i * matrixSize
                    matrixSetValue matrix i j 1.0
        
        let multArrays (from1: float []) (from2: float []) =
            let cublashandle = new CudaBlas.CudaBlasHandle()
            let mutable refhandle = ref cublashandle
            CudaBlas.CudaBlasNativeMethods.cublasCreate_v2(refhandle) |> ignore

            let transa = CudaBlas.Operation.NonTranspose
            let transb = CudaBlas.Operation.NonTranspose

            let alpha_ref = ref 1.0
            let beta_ref = ref 0.0

            let mutable devicePtrA : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(matrixSize * matrixSize))
            devicePtrA.CopyToDevice(from1)
            let mutable devicePtrB : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(matrixSize * matrixSize))
            devicePtrB.CopyToDevice(from2)
            let mutable devicePtrC : CudaDeviceVariable<float> = new CudaDeviceVariable<float>(new SizeT(matrixSize * matrixSize))
        
            CudaBlas.CudaBlasNativeMethods.cublasDgemm_v2(!refhandle, transa, transb, matrixSize, matrixSize, matrixSize,
                                                                alpha_ref, devicePtrB.DevicePointer, matrixSize,
                                                                devicePtrA.DevicePointer, matrixSize,
                                                                beta_ref, devicePtrC.DevicePointer, matrixSize) |> ignore
            
            let resultArr = Array.init (matrixSize * matrixSize) (fun x -> 0.0)   
            devicePtrC.CopyToHost(resultArr)
            resultArr


        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let arr1 = toArray matrix.[nt1] false
            let arr2 = toArray matrix.[nt2] false
            let resultArray = multArrays arr1 arr2

            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                unionArrays matrix.[nonTerm] (toArray matrix.[nonTerm] false) resultArray

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

    let sparseCudaSquareMatrix (matrix: ParsingMatrix<MySparseMatrix>) (allRules: RulesHolder) isChanged matrixSize =
        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let matrix1 = matrix.[nt1]
            let matrix2 = matrix.[nt2]

            let resultMatrix = sparseCudaGemm matrix1 matrix2 matrixSize
            
            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                let nnz = matrix.[nonTerm].Nnz

                let finalMatrix = sparseCudaGeam matrix.[nonTerm] resultMatrix matrixSize
                if (nnz <> finalMatrix.Nnz)
                then
                    matrix.Remove(nonTerm) |> ignore
                    matrix.Add(nonTerm, finalMatrix)
                    isChanged := true

        
    
    let nontermLockFreeSplit (allRules: RulesHolder) nonterminals (splitCount: int) =
        let tailsByHead = new Dictionary<NonTerminal, ResizeArray<NonTerminal*NonTerminal>>()
        for nontermPair in allRules.ComplexTails do
            let heads = allRules.HeadsByComplexTail nontermPair
            for (head, _) in heads do
                if not <| tailsByHead.ContainsKey head
                then
                    tailsByHead.Add(head, new ResizeArray<NonTerminal * NonTerminal>())
                tailsByHead.[head].Add(nontermPair)
        let sortedPairs = tailsByHead |> Seq.sortByDescending (fun (KeyValue(k,v)) -> v.Count) |> Seq.map (fun (KeyValue(k,v)) -> (k,v)) |> List.ofSeq
        let splitedNontermPairs = Array.init splitCount (fun _ -> new ResizeArray<NonTerminal*NonTerminal>())
        let processedPairs = ref 0
        let currentPart = ref 0
        for (head, tails) in sortedPairs do
            splitedNontermPairs.[!currentPart].AddRange(tails)
            processedPairs := !processedPairs + tails.Count
            if (!processedPairs >= (!currentPart + 1) * allRules.ComplexTails.Length / splitCount)
            then currentPart := !currentPart + 1
        splitedNontermPairs            

    type Message = bool

    let recognizeGraphP<'InnerType when 'InnerType : comparison> (graph:AbstractAnalysis.Common.SimpleInputGraph<int>)
                  (matrixInitializator:AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> seq<NonTerminal> -> (ParsingMatrix<SparseMatrix> * Dictionary<int,int>))
                  (allRules: RulesHolder)
                  nonterminals
                  S =
        let parsingMatrixCurrent, vertexToInt = matrixInitializator graph allRules nonterminals
        let matrixSize = graph.VertexCount
        let isChanged = ref true
        let mutable multCount = 0
        let parsingMatrixNew = new ParsingMatrix<SparseMatrix>()
        for nont in parsingMatrixCurrent.Keys do
            parsingMatrixNew.Add(nont, new SparseMatrix(matrixSize))
            parsingMatrixCurrent.[nont].CopyTo(parsingMatrixNew.[nont])


        let nontermPairs = allRules.ComplexTails

        let splitCount = 4

        let splitedNontermPairs = nontermLockFreeSplit allRules nonterminals splitCount

        let flags = Array.init splitCount (fun _ -> ref false)
        let values = Array.init splitCount (fun _ -> ref false)
        
        let mbp flg vl nontermPairs_mbp = new MailboxProcessor<Message>(fun inbox ->
            let rec loop n =
                async {                                  
                        let! message = inbox.Receive();
                        for (nt1, nt2) in nontermPairs_mbp do
                            let matrix1 = parsingMatrixCurrent.[nt1]
                            let matrix2 = parsingMatrixCurrent.[nt2]
                            let resultMatrix = matrix1.Multiply(matrix2)          
                            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                                    let nonZ = parsingMatrixCurrent.[nonTerm].NonZerosCount
                                    //lock parsingMatrix (fun () ->
                                    parsingMatrixNew.[nonTerm].PointwiseMaximum(resultMatrix, parsingMatrixNew.[nonTerm])
                                    //)
                                    if (nonZ <> parsingMatrixNew.[nonTerm].NonZerosCount)
                                    then 
                                        vl := true
                        flg:= true
                        do! loop (n + 1)
                }
            loop (0))
        
        let mailBoxes = Array.init splitCount (fun i -> mbp flags.[i] values.[i] splitedNontermPairs.[i])

        for i in 0..(mailBoxes.Length-1) do
            mailBoxes.[i].Start()

        while !isChanged do
            isChanged := false
            for i in 0..(mailBoxes.Length-1) do
                mailBoxes.[i].Post(false)

            while not <| Array.TrueForAll (flags, (fun fl -> !fl)) do ()

            for i in 0..(flags.Length-1) do
                flags.[i] := false

            isChanged := Array.Exists (values, (fun vl -> !vl))

            for i in 0..(values.Length-1) do
                values.[i] := false
            
            for nont in parsingMatrixNew.Keys do
                parsingMatrixNew.[nont].CopyTo(parsingMatrixCurrent.[nont])

            multCount <- multCount + 1            

        (parsingMatrixNew.[S], vertexToInt, multCount)


    let initRulesFromIL loadIL tokenToInt =
        let grammar = loadIL.grammar
        let mutable tokensCount = 0
        let S = ref (NonTerminal "")
        let nonterminals = new ResizeArray<NonTerminal>()
        let crl = new Dictionary<NonTerminal * NonTerminal, ResizeArray<NonTerminal*Probability.T>>()
        let srl = new Dictionary<int, ResizeArray<NonTerminal*Probability.T>>()
        let crl_result = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
        let srl_result = new Dictionary<int, (NonTerminal * Probability.T) list>()
        let erl_result: NonTerminal list = []

        let probOne = Probability.create 1.0

        for module' in grammar do
            for r in module'.rules do
                let nonterm = NonTerminal <| Source.toString r.name
                if not <| nonterminals.Contains nonterm
                then
                    nonterminals.Add nonterm
                    if r.isStart
                    then
                        S := nonterm

                match r.body with
                | PSeq([elem],_,_) ->
                    match elem.rule with
                    | PToken src ->
                        let token = Source.toString src
                        let intToken = tokenToInt token
                        if not <| srl.ContainsKey intToken
                        then
                            srl.Add(intToken, new ResizeArray<NonTerminal*Probability.T>())
                        if not <| srl.[intToken].Contains (nonterm, probOne)
                        then
                            srl.[intToken].Add (nonterm, probOne)
                    | _ ->
                        failwith "Given grammar is not in normal form."
                        
                | PSeq([e1; e2],_,_) ->
                    match e1.rule, e2.rule with 
                    | PRef (name1, _), PRef (name2, _) ->
                        let nonterm1 = NonTerminal <| Source.toString name1
                        if not <| nonterminals.Contains nonterm1
                        then
                            nonterminals.Add nonterm1
                        let nonterm2 = NonTerminal <| Source.toString name2
                        if not <| nonterminals.Contains nonterm2
                        then
                            nonterminals.Add nonterm2
                        if not <| crl.ContainsKey (nonterm1, nonterm2)
                        then
                            crl.Add((nonterm1, nonterm2), new ResizeArray<NonTerminal*Probability.T>())
                        if not <| crl.[(nonterm1, nonterm2)].Contains (nonterm, probOne)
                        then
                            crl.[(nonterm1, nonterm2)].Add (nonterm, probOne)                     
                    | _ -> failwith "Given grammar is not in normal form."               
                | _ -> failwith "Given grammar is not in normal form."

        for key in crl.Keys do
            let list = Seq.toList crl.[key]
            crl_result.Add(key, list)
        for key in srl.Keys do
            let list = Seq.toList srl.[key]
            srl_result.Add(key, list)
        
        let rulesHolder = new RulesHolder(crl_result, srl_result, erl_result)

        (rulesHolder, nonterminals, S)
 
    let graphParseParallel<'InnerType when 'InnerType : comparison> (graph:AbstractAnalysis.Common.SimpleInputGraph<int>)
                  (matrixInitializator:AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> seq<NonTerminal> -> (ParsingMatrix<SparseMatrix> * Dictionary<int,int>))
                  (loadIL:t<Source.t, Source.t>)
                  tokenToInt =

        let (rulesHolder, nonterminals, S) = initRulesFromIL loadIL tokenToInt

        recognizeGraphP<'InnerType> graph matrixInitializator rulesHolder nonterminals !S


    let recognizeGraph<'MatrixType, 'InnerType when 'InnerType : comparison> graph
                  (matrixInitializator:AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> seq<NonTerminal> -> (ParsingMatrix<'MatrixType> * Dictionary<int,int>))
                  (squareMatrix:ParsingMatrix<'MatrixType> -> RulesHolder -> bool ref -> int  -> unit)
                  (allRules: RulesHolder)
                  nonterminals
                  S =
        let parsingMatrix, vertexToInt = matrixInitializator graph allRules nonterminals
        printfn "Matrix initialized"
        let matrixSize = graph.VertexCount
        let isChanged = ref true
        let mutable multCount = 0

        while !isChanged do
            isChanged := false
            squareMatrix parsingMatrix allRules isChanged matrixSize
            printfn "Multiplication done"
            multCount <- multCount + 1

        (parsingMatrix.[S], vertexToInt, multCount)    

    let graphParse<'MatrixType, 'InnerType when 'InnerType : comparison> (graph:AbstractAnalysis.Common.SimpleInputGraph<int>)
                  (matrixInitializator:AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> seq<NonTerminal> -> (ParsingMatrix<'MatrixType> * Dictionary<int,int>))
                  squareMatrix
                  (loadIL:t<Source.t, Source.t>)
                  tokenToInt =

        let (rulesHolder, nonterminals, S) = initRulesFromIL loadIL tokenToInt

        recognizeGraph<'MatrixType, 'InnerType> graph matrixInitializator squareMatrix rulesHolder nonterminals !S

