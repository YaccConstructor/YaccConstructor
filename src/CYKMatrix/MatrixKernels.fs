module MatrixKernels
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


    type ParsingMatrix<'MatrixType> = Dictionary<NonTerminal, 'MatrixType>

    type IMatrixHandler<'MatrixType, 'InnerType when 'InnerType : comparison> =
        abstract matrixSize : int
        abstract createEmptyMatrix : int -> 'MatrixType
        abstract ParsingMatrixInitializator : AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> ResizeArray<NonTerminal> -> (ParsingMatrix<'MatrixType> * Dictionary<int,int>)
        abstract Multiply : 'MatrixType -> 'MatrixType -> 'MatrixType
        abstract Add : 'MatrixType -> 'MatrixType -> 'MatrixType
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
        member this.GetItem (i, j) =
            let nzInd =  
                [_CsrRow.[i] .. _CsrRow.[i + 1] - 1]
                |> List.tryFind (fun k -> _CsrColInd.[k] = j)
            in if nzInd.IsSome then _CsrVal.[nzInd.Value] else 0.0

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
        splitedNontermPairs
