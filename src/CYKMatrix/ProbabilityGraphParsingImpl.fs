module ProbabilityGraphParsingImpl
    open MatrixKernels
    open Util
    open Alea.CUDA
    open Alea.CUDA.CULib
    open Alea.CUDA.Utilities
    open ManagedCuda
    open ManagedCuda.CudaSparse
    open ManagedCuda.BasicTypes

    let getSizeProbability (matrix:ProbabilityMatrix.T) = matrix.Size
    let createEmptyMatrixProbability = ProbabilityMatrix.empty
    let matrixSetValueProbability (matrix: ProbabilityMatrix.T) (i: int) (j: int) (value: float) = matrix.InnerValue.[i*matrix.Size + j] <- value
    let toArrayProbability (matrix: ProbabilityMatrix.T) (isTranspose: bool) = matrix.GetSubArray id isTranspose matrix.WholeMatrix
    let fromArrayProbability (arr: float[]) matrixSize =
        let newMatrix = createEmptyMatrixProbability matrixSize
        for ind in 0..matrixSize*matrixSize - 1 do
            newMatrix.InnerValue.[ind] <- arr.[ind]
        newMatrix

    let innerSumFloat f1 f2 = f1 + f2
    let innerMultFloat f1 f2 = f1 * f2
    let innerZeroFloat = 0.0
    let innerOneFloat = 1.0        
    
    let innerAPSPSumFloat f1 f2 = if (f1 = innerZeroFloat || f2 = innerZeroFloat) then max f1 f2 else min f1 f2
    let innerAPSPMultFloat f1 f2 = if (f1 = innerZeroFloat || f2 = innerZeroFloat) then innerZeroFloat else f1 + f2
    let innerAPSPZeroFloat = 0.0
    let innerAPSPOneFloat = 1.0   

    type ProbabilityNaiveHandler(_matrixSize:int) =       
        member this.unionArrays (arr1: float[]) (arr2: float[]) =
            let newArray = Array.init (_matrixSize*_matrixSize) (fun x -> innerZeroFloat)
            for ind in 0.._matrixSize*_matrixSize - 1 do
                if arr1.[ind] > innerZeroFloat || arr2.[ind] > innerZeroFloat
                then
                    newArray.[ind] <- innerOneFloat
            newArray

        member this.intersectArrays (arr1: float[]) (arr2: float[]) =
            let newArray = Array.init (_matrixSize*_matrixSize) (fun x -> innerZeroFloat)
            for ind in 0.._matrixSize*_matrixSize - 1 do
                if arr1.[ind] > innerZeroFloat && arr2.[ind] > innerZeroFloat
                then
                    newArray.[ind] <- innerOneFloat
            newArray

        member this.multArrays (arr1: float[]) (arr2: float[]) =      
            let calculateCell x =
                let i = x / _matrixSize
                let j = x - i * _matrixSize 
                let skipRows = i * _matrixSize
                let skipColumns = j * _matrixSize                
                Array.fold2 (fun b v1 v2 -> innerSumFloat b <| innerMultFloat v1 v2)
                                            innerZeroFloat
                                            arr1.[skipRows..skipRows + _matrixSize - 1] 
                                            arr2.[skipColumns..skipColumns + _matrixSize - 1]

            Array.init (_matrixSize * _matrixSize) (fun x -> calculateCell <| x)

        interface IMatrixHandler<ProbabilityMatrix.T, float> with
            member this.matrixSize = _matrixSize
            member this.createEmptyMatrix size = createEmptyMatrixProbability size
            member this.ParsingMatrixInitializator graph allRules nonterminals =
                initParsingMatrix<ProbabilityMatrix.T, float> graph allRules nonterminals createEmptyMatrixProbability matrixSetValueProbability innerOneFloat

            member this.Multiply (matrix1: ProbabilityMatrix.T) (matrix2: ProbabilityMatrix.T) =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 true
               let resultArray = this.multArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize
 
            member this.Add matrix1 matrix2 =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.unionArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize

            member this.Conj matrix1 matrix2 =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.intersectArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize

            member this.getNonZerosCount (matrix:ProbabilityMatrix.T) =
                let nnz = ref 0
                for ind in 0..matrix.Size*matrix.Size - 1 do
                    if matrix.InnerValue.[ind] > innerZeroFloat
                    then        
                        nnz := !nnz + 1
                !nnz


    type ProbabilityAleaCudaHandler(_matrixSize:int) =
        let worker = Worker.Default
        let mult1 = worker.Malloc((int)(_matrixSize * _matrixSize))
        let mult2 = worker.Malloc((int)(_matrixSize * _matrixSize))
        let result = worker.Malloc((int)(_matrixSize * _matrixSize))
               
        member this.unionArrays (arr1: float[]) (arr2: float[]) =
            let newArray = Array.init (_matrixSize*_matrixSize) (fun x -> innerZeroFloat)
            for ind in 0.._matrixSize*_matrixSize - 1 do
                if arr1.[ind] > innerZeroFloat || arr2.[ind] > innerZeroFloat
                then
                    newArray.[ind] <- innerOneFloat
            newArray

        member this.intersectArrays (arr1: float[]) (arr2: float[]) =
            let newArray = Array.init (_matrixSize*_matrixSize) (fun x -> innerZeroFloat)
            for ind in 0.._matrixSize*_matrixSize - 1 do
                if arr1.[ind] > innerZeroFloat && arr2.[ind] > innerZeroFloat
                then
                    newArray.[ind] <- innerOneFloat
            newArray

        member this.multArrays (from1: float[]) (from2: float[]) =      
            aleaCudaMultArrays from1 from2 _matrixSize mult1 mult2 result

        interface IMatrixHandler<ProbabilityMatrix.T, float> with
            member this.matrixSize = _matrixSize
            member this.createEmptyMatrix size = createEmptyMatrixProbability size
            member this.ParsingMatrixInitializator graph allRules nonterminals =
                initParsingMatrix<ProbabilityMatrix.T, float> graph allRules nonterminals createEmptyMatrixProbability matrixSetValueProbability innerOneFloat

            member this.Multiply (matrix1: ProbabilityMatrix.T) (matrix2: ProbabilityMatrix.T) =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.multArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize
 
            member this.Add matrix1 matrix2 =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.unionArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize

            member this.Conj matrix1 matrix2 =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.intersectArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize

            member this.getNonZerosCount (matrix:ProbabilityMatrix.T) =
                let nnz = ref 0
                for ind in 0..matrix.Size*matrix.Size - 1 do
                    if matrix.InnerValue.[ind] > innerZeroFloat
                    then        
                        nnz := !nnz + 1
                !nnz

    type ProbabilityManagedCudaHandler(_matrixSize:int) =
        let mutable cublashandle = new CudaBlas.CudaBlasHandle()
        let refhandle = ref cublashandle
        let initStatus = CudaBlas.CudaBlasNativeMethods.cublasCreate_v2(refhandle)
        let mult1 = new CudaDeviceVariable<float>(new SizeT(_matrixSize * _matrixSize))
        let mult2 = new CudaDeviceVariable<float>(new SizeT(_matrixSize * _matrixSize))
        let result = new CudaDeviceVariable<float>(new SizeT(_matrixSize * _matrixSize))
        member this.unionArrays (arr1: float[]) (arr2: float[]) =
            let newArray = Array.init (_matrixSize*_matrixSize) (fun x -> innerZeroFloat)
            for ind in 0.._matrixSize*_matrixSize - 1 do
                if arr1.[ind] > innerZeroFloat || arr2.[ind] > innerZeroFloat
                then
                    newArray.[ind] <- innerOneFloat
            newArray

        member this.intersectArrays (arr1: float[]) (arr2: float[]) =
            let newArray = Array.init (_matrixSize*_matrixSize) (fun x -> innerZeroFloat)
            for ind in 0.._matrixSize*_matrixSize - 1 do
                if arr1.[ind] > innerZeroFloat && arr2.[ind] > innerZeroFloat
                then
                    newArray.[ind] <- innerOneFloat
            newArray

        member this.multArrays (from1: float[]) (from2: float[]) =      
            managedCudaMultArrays from1 from2 _matrixSize refhandle mult1 mult2 result

        interface IMatrixHandler<ProbabilityMatrix.T, float> with
            member this.matrixSize = _matrixSize
            member this.createEmptyMatrix size = createEmptyMatrixProbability size
            member this.ParsingMatrixInitializator graph allRules nonterminals =
                initParsingMatrix<ProbabilityMatrix.T, float> graph allRules nonterminals createEmptyMatrixProbability matrixSetValueProbability innerOneFloat

            member this.Multiply (matrix1: ProbabilityMatrix.T) (matrix2: ProbabilityMatrix.T) =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.multArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize
 
            member this.Add matrix1 matrix2 =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.unionArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize

            member this.Conj matrix1 matrix2 =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.intersectArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize

            member this.getNonZerosCount (matrix:ProbabilityMatrix.T) =
                let nnz = ref 0
                for ind in 0..matrix.Size*matrix.Size - 1 do
                    if matrix.InnerValue.[ind] > innerZeroFloat
                    then        
                        nnz := !nnz + 1
                !nnz

    type ProbabilityMinPlusHandler(_matrixSize:int) =       
        member this.unionArrays (arr1: float[]) (arr2: float[]) =
            let newArray = Array.init (_matrixSize*_matrixSize) (fun x -> innerAPSPZeroFloat)
            for ind in 0.._matrixSize*_matrixSize - 1 do
                newArray.[ind] <- innerAPSPSumFloat arr1.[ind] arr2.[ind]
            newArray

        member this.intersectArrays (arr1: float[]) (arr2: float[]) =
            let newArray = Array.init (_matrixSize*_matrixSize) (fun x -> innerAPSPZeroFloat)
            for ind in 0.._matrixSize*_matrixSize - 1 do
                if arr1.[ind] > innerAPSPZeroFloat && arr2.[ind] > innerAPSPZeroFloat
                then
                    newArray.[ind] <- innerAPSPSumFloat arr1.[ind] arr2.[ind]
            newArray

        member this.multArrays (arr1: float[]) (arr2: float[]) =      
            let calculateCell x =
                let i = x / _matrixSize
                let j = x - i * _matrixSize 
                let skipRows = i * _matrixSize
                let skipColumns = j * _matrixSize                
                Array.fold2 (fun acc v1 v2 -> innerAPSPSumFloat acc <| innerAPSPMultFloat v1 v2)
                                            innerAPSPZeroFloat
                                            arr1.[skipRows..skipRows + _matrixSize - 1] 
                                            arr2.[skipColumns..skipColumns + _matrixSize - 1]

            Array.init (_matrixSize * _matrixSize) (fun x -> calculateCell <| x)

        interface IMatrixHandler<ProbabilityMatrix.T, float> with
            member this.matrixSize = _matrixSize
            member this.createEmptyMatrix size = createEmptyMatrixProbability size
            member this.ParsingMatrixInitializator graph allRules nonterminals =
                initParsingMatrix<ProbabilityMatrix.T, float> graph allRules nonterminals createEmptyMatrixProbability matrixSetValueProbability innerAPSPOneFloat

            member this.Multiply (matrix1: ProbabilityMatrix.T) (matrix2: ProbabilityMatrix.T) =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 true
               let resultArray = this.multArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize
 
            member this.Add matrix1 matrix2 =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.unionArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize

            member this.Conj matrix1 matrix2 =
               let arr1 = toArrayProbability matrix1 false
               let arr2 = toArrayProbability matrix2 false
               let resultArray = this.intersectArrays arr1 arr2
               fromArrayProbability resultArray _matrixSize

            member this.getNonZerosCount (matrix:ProbabilityMatrix.T) =
                let nnz = ref 0
                for ind in 0..matrix.Size*matrix.Size - 1 do
                    if matrix.InnerValue.[ind] > innerAPSPZeroFloat
                    then        
                        nnz := !nnz + 1
                !nnz