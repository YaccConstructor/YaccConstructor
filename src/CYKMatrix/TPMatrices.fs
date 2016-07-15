module TPMatrices 
open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Quotations

open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.OpenCL
open Alea.CUDA
open Alea.CUDA.CULib
open Alea.CUDA.Utilities
open Alea.CUDA.IL

open Util

    type NewGPUBrahma = 
        { provider: ComputeProvider
          commandQueue: CommandQueue
          localMemory: int option
          maxWorkGroupSize: int option
          kernel: _1D Brahma.OpenCL.Kernel
          kernelPrepare: _1D -> int32 [] -> Probability.InnerType [] -> Probability.InnerType [] -> Probability.InnerType [] -> unit
          kernelRun: unit -> _1D Commands.Run
          mult1: Probability.InnerType []
          mult2: Probability.InnerType []
          result: Probability.InnerType []
          sizes: int32 []
          options: Util.GPUBrahma }

    type GPUBrahma = 
        { provider: ComputeProvider
          commandQueue: CommandQueue
          localMemory: int option
          maxWorkGroupSize: int option
          kernel: _2D Brahma.OpenCL.Kernel
          kernelPrepare: _2D -> int -> Probability.InnerType [] -> Probability.InnerType [] -> Probability.InnerType [] -> unit
          kernelRun: unit -> _2D Commands.Run
          options: Util.GPUBrahma }

    type GPUCuda = 
        { worker: Worker
          options: Util.GPUCuda }
    
    type CPUFast = Unit
    type CPUSimple = Unit
    type CPUParallel = Unit
    
    type Helpers = {
        newBrahma: NewGPUBrahma Info option 
        Brahma: GPUBrahma Info option  
        Cuda: GPUCuda Info option  
        Fast: CPUFast Info option
        Simple: CPUSimple Info option
        Parallel: CPUParallel Info option
    }


    type MatriceswMultiplicator (options: Options.T, summProbabilities, multiplicateProbabilities, zeroProbability, maxMatrixSize, ntsCount) =


        let newCommand = 
            <@
                fun (r:_1D) (sizes:array<_>) (a:array<_>) (b:array<_>) (c:array<_>) -> 
                    // предполагается, что все в рамках одной пары матриц
                    let gridSize = sizes.[0]
                    let matricesSize = sizes.[2]
                    let matricesCount = sizes.[3]
                    let matrixCellNum = matricesSize * matricesSize
                    let globalCellCount = matrixCellNum * matricesCount
                    // todo:
                    let cellsToHandle_ = globalCellCount / gridSize
                    let cellsToHandle = 
                        if cellsToHandle_ < matrixCellNum
                        then cellsToHandle_
                        else matrixCellNum
                    let firstToHandle = r.GlobalID0 * cellsToHandle 
                    if firstToHandle < globalCellCount
                    then
                        let num = firstToHandle / matrixCellNum
                        let skipMatrices = num * matrixCellNum
                        for ti in [firstToHandle..matricesSize..firstToHandle + cellsToHandle - 1] do
                            let i = (ti - skipMatrices) / matricesSize
                            for tj in [ti..(min (firstToHandle + cellsToHandle - 1) (ti + matricesSize - 1))] do
                                let j = tj - skipMatrices - (i * matricesSize)         
                            
                                let skipRows = i * matricesSize + skipMatrices
                                let skipCols = j * matricesSize + skipMatrices
                                // todo: innerZero
                                let mutable buf = c.[skipRows + j]
                                for k in 0 .. matricesSize - 1 do
                                    buf <- (%summProbabilities) buf ((%multiplicateProbabilities) a.[skipRows + k] b.[skipCols + k])
                                c.[skipRows + j] <- buf
            @>  

        let command = 
            <@
                fun (r:_2D) matricesSize (a:array<_>) (b:array<_>) (c:array<_>) -> 
                    let num = r.GlobalID0 / matricesSize
                    let ti = r.GlobalID0 - num * matricesSize
                    let tj = r.GlobalID1                 
                    let skipMatrices = num * matricesSize * matricesSize
                    let skipRows = ti * matricesSize + skipMatrices
                    let skipCols = tj * matricesSize + skipMatrices
                    let mutable buf = (%zeroProbability)
//                    let mutable buf = c.[skipRows + tj]
//                    let k = ref 0
//                    while (!k < matricesSize) do 
                    for k in 0 .. (matricesSize - 1) do
                        buf <- (%summProbabilities) buf ((%multiplicateProbabilities) a.[skipRows + k] b.[skipCols + k])
//                        k := !k + 1
                    c.[skipRows + tj] <- buf
            @>  

        let command_ = 
            <@
                fun global0 global1 matricesSize (a:array<_>) (b:array<_>) (c:array<_>) -> 
                    let num = global0 / matricesSize
                    let ti = global0 - num * matricesSize
                    let tj = global1                 
                    let skipMatrices = num * matricesSize * matricesSize
                    let skipRows = ti * matricesSize + skipMatrices
                    let skipCols = tj * matricesSize + skipMatrices
                    let mutable buf = (%zeroProbability)
//                    let mutable buf = c.[skipRows + tj]
                    for k in 0 .. (matricesSize - 1) do
                        buf <- (%summProbabilities) buf ((%multiplicateProbabilities) a.[skipRows + k] b.[skipCols + k])
                    c.[skipRows + tj] <- buf
            @>  

        let newCreateBrahmaHelper minMatrixSize brahmaOptions =
            let provider =
                try ComputeProvider.Create(brahmaOptions.PlatformName, brahmaOptions.DeviceType)
                with 
                | ex -> failwith ex.Message

            let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
            let kernel, kernelPrepare, kernelRun = provider.Compile newCommand

            let getInfo infoType =
                let info, ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, infoType)
                match ex with 
                    | OpenCL.Net.ErrorCode.Success -> Some <| info.CastTo<int>()
                    | _ -> None
                    
            let localMem = getInfo OpenCL.Net.DeviceInfo.LocalMemSize
            let maxWGSize = getInfo OpenCL.Net.DeviceInfo.MaxWorkGroupSize
            let maxAllocationBits_ = getInfo OpenCL.Net.DeviceInfo.MaxMemAllocSize

            // todo: type size
            let maxAllocatedBitsForVar = maxMatrixSize * maxMatrixSize * 32 * ntsCount

            let maxAllocationBits = 
                match maxAllocationBits_ with
                | Some maxBits ->
                    // todo: split tasks
                    if 3 * maxAllocatedBitsForVar > maxBits 
                    then failwith "string size too big"
                    else maxBits
                | None ->
                    //todo: do something 
                    failwith "not enough information"

            let bufferSize = maxMatrixSize * maxMatrixSize * ntsCount
            
            let wgSizeRestriction = 
                match maxWGSize with
                    | Some size -> size
                    | None -> 4
            let localLinesRestriction = 
                match localMem with
                    // todo:
                    | Some size -> size / (32 * maxMatrixSize)
                    | None -> 32 * (1 <<< 20) / (32 * maxMatrixSize)

            
            // todo: sizes
            let gridSize = maxMatrixSize / minMatrixSize
            let wgSize = 2
            let grid = new _1D(gridSize, wgSize)

            let mult1 = Array.create bufferSize <| Probability.innerZero
            let mult2 = Array.create bufferSize <| Probability.innerZero
            let result = Array.create bufferSize <| Probability.innerZero
            let sizes = [|gridSize; wgSize; 0; 0|] |> Array.map int32

            kernelPrepare grid sizes mult1 mult2 result

            {
                provider = provider
                localMemory = localMem
                maxWorkGroupSize = maxWGSize
                commandQueue = commandQueue
                kernel = kernel
                kernelPrepare = kernelPrepare
                kernelRun = kernelRun
                mult1 = mult1
                mult2 = mult2
                result = result
                sizes = sizes
                options = brahmaOptions
            }
            
        let createBrahmaHelper brahmaOptions =
            let provider =
                try ComputeProvider.Create(brahmaOptions.PlatformName, brahmaOptions.DeviceType)
                with 
                | ex -> failwith ex.Message

            let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
            let kernel, kernelPrepare, kernelRun = provider.Compile command

            let getInfo infoType =
                let info, ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, infoType)
                match ex with 
                    | OpenCL.Net.ErrorCode.Success -> Some <| info.CastTo<int>()
                    | _ -> None
                    
            let localMem = getInfo OpenCL.Net.DeviceInfo.LocalMemSize
            let maxWGSize = getInfo OpenCL.Net.DeviceInfo.MaxWorkGroupSize

            {
                provider = provider; 
                localMemory = localMem;
                maxWorkGroupSize = maxWGSize;
                commandQueue = commandQueue; 
                kernel = kernel;
                kernelPrepare = kernelPrepare;
                kernelRun = kernelRun;
                options = brahmaOptions
            }

        let createCudaHelper cudaOptions =   
            {
                worker = Worker.Default
                options = cudaOptions
            }

        let helpers = {
            newBrahma = 
                match options.newBrahma with 
                | Some options -> Some (Options.map (newCreateBrahmaHelper options.MinMatrixSize) options)
                | None -> None
            Brahma = Option.map (Options.map createBrahmaHelper) options.Brahma
            Cuda = Option.map (Options.map createCudaHelper) options.Cuda
            Fast = Option.map id options.Fast
            Parallel = Option.map id options.Parallel
            Simple = Some { MinMatrixSize = 1; Options = () }
        }

        let simpleMultiplicate (from1: Probability.InnerType []) (from2: Probability.InnerType []) matricesSize actualColCount =        
            let calculateCell (n, i, j) = 
                let skipMatrices = n * matricesSize * matricesSize
                let skipRows = skipMatrices + i * matricesSize
                let skipColumns = skipMatrices + j * matricesSize                
                Array.fold2 (fun b v1 v2 -> Probability.innerSumm b <| Probability.innerMult v1 v2)
                            Probability.innerZero
                            from1.[skipRows..skipRows + matricesSize - 1] 
                            from2.[skipColumns..skipColumns + matricesSize - 1]

            let getNIJ x = 
                let n = x / (matricesSize * matricesSize)
                let i = (x - n * matricesSize * matricesSize) / matricesSize
                let j = x - n * matricesSize * matricesSize - i * matricesSize
                n, i, j
            Array.init (matricesSize * actualColCount) (fun x -> calculateCell <| getNIJ x)


        let ceilToPowerOf2 x = 
            let exp = (log (double x)) / (log 2.) |> ceil |> int
            1 <<< exp
        let floorToPowerOf2 x = 
            let exp = (log (double x)) / (log 2.) |> floor |> int
            1 <<< exp

        let wgSizesForMatricesWise wgSizeRestriction localLinesRestriction matricesCount matricesSize =
            if wgSizeRestriction < matricesSize * matricesSize || localLinesRestriction < 2 * matricesSize 
            then 
                let maxPairedLines = floorToPowerOf2 <| min ((double >> sqrt >> floor >> int) wgSizeRestriction) (localLinesRestriction / 2)
                maxPairedLines, maxPairedLines
            else 
                let maxCoeff = min (wgSizeRestriction / (matricesSize * matricesSize)) 
                                    (localLinesRestriction / (2 * matricesSize))
//                // !!!!!!!!!!!!!! some crazy code
//                let mutable coeff = maxCoeff
//                while (not (matricesCount % coeff = 0)) do 
//                    coeff <- coeff - 1
                // coeff should divide matrixCount
                let coeff = 1
                coeff * matricesSize, matricesSize

        let doCublasMultiplications matrixHandler (matricesCount: int) matricesSize fullTasks helpers =   
            let getSubMatrix, flushSubMatrix = matrixHandler
            let matricesCellCount = matricesSize * matricesSize
            
            let from1 = 
                fullTasks
                |> Array.map (fun ({ from1 = from }, nts) -> getSubMatrix (fst nts) from false)
                |> Array.concat            
            let from2 = 
                fullTasks
                |> Array.map (fun ({ from2 = from }, nts) -> getSubMatrix (snd nts) from false)
                |> Array.concat

            let worker = helpers.worker

            let transa = cublasOperation_t.CUBLAS_OP_N
            let transb = cublasOperation_t.CUBLAS_OP_N
                    
            let lda = matricesSize
            let ldb = matricesSize
            let ldc = matricesSize
        
            let dalpha = 1.
            use dA = worker.Malloc(from1)
            use dB = worker.Malloc(from2)
            let dbeta = 0.
            use dC = worker.Malloc(matricesSize * matricesSize * matricesCount)

            let doOne i =
                let ptrShift = i * matricesCellCount
                CUBLAS.Default.Dgemm(transa, transb, matricesSize, matricesSize, matricesSize, dalpha, dB.Ptr + ptrShift, ldb, dA.Ptr + ptrShift, lda, dbeta, dC.Ptr + ptrShift, ldc)

            [|0..matricesCount - 1|]
            |> Array.iter doOne      
            
            let multiplicationResult = dC.Gather()         

            let flushOneMatrix num = 
                let matrix = 
                    multiplicationResult.[num * matricesCellCount .. (num + 1) * matricesCellCount - 1]
                let task, nts = fullTasks.[num]
                flushSubMatrix task.where nts matrix   
            
            [|0..matricesCount - 1|]
            |> (if  helpers.options.doParallelFlush then Array.Parallel.iter else Array.iter) flushOneMatrix       


        let gpuMultiplicate helpers
                            (from1: Probability.InnerType []) 
                            (from2: Probability.InnerType []) 
                            matricesCount 
                            matricesSize =
               
            let result: Probability.InnerType [] = Array.zeroCreate(matricesCount * matricesSize * matricesSize)

            let wgSizeRestriction = 
                match helpers.maxWorkGroupSize with
                    | Some size -> size
                    | None -> 4
            let localLinesRestriction = 
                match helpers.localMemory with
                    // todo:
                    | Some size -> size / (32 * matricesSize)
                    | None -> 32 * (1 <<< 20) / (32 * matricesSize)
                    
            let wgItemsXCount = matricesCount * matricesSize
            let wgItemsYCount = matricesSize

//            let wgSizeX, wgSizeY = 1, 1
            let wgSizeX, wgSizeY = wgSizesForMatricesWise wgSizeRestriction localLinesRestriction matricesCount matricesSize 
//            let wgSizeX, wgSizeY = if wgSizeY_ < 2 then wgSizeX_, wgSizeY_ else wgSizeX_ / 2, wgSizeY_ / 2
                                                   
//            let d = new _2D(wgItemsXCount, wgItemsYCount, 1, 1)
//            let d = new _2D(wgItemsXCount, wgItemsYCount, wgSizeX, wgSizeY)

            // todo: local size
            let d = new _2D(wgItemsXCount, wgItemsYCount, wgSizeX, wgSizeY)

            helpers.kernelPrepare d matricesSize from1 from2 result
         
            helpers.commandQueue.Add(helpers.kernelRun()).Finish() |> ignore        
            helpers.commandQueue.Add(result.ToHost helpers.provider).Finish() |> ignore
            
            let checkWhetherResultIsValid () = 
                let result_: Probability.InnerType [] = Array.zeroCreate(matricesCount * matricesSize * matricesSize)
                for i in [0 .. (matricesCount * matricesSize - 1)] do
                    for j in [0 .. matricesSize - 1] do
                        (QuotationEvaluator.Evaluate command_) i j matricesSize from1 from2 result_
                
                let sameCells x = 
                    //                (Probability.unwrap toCheck.[i, j]) = (Probability.unwrap toCheckBFS.[i, j])
                    let value = float <| min result.[x] result_.[x]
                    let diff = abs <| result.[x] - result_.[x]
                    (float diff * 1000.) <= value

                for x in [ 0..matricesCount * matricesSize * matricesSize - 1 ] do 
                    if not <| sameCells x
                    then
                        let num = x / (matricesSize * matricesSize)
                        let i = (x - num * (matricesSize * matricesSize)) / matricesSize
                        let j = (x - num * (matricesSize * matricesSize) - i * matricesSize) 
                        printfn "During evaluation of %ith multiplication task value in cell (%i,%i) was computed wrong. Expecting %e but %e found." (num + 1) i j result_.[x] result.[x]

            checkWhetherResultIsValid()
                        
            result
            
        let doSimpleMultiplications isParallel matrixHandler matricesSize fullTasks = 
            let getSubMatrix, flushSubMatrix = matrixHandler
            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = getSubMatrix nt1 task.from1 false
                let from2 = getSubMatrix nt2 task.from2 true
                let resultArray = simpleMultiplicate from1 from2 matricesSize matricesSize
                let result = ProbabilityMatrix.create matricesSize matricesSize resultArray
                flushSubMatrix task.where nts result 
            
            fullTasks
            |> (if isParallel then Array.Parallel.iter else Array.iter) doOne 
            
        let doFastMultiplications isParallel matrixHandler matricesSize fullTasks = 
            let getSubMatrix, flushSubMatrix = matrixHandler
            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = getSubMatrix nt1 task.from1 false
                let from2 = getSubMatrix nt2 task.from2 false
                let result = from1 * from2                
                flushSubMatrix task.where nts result 
            
            fullTasks
            |> (if isParallel then Array.Parallel.iter else Array.iter) doOne 

        let newDoBrahmaMultiplications matrixHandler matricesCount matricesSize fullTasks helpers = 
            let getSubMatrix, flushSubMatrix = matrixHandler
            let matricesCellCount = matricesSize * matricesSize

            let copyMatrix (where: _ []) skip matrix =
                matrix |> Array.iteri (fun j v -> where.[skip + j] <- v)

            //todo: 2x copy
            fullTasks
            |> Array.map (fun ({ from1 = from }, nts) -> getSubMatrix (fst nts) from false)
            |> Array.iteri (fun i matrix -> copyMatrix helpers.mult1 (i * matricesCellCount) matrix)

            fullTasks
            |> Array.map (fun ({ from2 = from }, nts) -> getSubMatrix (snd nts) from true)
            |> Array.iteri (fun i matrix -> copyMatrix helpers.mult2 (i * matricesCellCount) matrix)

            [0..matricesCellCount * matricesCount-1]
            |> List.iter (fun i -> helpers.result.[i] <- Probability.innerZero)

            helpers.sizes.[2] <- int32 matricesSize
            helpers.sizes.[3] <- int32 matricesCount
            
            helpers.commandQueue.Add(helpers.mult1.ToGpu(helpers.provider)) |> ignore   
            helpers.commandQueue.Add(helpers.mult2.ToGpu(helpers.provider)) |> ignore  
            helpers.commandQueue.Add(helpers.result.ToGpu(helpers.provider)) |> ignore 
            helpers.commandQueue.Add(helpers.sizes.ToGpu(helpers.provider)) |> ignore          
            helpers.commandQueue.Add(helpers.kernelRun()) |> ignore        
            helpers.commandQueue.Add(helpers.result.ToHost helpers.provider).Finish() |> ignore

            let flushOneMatrix num = 
                let matrix = 
                    helpers.result.[num * matricesCellCount .. (num + 1) * matricesCellCount - 1]
                    |> ProbabilityMatrix.create matricesSize matricesSize
                let task, nts = fullTasks.[num]
                flushSubMatrix task.where nts matrix          
            
            [|0..matricesCount - 1|]
            |> (if helpers.options.doParallelFlush then Array.Parallel.iter else Array.iter) flushOneMatrix
            
        let doBrahmaMultiplications matrixHandler matricesCount matricesSize fullTasks helpers = 
            let getSubMatrix, flushSubMatrix = matrixHandler
            let matricesCellCount = matricesSize * matricesSize
            
            let from1 = 
                fullTasks
                |> Array.map (fun ({ from1 = from }, nts) -> getSubMatrix (fst nts) from false)
                |> Array.concat
            
            let from2 = 
                fullTasks
                |> Array.map (fun ({ from2 = from }, nts) -> getSubMatrix (snd nts) from true)
                |> Array.concat
                
            let multiplicationResult = gpuMultiplicate helpers from1 from2 matricesCount matricesSize

            let flushOneMatrix num = 
                let matrix = 
                    multiplicationResult.[num * matricesCellCount .. (num + 1) * matricesCellCount - 1]
                    |> ProbabilityMatrix.create matricesSize matricesSize
                let task, nts = fullTasks.[num]
                flushSubMatrix task.where nts matrix          
            
            [|0..matricesCount - 1|]
            |> (if helpers.options.doParallelFlush then Array.Parallel.iter else Array.iter) flushOneMatrix
  
        member this.performMultiplication arrayMatrixHandler fastMatrixHandler cublasMatrixHandler (tasks: MultiplicationTask []) nts = 

            let crossproduct l1 l2 =
                let product lst v = Array.map (fun vl -> (vl, v)) lst
                Array.collect (product l1) l2

            let fullTasks = crossproduct tasks nts

            let matricesSize = tasks.[0].where.Size
            let matricesCount = fullTasks.Length

            let isParallel =
                match options.Parallel with 
                | Some { MinMatrixSize = size } -> matricesSize >= size
                | None -> false
            
            let doFast _ = doFastMultiplications isParallel fastMatrixHandler matricesSize fullTasks 
            let doSimple _  = doSimpleMultiplications isParallel arrayMatrixHandler matricesSize fullTasks 
            let doBrahma helpers = doBrahmaMultiplications arrayMatrixHandler matricesCount matricesSize fullTasks helpers
            let doNewBrahma helpers = newDoBrahmaMultiplications arrayMatrixHandler matricesCount matricesSize fullTasks helpers
            let doCublas helpers = doCublasMultiplications cublasMatrixHandler matricesCount matricesSize fullTasks helpers

            let doCheck doMultiplication (helpers_: _ Info option) isDone = 
                if not isDone 
                then 
                    match helpers_ with
                    | Some helpers ->            
                        if matricesSize >= helpers.MinMatrixSize
                        then 
                            doMultiplication helpers.Options
                            true
                        else false
                    | None -> false
                else isDone

            false 
            |> doCheck doCublas helpers.Cuda
            |> doCheck doBrahma helpers.Brahma  
            |> doCheck doNewBrahma helpers.newBrahma      
            |> doCheck doFast helpers.Fast
            |> doCheck doSimple helpers.Simple
            |> ignore


        member this.releaseResources () =
            match helpers.Brahma with
            | Some helpers ->
                helpers.Options.commandQueue.Dispose()
                helpers.Options.provider.Dispose()
                helpers.Options.provider.CloseAllBuffers()
                ()
            | None -> 
            match helpers.Cuda with
            | Some helpers -> ()
            // wtf?
//                    helpers.Options.worker.Dispose()  
            | None -> ()




       
    type MatrixHolder(tKeys, pKeys, stringSize, options: Options.T) = 
        
        let GPUMultiplicator = 
            // todo: think about search size!!!!!!!!!!
            let matrixSizeExponent = (log (double stringSize + 1.)) / (log 2.) |> ceil |> int
            let maxMatrixForMultiplicationSize = (1 <<< (matrixSizeExponent - 1))
            MatriceswMultiplicator (options, 
                                    Probability.innerSummQuote, 
                                    Probability.innerMultQuote, 
                                    Probability.innerZeroQuote, 
                                    maxMatrixForMultiplicationSize, 
                                    Array.length pKeys)
            
        // swich to dictionary (?)
        let tMatrix = Map<NonTerminal, ProbabilityMatrix.T>
                            (
                                tKeys 
                                |> Seq.map (fun x -> x, ProbabilityMatrix.empty (stringSize + 1))
                            )

        let pMatrix = Map<NonTerminal * NonTerminal, ProbabilityMatrix.T>
                            (
                                pKeys
                                |> Array.map (fun x -> x, ProbabilityMatrix.empty (stringSize + 1))
                            )   

        
        let addToPMatrix (where: SubMatrix.T) nts getValueFromCell =
            let whereMatrix = pMatrix.[nts]
            for i in [0 .. where.Size - 1] do
                let actualColCount = (min (where.Top.Column) (stringSize + 1)) - where.Left.Column
                for j in [0 .. actualColCount - 1] do
                    let matrixCell = Cell.create i j
                    let realCell = Cell.shift where.Left.Row where.Left.Column matrixCell
                    whereMatrix.AddValueToCell realCell <| getValueFromCell matrixCell


        member this.updateTCellWith cell nonterminals =
            nonterminals |> List.iter (fun (key, prob) -> tMatrix.[key].AddValueToCell cell prob)
            
        member this.getProbabilities nt = tMatrix.[nt]

        member this.initTDiagonalWith nonterminals =
            let diagonalCell i = Cell.create i (i + 1)
            nonterminals |> List.iteri (fun i ntProbs -> this.updateTCellWith (diagonalCell i) ntProbs)

        member this.refreshTCells headProbsFromTail cells =
                let tails cell = 
                    pMatrix 
                    |> Map.map (fun _ probs -> probs.[cell]) 
                    |> Map.filter (fun _ prob -> not <| Probability.isZero prob)
                let heads cell = 
                    tails cell 
                    |> Map.toList 
                    |> List.map headProbsFromTail 
                    |> List.concat

                cells
                |> Array.iter (fun cell -> heads cell |> this.updateTCellWith cell)

        member this.performMultiplication tasks nts = 
            
            let addProbMatrix (where: SubMatrix.T) nts (matrix: ProbabilityMatrix.T) = addToPMatrix where nts (fun cell -> matrix.[cell])
            let addCublasMatrix (where: SubMatrix.T) nts (matrix: float []) =
                let getValueFromCell (cell: Cell.T) =
                    let x = cell.Row * where.Size + cell.Column
                    Probability.create matrix.[x]
                addToPMatrix where nts getValueFromCell
            let addFastMatrix (where: SubMatrix.T) nts (matrix: Matrix<float>) = 
                addToPMatrix where nts (fun cell -> Probability.create matrix.[cell.Row, cell.Column])         

            let getInnerSubMatrix nt = tMatrix.[nt].GetSubArrayWithType id
            let getFloatSubMatrix nt = tMatrix.[nt].GetSubArrayWithType float
            let getFastSubMatrix nt (submatrix: SubMatrix.T) isTransponed : Matrix<float> =
                let valueGetter = tMatrix.[nt].SubMatrixValuesGetter submatrix isTransponed float                        
                Matrix.init submatrix.Size submatrix.Size (fun x y -> Cell.create x y |> valueGetter)

            let arrayMatrixHandler = (getInnerSubMatrix, addProbMatrix)
            let fastMatrixHandler = (getFastSubMatrix, addFastMatrix)
            let cublasMatrixHandler = (getFloatSubMatrix, addCublasMatrix)
            GPUMultiplicator.performMultiplication arrayMatrixHandler fastMatrixHandler cublasMatrixHandler tasks nts

        member this.releaseResources = GPUMultiplicator.releaseResources