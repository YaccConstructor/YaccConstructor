module TPMatrices 

open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System.Threading

open Alea.CUDA.Utilities.Array2D

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
          kernelPrepare: _1D -> int32 [] -> Probability.InnerType.T [] -> Probability.InnerType.T [] -> Probability.InnerType.T [] -> unit
          kernelRun: unit -> _1D Commands.Run
          mult1: Probability.InnerType.T []
          mult2: Probability.InnerType.T []
          result: Probability.InnerType.T []
          sizes: int32 []
          options: Util.GPUBrahma }

    type GPUBrahma = 
        { provider: ComputeProvider
          commandQueue: CommandQueue
          localMemory: int option
          maxWorkGroupSize: int option
          kernel: _2D Brahma.OpenCL.Kernel
          kernelPrepare: _2D -> int -> Probability.InnerType.T [] -> Probability.InnerType.T [] -> Probability.InnerType.T [] -> unit
          kernelRun: unit -> _2D Commands.Run
          options: Util.GPUBrahma }

    type GPUCuda = 
        { worker: Worker
          options: Util.GPUCuda
          mult1: DeviceMemory<float>
          mult2: DeviceMemory<float>
          result: DeviceMemory<float> }
    
    type CPU_ = { IsParallel: int -> bool }
    type Parallel_ = Unit
    
    type Helpers = {
        _1DBrahma: NewGPUBrahma Info option 
        Brahma: GPUBrahma Info option  
        Cuda: GPUCuda Info option  
        Fast: CPU_ Info option
        Simple: CPU_ Info option
        Parallel: Parallel_ Info option
    }
        
    type MatricesMultiplicator (options: Options.T, summProbabilities, multiplicateProbabilities, zeroProbability, maxMatrixSize, ntsCount) =
        
        let gpuMutex = new Mutex()
        
        let newCommand = 
            <@
                fun (r: _1D) (sizes:array<_>) (a:array<_>) (b:array<_>) (c:array<_>) -> 
                    let gridSize = sizes.[0]
                    let matricesSize = sizes.[2]
                    let matricesCount = sizes.[3]
                    let matrixCellNum = matricesSize * matricesSize
                    let globalCellCount = matrixCellNum * matricesCount
                    let cellsToHandle = (globalCellCount - 1) / gridSize + 1
                    let firstToHandle = r.GlobalID0 * cellsToHandle 
                    if firstToHandle < globalCellCount
                    then
                        for currentCell in firstToHandle .. firstToHandle + cellsToHandle - 1 do                         
                            let num = currentCell / matrixCellNum
                            let ti = (currentCell - num * matrixCellNum) / matricesSize
                            let tj = currentCell - num * matrixCellNum - ti * matricesSize          
                            let skipMatrices = num * matricesSize * matricesSize
                            let skipRows = ti * matricesSize + skipMatrices
                            let skipCols = tj * matricesSize + skipMatrices
                            let mutable buf = (%zeroProbability)
                            for k in 0 .. (matricesSize - 1) do
                                buf <- (%summProbabilities) buf ((%multiplicateProbabilities) a.[skipRows + k] b.[skipCols + k])
                            c.[skipRows + tj] <- buf
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
            let realMaxMatrixSize = 
                match options.Cuda with
                | Some {MinMatrixSize = size} -> size / 2
                | None -> maxMatrixSize
            let maxMaxMatricesCount = maxMatrixSize / realMaxMatrixSize
            let maxAllocatedBitsForVar = maxMaxMatricesCount * realMaxMatrixSize * realMaxMatrixSize * 32 * ntsCount

            let maxAllocationBits = 
                match maxAllocationBits_ with
                | Some maxBits ->
                    // todo: split tasks
                    if 3 * maxAllocatedBitsForVar > maxBits 
                    then failwith "string size too big"
                    else maxBits
                | None ->
                    //todo: do something 
                    0
//                    failwith "not enough information"

            let bufferSize = maxAllocatedBitsForVar
            
            let floorToPowerOf2 x = 
//                let exp = (log (double x)) / (log 2.) |> floor |> int
                let exp = (log (double x)) / (log 2.) |> ceil |> int
                1 <<< exp

            let wgSizeRestriction = 
                match maxWGSize with
                    | Some size -> size
                    | None -> 4
            let localLinesRestriction = 
                match localMem with
                    // todo:
                    | Some size -> size / (32 * realMaxMatrixSize)
                    | None -> 32 * (1 <<< 20) / (32 * realMaxMatrixSize)
                                                
            // todo: sizes
            let gridSize = maxMatrixSize * 4
            let wgSize = min (floorToPowerOf2 wgSizeRestriction) <| min 128 (floorToPowerOf2 gridSize)
            let grid = new _1D(gridSize, wgSize)

            let mult1 = Array.create bufferSize <| Probability.InnerType.zero
            let mult2 = Array.create bufferSize <| Probability.InnerType.zero
            let result = Array.create bufferSize <| Probability.InnerType.zero
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
            let bufferSize = maxMatrixSize * maxMatrixSize * ntsCount
            let worker = Worker.Default
            {
                worker = worker
                options = cudaOptions
                mult1 = worker.Malloc(bufferSize)
                mult2 = worker.Malloc(bufferSize)
                result = worker.Malloc(bufferSize)
            }

        let createCpuHelper opt =
            let parallelSize i =
                match options.Parallel with 
                | Some { MinMatrixSize = size } -> size <= i
                | None -> false
            { IsParallel = parallelSize }

        let helpers = {
            _1DBrahma = 
                match options._1DBrahma with 
                | Some options -> 
                    if options.MinMatrixSize <= maxMatrixSize
                    then Some (Options.map (newCreateBrahmaHelper options.MinMatrixSize) options)
                    else None
                | None -> None
            Brahma = Option.map (Options.map createBrahmaHelper) options.Brahma
            Cuda = Option.map (Options.map createCudaHelper) options.Cuda
            Fast = Option.map (Options.map createCpuHelper) options.Fast
            Parallel = Option.map id options.Parallel
            Simple = Some { MinMatrixSize = 1; Options = createCpuHelper options.Fast }
        }

        let simpleMultiplicate (from1: Probability.InnerType.T []) (from2: Probability.InnerType.T []) matricesSize actualColCount =        
            let calculateCell (n, i, j) = 
                let skipMatrices = n * matricesSize * matricesSize
                let skipRows = skipMatrices + i * matricesSize
                let skipColumns = skipMatrices + j * matricesSize                
                Array.fold2 (fun b v1 v2 -> Probability.innerSumm b <| Probability.innerMult v1 v2)
                            Probability.InnerType.zero
                            from1.[skipRows..skipRows + matricesSize - 1] 
                            from2.[skipColumns..skipColumns + matricesSize - 1]

            // todo: recurr
            let getNIJ x = 
                let n = x / (matricesSize * matricesSize)
                let i = (x - n * matricesSize * matricesSize) / matricesSize
                let j = x - n * matricesSize * matricesSize - i * matricesSize
                n, i, j
            Array.init (matricesSize * actualColCount) (fun x -> calculateCell <| getNIJ x)

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

        let doCublasMultiplications helpers (getTMatrix: _ -> ProbabilityMatrix.T) (getPMatrix: _ -> ProbabilityMatrix.T) matricesSize (fullTasks: _ []) = 
            let matricesCount = fullTasks.Length
            let matricesCellCount = matricesSize * matricesSize
            
            let from1 = 
                fullTasks
                |> Array.map (fun ({ from1 = from }, nts) -> (getTMatrix <| fst nts).GetSubArray Probability.InnerType.unwrap false from)
                |> Array.concat
            
            let from2 = 
                fullTasks
                |> Array.map (fun ({ from2 = from }, nts) -> (getTMatrix <| snd nts).GetSubArray Probability.InnerType.unwrap false from)
                |> Array.concat

            let transa = cublasOperation_t.CUBLAS_OP_N
            let transb = cublasOperation_t.CUBLAS_OP_N
                            
            let dalpha = 1.
            let dbeta = 0.

            let multiplicationResult = 
                if gpuMutex.WaitOne()
                then        
                    helpers.mult1.Scatter(from1) 
                    helpers.mult2.Scatter(from2) 

                    let doOne i =
                        let ptrShift = i * matricesCellCount
                        CUBLAS.Default.Dgemm(transa, 
                                             transb, 
                                             matricesSize, 
                                             matricesSize, 
                                             matricesSize, 
                                             dalpha, 
                                             helpers.mult2.Ptr + ptrShift, 
                                             matricesSize, 
                                             helpers.mult1.Ptr + ptrShift, 
                                             matricesSize, 
                                             dbeta, 
                                             helpers.result.Ptr + ptrShift, 
                                             matricesSize)

                    [|0..matricesCount - 1|]
                    |> Array.iter doOne      
            
                    let result = helpers.result.Gather()  
                    gpuMutex.ReleaseMutex()
                    result 
                else 
                    failwith "cant get gpuMutex" 

            let flushOneMatrix num = 
                let matrix = 
                    multiplicationResult.[num * matricesCellCount .. (num + 1) * matricesCellCount - 1]
                let task, nts = fullTasks.[num]
                (getPMatrix nts).AddSubmatrixByGetter task.where (fun cell -> Probability.create matrix.[task.where.XByCell cell])   
            
            [|0..matricesCount - 1|]
            |> (if  helpers.options.doParallelFlush then Array.Parallel.iter else Array.iter) flushOneMatrix   
    

        let gpuMultiplicate helpers
                            (from1: Probability.InnerType.T []) 
                            (from2: Probability.InnerType.T []) 
                            matricesCount 
                            matricesSize =
               
            let result: Probability.InnerType.T [] = Array.zeroCreate(matricesCount * matricesSize * matricesSize)

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
                               
            // todo: local size
            let wgSizeX, wgSizeY = wgSizesForMatricesWise wgSizeRestriction localLinesRestriction matricesCount matricesSize 
                                
            let d = new _2D(wgItemsXCount, wgItemsYCount, wgSizeX, wgSizeY)

            if gpuMutex.WaitOne()
            then
                helpers.kernelPrepare d matricesSize from1 from2 result
         
                helpers.commandQueue.Add(helpers.kernelRun()).Finish() |> ignore        
                helpers.commandQueue.Add(result.ToHost helpers.provider).Finish() |> ignore
                gpuMutex.ReleaseMutex()
            else 
                failwith "cant get gpuMutex"
                        
            result
            
        let doSimpleMultiplications helpers (getTMatrix: _ -> ProbabilityMatrix.T) (getPMatrix: _ -> ProbabilityMatrix.T) matricesSize fullTasks = 
            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = (getTMatrix nt1).GetSubArray id false task.from1
                let from2 = (getTMatrix nt2).GetSubArray id true  task.from2
                let resultArray = simpleMultiplicate from1 from2 matricesSize matricesSize
                let result = ProbabilityMatrix.create matricesSize resultArray
                (getPMatrix nts).AddSubmatrix task.where result 
            
            fullTasks
            |> (if helpers.IsParallel matricesSize then Array.Parallel.iter else Array.iter) doOne 
            
        let doFastMultiplications helpers (getTMatrix: _ -> ProbabilityMatrix.T) (getPMatrix: _ -> ProbabilityMatrix.T) matricesSize fullTasks = 
                      
            let getFastSubMatrix nt (submatrix: SubMatrix.T) =
                let valueGetter = (getTMatrix nt).SubMatrixValuesGetter false submatrix
                Array2D.init submatrix.Size submatrix.Size (fun x y -> Cell.create x y |> valueGetter |> float)

            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = getFastSubMatrix nt1 task.from1
                let from2 = getFastSubMatrix nt2 task.from2
                
                let result =  Array2D.create task.from1.Size task.from1.Size 0.0
                for i in 0 .. task.from1.Size - 1 do
                   for j in 0 .. task.from1.Size - 1 do
                     for k in 0 .. task.from1.Size - 1 do
                       result.[i,j] <- result.[i,j] + from1.[i,k] * from2.[k,j]

                (getPMatrix nts).AddSubmatrixByGetter task.where (fun cell -> Probability.create result.[cell.Row, cell.Column])  


            fullTasks
            |> (if helpers.IsParallel matricesSize then Array.Parallel.iter else Array.iter) doOne 

        let _1DDoBrahmaMultiplications (helpers: NewGPUBrahma) (getTMatrix: _ -> ProbabilityMatrix.T) (getPMatrix: _ -> ProbabilityMatrix.T) matricesSize (fullTasks: _ []) = 
            let matricesCount = fullTasks.Length
            let matricesCellCount = matricesSize * matricesSize

            if gpuMutex.WaitOne()
            then
                fullTasks
                |> Array.iteri (fun i ({ from1 = from }, nts) -> (getTMatrix <| fst nts).CopyToArray id false from helpers.mult1 (i * matricesCellCount))
            
                fullTasks
                |> Array.iteri (fun i ({ from2 = from }, nts) -> (getTMatrix <| snd nts).CopyToArray id true  from helpers.mult2 (i * matricesCellCount))

                helpers.sizes.[2] <- int32 matricesSize
                helpers.sizes.[3] <- int32 matricesCount

                helpers.commandQueue.Add(helpers.mult1.ToGpu(helpers.provider)) |> ignore   
                helpers.commandQueue.Add(helpers.mult2.ToGpu(helpers.provider)) |> ignore  
                helpers.commandQueue.Add(helpers.sizes.ToGpu(helpers.provider)) |> ignore          
                helpers.commandQueue.Add(helpers.kernelRun()) |> ignore        
                helpers.commandQueue.Add(helpers.result.ToHost helpers.provider).Finish() |> ignore

                let flushOneMatrix num = 
                    let matrix = 
                        helpers.result.[num * matricesCellCount .. (num + 1) * matricesCellCount - 1]
                        |> ProbabilityMatrix.create matricesSize
                    let task, nts = fullTasks.[num]
                    (getPMatrix nts).AddSubmatrix task.where matrix          
            
                [|0..matricesCount - 1|]
                |> (if helpers.options.doParallelFlush then Array.Parallel.iter else Array.iter) flushOneMatrix

                gpuMutex.ReleaseMutex()
            else 
                failwith "cant get gpuMutex"
            
        let doBrahmaMultiplications helpers (getTMatrix: _ -> ProbabilityMatrix.T) (getPMatrix: _ -> ProbabilityMatrix.T) matricesSize (fullTasks: _ []) = 
            let matricesCount = fullTasks.Length
            let matricesCellCount = matricesSize * matricesSize
            
            let from1 = 
                fullTasks
                |> Array.map (fun ({ from1 = from }, nts) -> (getTMatrix <| fst nts).GetSubArray id false from)
                |> Array.concat
            
            let from2 = 
                fullTasks
                |> Array.map (fun ({ from2 = from }, nts) -> (getTMatrix <| snd nts).GetSubArray id true  from)
                |> Array.concat
                
            let multiplicationResult = gpuMultiplicate helpers from1 from2 matricesCount matricesSize

            let flushOneMatrix num = 
                let matrix = 
                    multiplicationResult.[num * matricesCellCount .. (num + 1) * matricesCellCount - 1]
                    |> ProbabilityMatrix.create matricesSize
                let task, nts = fullTasks.[num]
                (getPMatrix nts).AddSubmatrix task.where matrix          
            
            [|0..matricesCount - 1|]
            |> (if helpers.options.doParallelFlush then Array.Parallel.iter else Array.iter) flushOneMatrix
  
        member this.performMultiplication getTMatrix (getPMatrix: NonTerminal * NonTerminal -> ProbabilityMatrix.T) (tasks: MultiplicationTask []) nts = 

            let crossproduct l1 l2 =
                let product lst v = Array.map (fun vl -> (vl, v)) lst
                Array.collect (product l1) l2

            let fullTasks = crossproduct tasks nts

            let matricesSize = tasks.[0].where.Size

            let doCheck doMultiplication (helpers_: _ Info option) isDone = 
                if not isDone 
                then 
                    match helpers_ with
                    | Some helpers ->            
                        if matricesSize >= helpers.MinMatrixSize
                        then 
                            doMultiplication helpers.Options getTMatrix getPMatrix matricesSize fullTasks
                            true
                        else false
                    | None -> false
                else isDone           

            false 
            |> doCheck doCublasMultiplications helpers.Cuda
            |> doCheck doBrahmaMultiplications helpers.Brahma
            |> doCheck _1DDoBrahmaMultiplications helpers._1DBrahma
            |> doCheck doFastMultiplications helpers.Fast
            |> doCheck doSimpleMultiplications helpers.Simple
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




    type TMatrix = Dictionary<NonTerminal, ProbabilityMatrix.T>
    type PMatrix = Dictionary<NonTerminal * NonTerminal, ProbabilityMatrix.T>
       
    type MatrixHolder(tKeys, pKeys, stringSize, options: Options.T) = 

        let matrixSizeExponent = log2 <| stringSize + 1
        
        let GPUMultiplicator = 
            // todo: think about search size!!!!!!!!!!            
            let maxMatrixForMultiplicationSize = (1 <<< (matrixSizeExponent - 1))
            MatricesMultiplicator (options, 
                                    Probability.innerSummQuote, 
                                    Probability.innerMultQuote, 
                                    Probability.InnerType.zeroQuote, 
                                    maxMatrixForMultiplicationSize, 
                                    Array.length pKeys)

        let tMatrix = new TMatrix ()
        do 
            (
                tKeys 
                |> Seq.map (fun x -> x, ProbabilityMatrix.empty (stringSize + 1))
            )
            |> Seq.iter tMatrix.Add                            

        let pMatrix = new PMatrix ()
        do 
            (
                pKeys
                |> Array.map (fun x -> x, ProbabilityMatrix.empty (stringSize + 1))
            )
            |> Seq.iter pMatrix.Add

        let multiplicationCounter = 
            match options.mode with
            | Work -> None
            | Test -> Some <| Array.zeroCreate matrixSizeExponent

        member this.MultiplicationCounter = multiplicationCounter

        member this.updateTCellWith cell nonterminals =
            nonterminals |> List.iter (fun (key, prob) -> tMatrix.[key].AddValueToCell cell prob)
            
        member this.getProbabilities nt = tMatrix.[nt]

        member this.initTDiagonalWith nonterminals =
            let diagonalCell i = Cell.create i (i + 1)
            nonterminals |> List.iteri (fun i ntProbs -> this.updateTCellWith (diagonalCell i) ntProbs)

        member this.refreshTCells headProbsFromTail cells =
                let heads cell = 
                    pMatrix
                    |> List.ofSeq
                    |> List.map (fun pair -> (pair.Key, pair.Value.[cell]))
                    |> List.filter (fun (_, prob) -> not <| Probability.isZero prob)
                    |> List.map headProbsFromTail 
                    |> List.concat

                cells
                |> Array.iter (fun cell -> heads cell |> this.updateTCellWith cell)

        member this.performMultiplication tasks nts = 
            match options.mode with
            | Work ->             
                let getPMatrix (nts: NonTerminal * NonTerminal) : ProbabilityMatrix.T = pMatrix.[nts]             
                let getTMatrix nt = tMatrix.[nt]
                
                GPUMultiplicator.performMultiplication getTMatrix getPMatrix tasks nts
            | Test -> 
                let size = tasks.[0].where.Size
                multiplicationCounter
                |> Option.iter (fun arr -> arr.[log2 size] <- (arr.[log2 size] + tasks.Length ))
                
            
        member this.releaseResources = GPUMultiplicator.releaseResources