module TPMatrices 

open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Quotations
open System.Collections.Generic

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
          options: Util.GPUCuda }
    
    type CPUFast = Unit
    type CPUSimple = Unit
    type CPUParallel = Unit
    
    type Helpers = {
        _1DBrahma: NewGPUBrahma Info option 
        Brahma: GPUBrahma Info option  
        Cuda: GPUCuda Info option  
        Fast: CPUFast Info option
        Simple: CPUSimple Info option
        Parallel: CPUParallel Info option
    }


    type MatriceswMultiplicator (options: Options.T, summProbabilities, multiplicateProbabilities, zeroProbability, maxMatrixSize, ntsCount) =

    
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
        //                    let mutable buf = c.[skipRows + tj]
        //                    let k = ref 0
        //                    while (!k < matricesSize) do 
                            for k in 0 .. (matricesSize - 1) do
                                buf <- (+) buf ((*) a.[skipRows + k] b.[skipCols + k])
        //                        k := !k + 1
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
//                    let mutable buf = c.[skipRows + tj]
//                    let k = ref 0
//                    while (!k < matricesSize) do 
                    for k in 0 .. (matricesSize - 1) do
                        buf <- (%summProbabilities) buf ((%multiplicateProbabilities) a.[skipRows + k] b.[skipCols + k])
//                        k := !k + 1
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
            let gridSize = maxMatrixSize
            let wgSize = 128
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
            {
                worker = Worker.Default
                options = cudaOptions
            }

        let helpers = {
            _1DBrahma = 
                match options._1DBrahma with 
                | Some options -> Some (Options.map (newCreateBrahmaHelper options.MinMatrixSize) options)
                | None -> None
            Brahma = Option.map (Options.map createBrahmaHelper) options.Brahma
            Cuda = Option.map (Options.map createCudaHelper) options.Cuda
            Fast = Option.map id options.Fast
            Parallel = Option.map id options.Parallel
            Simple = Some { MinMatrixSize = 1; Options = () }
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

        let doCublasMultiplications (getMatrix: NonTerminal -> ProbabilityMatrix.T) flushSubMatrix (matricesCount: int) matricesSize fullTasks helpers =
            let matricesCellCount = matricesSize * matricesSize
            
            let from1 = 
                fullTasks
                |> Array.map (fun ({ from1 = from }, nts) -> (getMatrix <| fst nts).GetSubArray Probability.InnerType.unwrap from false)
                |> Array.concat
            
            let from2 = 
                fullTasks
                |> Array.map (fun ({ from2 = from }, nts) -> (getMatrix <| snd nts).GetSubArray Probability.InnerType.unwrap from false)
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
                        
            result
            
        let doSimpleMultiplications isParallel (getMatrix: NonTerminal -> ProbabilityMatrix.T) flushSubMatrix matricesSize fullTasks = 
            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = (getMatrix nt1).GetSubArray id task.from1 false
                let from2 = (getMatrix nt2).GetSubArray id task.from2 true
                let resultArray = simpleMultiplicate from1 from2 matricesSize matricesSize
                let result = ProbabilityMatrix.create matricesSize matricesSize resultArray
                flushSubMatrix task.where nts result 
            
            fullTasks
            |> (if isParallel then Array.Parallel.iter else Array.iter) doOne 
            
        let doFastMultiplications isParallel (getMatrix: NonTerminal -> ProbabilityMatrix.T) flushSubMatrix matricesSize fullTasks = 
                      
            let getFastSubMatrix nt (submatrix: SubMatrix.T) =
                let valueGetter = (getMatrix nt).SubMatrixValuesGetter submatrix false                        
                Matrix.init submatrix.Size submatrix.Size (fun x y -> Cell.create x y |> valueGetter |> float)

            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = getFastSubMatrix nt1 task.from1
                let from2 = getFastSubMatrix nt2 task.from2
                let result = from1 * from2                
                flushSubMatrix task.where nts result 
            
            fullTasks
            |> (if isParallel then Array.Parallel.iter else Array.iter) doOne 

        let _1DDoBrahmaMultiplications (getMatrix: NonTerminal -> ProbabilityMatrix.T) flushSubMatrix matricesCount matricesSize fullTasks helpers = 
            let matricesCellCount = matricesSize * matricesSize
            
            let cellGetter (from: SubMatrix.T) isTransponed = 
                let leftCell = from.Left

                from.CellByX
                >> if isTransponed then Cell.transpone else id 
                >> Cell.shift leftCell.Row leftCell.Column

            let copyMatrix nt getCell (where: _ []) skip =
                [0..matricesCellCount - 1]
                |> List.iter (fun x -> where.[skip + x] <- getCell x |> (getMatrix nt).GetInnerFromCell)

            fullTasks
            |> Array.iteri (fun i ({ from1 = from }, nts) -> copyMatrix (fst nts) (cellGetter from false) helpers.mult1 (i * matricesCellCount))
            
            fullTasks
            |> Array.iteri (fun i ({ from2 = from }, nts) -> copyMatrix (snd nts) (cellGetter from true) helpers.mult2 (i * matricesCellCount))

            [0..matricesCellCount * matricesCount - 1]
            |> List.iter (fun i -> helpers.result.[i] <- Probability.InnerType.zero)

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
            
        let doBrahmaMultiplications (getMatrix: NonTerminal -> ProbabilityMatrix.T) flushSubMatrix matricesCount matricesSize fullTasks helpers = 
//            let (getMatrix: NonTerminal -> ProbabilityMatrix.T), flushSubMatrix = matrixHandler
            let matricesCellCount = matricesSize * matricesSize
            
            let from1 = 
                fullTasks
                |> Array.map (fun ({ from1 = from }, nts) -> (getMatrix <| fst nts).GetSubArray id from false)
                |> Array.concat
            
            let from2 = 
                fullTasks
                |> Array.map (fun ({ from2 = from }, nts) -> (getMatrix <| snd nts).GetSubArray id from true)
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
  
//        member this.performMultiplication arrayMatrixHandler fastMatrixHandler cublasMatrixHandler (tasks: MultiplicationTask []) nts = 
        member this.performMultiplication getTMatrix flushArrayMatrix flushFastMatrix flushCublasMatrix (tasks: MultiplicationTask []) nts = 

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
            
            let doFast _ = doFastMultiplications isParallel getTMatrix flushFastMatrix matricesSize fullTasks 
            let doSimple _  = doSimpleMultiplications isParallel getTMatrix flushArrayMatrix matricesSize fullTasks 
            let doBrahma helpers = doBrahmaMultiplications getTMatrix flushArrayMatrix matricesCount matricesSize fullTasks helpers
            let do_1DBrahma helpers = _1DDoBrahmaMultiplications getTMatrix flushArrayMatrix matricesCount matricesSize fullTasks helpers
            let doCublas helpers = doCublasMultiplications getTMatrix flushCublasMatrix matricesCount matricesSize fullTasks helpers

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
            |> doCheck do_1DBrahma helpers._1DBrahma      
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



    type TMatrix = Dictionary<NonTerminal, ProbabilityMatrix.T>
    type PMatrix = Dictionary<NonTerminal * NonTerminal, ProbabilityMatrix.T>
       
    type MatrixHolder(tKeys, pKeys, stringSize, options: Options.T) = 
        
        let GPUMultiplicator = 
            // todo: think about search size!!!!!!!!!!
            let matrixSizeExponent = (log (double stringSize + 1.)) / (log 2.) |> ceil |> int
            let maxMatrixForMultiplicationSize = (1 <<< (matrixSizeExponent - 1))
            MatriceswMultiplicator (options, 
                                    Probability.innerSummQuote, 
                                    Probability.innerMultQuote, 
                                    Probability.InnerType.zeroQuote, 
                                    maxMatrixForMultiplicationSize, 
                                    Array.length pKeys)

        // swich to dictionary (?)
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
            
            let flushArrayMatrix (where: SubMatrix.T) nts (matrix: ProbabilityMatrix.T) = addToPMatrix where nts (fun cell -> matrix.[cell])
            let flushCublasMatrix (where: SubMatrix.T) nts (matrix: float []) =
                let getValueFromCell (cell: Cell.T) =
                    let x = cell.Row * where.Size + cell.Column
                    Probability.create matrix.[x]
                addToPMatrix where nts getValueFromCell
            let flushFastMatrix (where: SubMatrix.T) nts (matrix: Matrix<float>) = 
                addToPMatrix where nts (fun cell -> Probability.create matrix.[cell.Row, cell.Column])         

            let getInnerMatrix nt = tMatrix.[nt]
                
            GPUMultiplicator.performMultiplication getInnerMatrix flushArrayMatrix flushFastMatrix flushCublasMatrix tasks nts
            
        member this.releaseResources = GPUMultiplicator.releaseResources