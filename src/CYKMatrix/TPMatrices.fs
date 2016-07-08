module TPMatrices

open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.OpenCL

open Util

    type GPUHelpers = 
        { provider: ComputeProvider
          commandQueue: CommandQueue
          localMemory: int option
          maxWorkGroupSize: int option
          kernel: _2D Kernel
          kernelPrepare: _2D -> int -> Probability.InnerType [] -> Probability.InnerType [] -> Probability.InnerType [] -> unit
          kernelRun: unit -> _2D Commands.Run
          options: GPUOptions }

    type MatriceswMultiplicator (options: Options.T, probabilitySummQuote, probabilityMultQuote) =

        let multiplicateProbabilities = probabilityMultQuote
        let summProbabilities = probabilitySummQuote

        let command = 
            <@
                fun (r:_2D) matricesSize (a:array<_>) (b:array<_>) (c:array<_>) -> 
                    let num = r.GlobalID0 / matricesSize
                    let ti = r.GlobalID0 - num * matricesSize
                    let tj = r.GlobalID1                 
                    let skipMatrices = num * matricesSize * matricesSize
                    let skipRows = ti * matricesSize + skipMatrices
                    let skipCols = tj * matricesSize + skipMatrices
                    // todo: innerZero
                    let mutable buf = c.[skipRows + tj]
                    for k in 0 .. matricesSize - 1 do
                        buf <- (%summProbabilities) buf ((%multiplicateProbabilities) a.[skipRows + k] b.[skipCols + k])
                    c.[skipRows + tj] <- buf
            @>  

        let gpuHelpers = 
            match options.GPU with
            | Some gpuOptions -> 
                let provider =
                    try ComputeProvider.Create(gpuOptions.PlatformName, gpuOptions.DeviceType)
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

                Some {
                    provider = provider; 
                    localMemory = localMem;
                    maxWorkGroupSize = maxWGSize;
                    commandQueue = commandQueue; 
                    kernel = kernel;
                    kernelPrepare = kernelPrepare;
                    kernelRun = kernelRun;
                    options = gpuOptions
                }
            | None -> None      

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

        let wgSizesLineWise wgSizeRestriction localLinesRestriction matricesCount matricesSize = 
            if wgSizeRestriction < matricesSize || localLinesRestriction < matricesSize + 1
            then 
                let maxWgSize = min wgSizeRestriction (localLinesRestriction - 1)
                matricesSize / (ceilToPowerOf2 <| matricesSize / maxWgSize), 1          
            else if wgSizeRestriction < matricesSize * matricesSize || localLinesRestriction < 2 * matricesSize 
            then 
                let maxRightLines = min (wgSizeRestriction / matricesSize) (localLinesRestriction - matricesSize)
                matricesSize, floorToPowerOf2 maxRightLines
            else 
                let coeff = floorToPowerOf2 
                            <| min (wgSizeRestriction / (matricesSize * matricesSize)) 
                                    (localLinesRestriction / (2 * matricesSize))
                // coeff should divide matrixCount
                let coeff = 1
                coeff * matricesSize, matricesSize

        let wgSizesIgnoringLocalMemoryRestrictions wgSizeRestriction localLinesRestriction matricesCount matricesSize = 
            if wgSizeRestriction < matricesSize
            then 
                matricesSize / (ceilToPowerOf2 <| matricesSize / wgSizeRestriction), 1          
            else if wgSizeRestriction < matricesSize * matricesSize
            then 
                let maxRightLines = wgSizeRestriction / matricesSize
                matricesSize, floorToPowerOf2 maxRightLines
            else 
                let coeff = floorToPowerOf2 
                            <| wgSizeRestriction / (matricesSize * matricesSize)    
                // coeff should divide matrixCount
                let coeff = 1                                 
                coeff * matricesSize, matricesSize


                
        let gpuMultiplicate (helpers: GPUHelpers) 
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

            let wgSizeX, wgSizeY = wgSizesForMatricesWise wgSizeRestriction localLinesRestriction matricesCount matricesSize 
//            let wgSizeX, wgSizeY = if wgSizeY_ < 2 then wgSizeX_, wgSizeY_ else wgSizeX_ / 2, wgSizeY_ / 2
                                                   
//            let d = new _2D(wgItemsXCount, wgItemsYCount, 1, 1)
            let d = new _2D(wgItemsXCount, wgItemsYCount, wgSizeX, wgSizeY)

            helpers.kernelPrepare d matricesSize from1 from2 result
         
            helpers.commandQueue.Add(helpers.kernelRun()).Finish() |> ignore        
            helpers.commandQueue.Add(result.ToHost helpers.provider).Finish() |> ignore

            result

        let doSimpleMultiplications isParallel arrayMatrixHandler matricesSize fullTasks = 
            let getSubMatrix, flushSubMatrix = arrayMatrixHandler
            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = getSubMatrix nt1 task.from1 false
                let from2 = getSubMatrix nt2 task.from2 true
                let resultArray = simpleMultiplicate from1 from2 matricesSize matricesSize
                let result = ProbabilityMatrix.create matricesSize matricesSize resultArray
                flushSubMatrix task.where nts result 
            
            fullTasks
            |> (if isParallel then Array.Parallel.iter else Array.iter) doOne 
            
        let doFastMultiplications isParallel fastMatrixHandler matricesSize fullTasks = 
            let getSubMatrix, flushSubMatrix = fastMatrixHandler
            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = getSubMatrix nt1 task.from1 false
                let from2 = getSubMatrix nt2 task.from2 false
                let result = from1 * from2                
                flushSubMatrix task.where nts result 
            
            fullTasks
            |> (if isParallel then Array.Parallel.iter else Array.iter) doOne 

        let doGPUMultiplications arrayMatrixHandler matricesCount matricesSize helpers fullTasks = 
            let getSubMatrix, flushSubMatrix = arrayMatrixHandler
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
                
            let doParallelFlush = 
                match gpuHelpers with
                | Some helpers -> helpers.options.doParallelFlush
                | None -> failwith "imposibru"        
            
            [|0..matricesCount - 1|]
            |> (if doParallelFlush then Array.Parallel.iter else Array.iter) flushOneMatrix

        member this.performMultiplication arrayMatrixHandler fastMatrixHandler (tasks: MultiplicationTask []) nts = 

            let crossproduct l1 l2 =
                let product lst v = Array.map (fun vl -> (vl, v)) lst
                Array.collect (product l1) l2

            let fullTasks = crossproduct tasks nts

            let matricesSize = tasks.[0].where.Size
            let matricesCount = fullTasks.Length
            let matricesCellCount = matricesSize * matricesSize
            
            let doFastParallel () = doFastMultiplications true fastMatrixHandler matricesSize fullTasks 
            let doFast () = doFastMultiplications false fastMatrixHandler matricesSize fullTasks 
            let doSimpleParallel () = doSimpleMultiplications true arrayMatrixHandler matricesSize fullTasks 
            let doSimple () = doSimpleMultiplications false arrayMatrixHandler matricesSize fullTasks 
            let doGPU helpers = doGPUMultiplications arrayMatrixHandler matricesCount matricesSize helpers fullTasks

            let doCheckParallel doOneThread doParallel  =
                match options.Parallel with 
                | Some { MinMatrixSize = size } -> 
                    if matricesSize >= size
                    then doParallel ()
                    else doOneThread ()
                | None -> doOneThread ()
                            
            match gpuHelpers with
            | Some helpers ->
                if matricesSize >= helpers.options.MinMatrixSize
                then doGPU helpers
                else doCheckParallel doSimple doSimpleParallel                
            | None -> 
            match options.Fast with
            | Some { MinMatrixSize = size } -> 
                if matricesSize >= size
                then doCheckParallel doFast doFastParallel
                else doCheckParallel doSimple doSimpleParallel 
            | None -> doCheckParallel doSimple doSimpleParallel
                

        member this.releaseResources () =
            match gpuHelpers with
            | Some helpers ->
                helpers.commandQueue.Dispose()
                helpers.provider.Dispose()
                helpers.provider.CloseAllBuffers()
            // todo: hmmm...
            | None -> ignore 0




       
    type MatrixHolder(tKeys, pKeys, stringSize, options: Options.T) = 
    
        let GPUMultiplicator = new MatriceswMultiplicator (options, Probability.innerSummQuote, Probability.innerMultQuote)
            
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

        // todo: recurring code
        let addToPSubMatrix (where: SubMatrix.T) nts (matrix: ProbabilityMatrix.T) =
            let whereMatrix = pMatrix.[nts]
            for i in [0 .. where.Size - 1] do
                let actualColCount = (min (where.Top.Column) (stringSize + 1)) - where.Left.Column
                for j in [0 .. actualColCount - 1] do
                    let matrixCell = Cell.create i j
                    let realCell = Cell.shift where.Left.Row where.Left.Column matrixCell
                    whereMatrix.AddValueToCell realCell matrix.[matrixCell]

        let addToPFastMatrix (where: SubMatrix.T) nts (matrix: Matrix<float>) =
            let whereMatrix = pMatrix.[nts]
            for i in [0 .. where.Size - 1] do
                let actualColCount = (min (where.Top.Column) (stringSize + 1)) - where.Left.Column
                for j in [0 .. actualColCount - 1] do
                    let matrixCell = Cell.create i j
                    let realCell = Cell.shift where.Left.Row where.Left.Column matrixCell
                    whereMatrix.AddValueToCell realCell (Probability.create matrix.[i,j])
                    
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
            let getTSubMatrix nt = tMatrix.[nt].GetInnerSubMatrix
            let getFastSubMatrix nt = tMatrix.[nt].GetFastSubMatrix
            let arrayMatrixHandler = (getTSubMatrix, addToPSubMatrix)
            let fastMatrixHandler = (getFastSubMatrix, addToPFastMatrix)
            GPUMultiplicator.performMultiplication arrayMatrixHandler fastMatrixHandler tasks nts

        member this.releaseResources = GPUMultiplicator.releaseResources