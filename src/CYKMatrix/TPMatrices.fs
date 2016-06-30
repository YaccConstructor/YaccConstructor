module TPMatrices

    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Math

    open Brahma.Helpers
    open OpenCL.Net
    open Brahma.OpenCL
    open Brahma.FSharp.OpenCL.Core
    open Microsoft.FSharp.Quotations
    open Brahma.FSharp.OpenCL.Extensions

    open Util

    type GPUHelpers = {
        provider: ComputeProvider
        commandQueue: CommandQueue
        kernel: _2D Kernel
        kernelPrepare: _2D -> int -> Probability.InnerType [] -> Probability.InnerType [] -> Probability.InnerType [] -> unit
        kernelRun: unit -> _2D Commands.Run
        options: GPUOptions
    }

    type GPUMatriceswMultiplicator (options: MultiplicationOptions, probabilitySummQuote, probabilityMultQuote) =

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

                Some {
                    provider = provider; 
                    commandQueue = commandQueue; 
                    kernel = kernel;
                    kernelPrepare = kernelPrepare;
                    kernelRun = kernelRun;
                    options = gpuOptions
                }
            | None -> None      
            
        let flushMultiplicationResults addToPSubMatrix fullTasks matrices =
            Array.zip fullTasks matrices
            |> Array.iter (fun (fullTask, matrix) ->
                               let task, nts = fullTask
                               addToPSubMatrix task.where nts matrix)

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

        let gpuMultiplicate (helpers: GPUHelpers) 
                            (from1: Probability.InnerType []) 
                            (from2: Probability.InnerType []) 
                            matricesCount 
                            matricesSize =
               
            let result: Probability.InnerType [] = Array.zeroCreate(matricesCount * matricesSize * matricesSize)
                              
            let d = new _2D(matricesSize * matricesCount, matricesSize, min 4 (matricesSize * matricesCount), 1)
    //            let d = new _2D(matricesSize * matricesCount, matricesSize, min 4 (matricesSize * matricesCount), 1)

            helpers.kernelPrepare d matricesSize from1 from2 result
         
            helpers.commandQueue.Add(helpers.kernelRun()).Finish() |> ignore        
            helpers.commandQueue.Add(result.ToHost helpers.provider).Finish() |> ignore

            result

        let doSimpleMultiplications getTSubmatrix addToPSubMatrix matricesSize fullTasks = 
            let doOne (task, nts) =
                let nt1, nt2 = nts
                let from1 = getTSubmatrix nt1 task.from1 false
                let from2 = getTSubmatrix nt2 task.from2 true
                simpleMultiplicate from1 from2 matricesSize matricesSize

            fullTasks
            |> Array.map doOne
            |> Array.map (ProbabilityMatrix.create matricesSize matricesSize)
            |> flushMultiplicationResults addToPSubMatrix fullTasks

        let doGPUMultiplications getTSubmatrix addToPSubMatrix matricesCount matricesSize helpers fullTasks = 
            let matricesCellCount = matricesSize * matricesSize

            let addOneFromToBuffer doTranspone buffer (from: SubMatrix.T, nt) = 
                getTSubmatrix nt from doTranspone
                |> Array.append buffer

            let from1 = 
                fullTasks 
                |> Array.map (fun ({from1 = from}, nts) -> from, fst nts)
                |> Array.fold (addOneFromToBuffer false) Array.empty
            let from2 = 
                fullTasks 
                |> Array.map (fun ({from2 = from}, nts) -> from, snd nts)
                |> Array.fold (addOneFromToBuffer true) Array.empty 

            let multiplicationResult = gpuMultiplicate helpers from1 from2 matricesCount matricesSize

            fullTasks 
            |> Array.mapi (fun num _ -> multiplicationResult.[num * matricesCellCount..(num + 1) * matricesCellCount - 1])
            |> Array.map (ProbabilityMatrix.create matricesSize matricesSize)
            |> flushMultiplicationResults addToPSubMatrix fullTasks

        member this.performMultiplication getTSubmatrix addToPSubMatrix (tasks: MultiplicationTask []) nts = 
//            multiplicationCounter := !multiplicationCounter + (Array.length tasks)

            let crossproduct l1 l2 =
                let product lst v = Array.map (fun vl -> (vl, v)) lst
                Array.collect (product l1) l2

            let fullTasks = crossproduct tasks nts

            let matricesSize = tasks.[0].where.Size
            let matricesCount = fullTasks.Length
            let matricesCellCount = matricesSize * matricesSize
            
            // todo: last matrix reduced size !!!!!!!!!!!!
            match gpuHelpers with
            | Some helpers ->
                if matricesSize >= helpers.options.MinMatrixSize
                then doGPUMultiplications getTSubmatrix addToPSubMatrix matricesCount matricesSize helpers fullTasks
                else doSimpleMultiplications getTSubmatrix addToPSubMatrix matricesSize fullTasks 
            | None -> doSimpleMultiplications getTSubmatrix addToPSubMatrix matricesSize fullTasks 
                

        member this.releaseResources () =
            match gpuHelpers with
            | Some helpers ->
                helpers.commandQueue.Dispose()
                helpers.provider.Dispose()
                helpers.provider.CloseAllBuffers()
            // todo: hmmm...
            | None -> ignore 0




       
    type MatrixHolder(tKeys, pKeys, stringSize, options: MultiplicationOptions) = 
    
        let GPUMultiplicator = new GPUMatriceswMultiplicator (options, Probability.innerSummQuote, Probability.innerMultQuote)
            
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
   
        let addToTMatrix cell nontermProbs =
            nontermProbs |> List.iter (fun (key, prob) -> tMatrix.[key].AddValueToCell cell prob)

        let addToPSubMatrix (where: SubMatrix.T) nts (matrix: ProbabilityMatrix.T) =
                let whereMatrix = pMatrix.[nts]
                for i in [0 .. where.Size - 1] do
                    let actualColCount = (min (where.Top.Column) (stringSize + 1)) - where.Left.Column
                    for j in [0 .. actualColCount - 1] do
                        let matrixCell = Cell.create i j
                        let realCell = Cell.shift matrixCell where.Left.Row where.Left.Column
                        whereMatrix.AddValueToCell realCell matrix.[matrixCell]
            
        member this.getProbabilities nt = tMatrix.[nt]

        member this.initTDiagonalWith nonterminals =
            let diagonalCell i = Cell.create i (i + 1)
            nonterminals |> List.iteri (fun i ntProbs -> addToTMatrix (diagonalCell i) ntProbs)

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
                |> Array.iter (fun cell -> heads cell |> addToTMatrix cell)
                
        member this.performMultiplication tasks nts = 
            let getTSubMatrix nt = tMatrix.[nt].GetInnerSubMatrix
            GPUMultiplicator.performMultiplication getTSubMatrix addToPSubMatrix tasks nts

        member this.releaseResources = GPUMultiplicator.releaseResources