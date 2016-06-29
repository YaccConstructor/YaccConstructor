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

            
    type SimpleMatriceswMultiplicator () =
        member this.multiplicate (nt1Matrix: ProbabilityMatrix.T) (nt2Matrix: ProbabilityMatrix.T) (from1: SubMatrix.T) (from2: SubMatrix.T) actualColCount =
            let from1Matrix = nt1Matrix.GetInnerSubMatrix from1.Left (Cell.shift from1.Right -1 -1) false
            let from2Matrix = nt2Matrix.GetInnerSubMatrix from2.Left (Cell.shift from2.Right -1 -1) true
//            (snd from2.Right - 1 - (from1.Size - actualColCount)) true

            let matricesSize = from1.Size
//            let resultColumns = actualColCount
//            let vectorLength = from1.Size
        
            let calculateCell (n, i, j) = 
                let skipMatrices = n * matricesSize * matricesSize
                let skipRows = skipMatrices + i * matricesSize
                let skipColumns = skipMatrices + j * matricesSize                
                Array.fold2 (fun b v1 v2 -> Probability.innerSumm b <| Probability.innerMult v1 v2)
                            Probability.innerZero
                            from1Matrix.[skipRows..skipRows + matricesSize - 1] 
                            from2Matrix.[skipColumns..skipColumns + matricesSize - 1]

            let getNIJ x = 
                let n = x / (matricesSize * matricesSize)
                let i = (x - n * matricesSize * matricesSize) / matricesSize
                let j = x - n * matricesSize * matricesSize - i * matricesSize
                n, i, j

            Array.init (matricesSize * actualColCount) (fun x -> calculateCell <| getNIJ x)
    

    type GPUMatriceswMultiplicator (probabilitySummQuote, probabilityMultQuote) =
    
        let platformName = "*"
        let deviceType = DeviceType.Default

        let provider =
            try  ComputeProvider.Create(platformName, deviceType)
            with 
            | ex -> failwith ex.Message

        let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

        let multiplicateProbabilities = probabilityMultQuote
        let summProbabilities = probabilitySummQuote

        let command = 
            <@
                fun (r:_2D) matricesSize (a:array<_>) (b:array<_>) (c:array<_>) -> 
//                    let num =  r.GlobalID0
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

        member this.multiplicate (from1: Probability.InnerType []) (from2: Probability.InnerType []) matricesCount matricesSize =
               
            let result: Probability.InnerType [] = Array.zeroCreate(matricesCount * matricesSize * matricesSize)
                          
    
            let d = new _2D(matricesSize * matricesCount, matricesSize, 1, 1)
//            let d = new _2D(matricesSize * matricesCount, matricesSize, min 4 (matricesSize * matricesCount), 1)

            let kernel, kernelPrepare, kernelRun = provider.Compile command
            kernelPrepare d matricesSize from1 from2 result
         
            commandQueue.Add(kernelRun()).Finish() |> ignore        
            commandQueue.Add(result.ToHost provider).Finish() |> ignore

            result

        member this.releaseResources =
            commandQueue.Dispose()
            provider.Dispose()
            provider.CloseAllBuffers()

       
    type MatrixHolder(tKeys, pKeys, stringSize) = 
    
        let GPUMultiplicator = new GPUMatriceswMultiplicator (Probability.innerSummQuote, Probability.innerMultQuote)
        let simpleMultiplicator = new SimpleMatriceswMultiplicator ()
            
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
   
        let addProbsToTMatrix cell nontermProbs =
            nontermProbs |> List.iter (fun (key, prob) -> tMatrix.[key].AddValueToCell cell prob)

        let addProbsToPSubMatrix (where: SubMatrix.T) nts (matrix: ProbabilityMatrix.T) =
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
            nonterminals |> List.iteri (fun i ntProbs -> addProbsToTMatrix (diagonalCell i) ntProbs)

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
                |> Array.iter (fun cell -> heads cell |> addProbsToTMatrix cell)


        // todo: combine small matrices to multiplicate together !!!
        member this.performMultiplication (tasks: MultiplicationTask []) nts = 
//            multiplicationCounter := !multiplicationCounter + (Array.length tasks)

            let crossproduct l1 l2 =
                let product lst v = Array.map (fun vl -> (vl, v)) lst
                Array.collect (product l1) l2

            let fullTasks = crossproduct tasks nts

            let matricesSize = tasks.[0].where.Size
            let matricesCount = fullTasks.Length
            let matricesCellCount = matricesSize * matricesSize

            if matricesSize > 8
            then
                // todo: last matrix reduced size !!!!!!!!!!!!

                let addOneFrom doTranspone buffer (from: SubMatrix.T, nt)  = 
                    let ntMatrix = tMatrix.[nt]

                    ntMatrix.GetInnerSubMatrix from.Left (Cell.shift from.Right -1 -1) doTranspone
                    |> Array.append buffer

                let from1 = 
                    fullTasks 
                    |> Array.map (fun ({from1 = from}, nts) -> from, fst nts)
                    |> Array.fold (addOneFrom false) Array.empty
                let from2 = 
                    fullTasks 
                    |> Array.map (fun ({from2 = from}, nts) -> from, snd nts)
                    |> Array.fold (addOneFrom true) Array.empty 

                let multiplicationResult = GPUMultiplicator.multiplicate from1 from2 matricesCount matricesSize

                [0..matricesCount - 1] 
                |> List.map (fun num -> multiplicationResult.[num * matricesCellCount..(num + 1) * matricesCellCount - 1])
                |> List.map (ProbabilityMatrix.create matricesSize matricesSize)
                |> List.iteri 
                    (fun i matrix -> 
                        let task, nt = fullTasks.[i]
                        addProbsToPSubMatrix task.where nt matrix)
            else
                let doOne (task, nts) =
                    let nt1, nt2 = nts
                    let nt1Matrix = tMatrix.[nt1]
                    let nt2Matrix = tMatrix.[nt2]
                    simpleMultiplicator.multiplicate nt1Matrix nt2Matrix task.from1 task.from2 matricesSize

                fullTasks
                |> Array.map doOne
                |> Array.map (ProbabilityMatrix.create matricesSize matricesSize)
                |> Array.iteri 
                    (fun i matrix -> 
                        let task, nt = fullTasks.[i]
                        addProbsToPSubMatrix task.where nt matrix)
