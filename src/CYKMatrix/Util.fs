module Util
    
    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Math

    open Brahma.Helpers
    open OpenCL.Net
    open Brahma.OpenCL
    open Brahma.FSharp.OpenCL.Core
    open Microsoft.FSharp.Quotations
    open Brahma.FSharp.OpenCL.Extensions

    type NonTerminal = NonTerminal of string

    type ComplexRule = { 
        Head: NonTerminal;
        LeftTail: NonTerminal;
        RightTail: NonTerminal; 
        probability: double;
    }

    type SimpleRule = {
        Head: NonTerminal;
        Tail: char;
        probability: double;
    } 

    type EpsRule =  {
        Head: NonTerminal;
        probability: double;
    } 

    type RulesHolder(complexRules: Dictionary<(NonTerminal * NonTerminal), (NonTerminal * double) list>,
                     simpleRules: Dictionary<char, (NonTerminal * double) list>,
                     epsilonRules: NonTerminal list)  =  
                                         
        member this.SimpleTails = simpleRules.Keys |> Array.ofSeq
        member this.IsSimpleTail = simpleRules.ContainsKey
        member this.HeadsBySimpleTail c = 
            if simpleRules.ContainsKey c
            then simpleRules.Item c
            else []

        member this.ComplexTails = complexRules.Keys |> Array.ofSeq
        member this.IsComplexTail = complexRules.ContainsKey
        member this.HeadsByComplexTail c = 
            if complexRules.ContainsKey c
            then complexRules.Item c
            else []

    module SubMatrix = 
        // todo: private?
        let dotWithShift initial rowShift colShift =
            let initRow, initCol = initial
            (initRow + rowShift, initCol + colShift)

        type T(top: int * int, size: int) =
            let relativeDot = dotWithShift top

            member this.Top  = top
            member this.Size = size

            member this.HalfSize = int(float(this.Size) / 2.)
            member this.RelativeDot = dotWithShift this.Top

            member this.Right  = this.RelativeDot this.Size  0
            member this.Left   = this.RelativeDot 0 -this.Size
            member this.Bottom = this.RelativeDot this.Size -this.Size        

        let matrixWithShift matrixSize (initial: T) rowShift colShift =
            let newTop = dotWithShift initial.Top rowShift colShift
            T(newTop, matrixSize)

        type T with
            member this.RelativeMatrix = matrixWithShift this.Size this

            member this.TopSubmatrix    = matrixWithShift this.HalfSize this 0  0 
            member this.RightSubmatrix  = matrixWithShift this.HalfSize this this.HalfSize  0
            member this.LeftSubmatrix   = matrixWithShift this.HalfSize this 0 -this.HalfSize
            member this.BottomSubmatrix = matrixWithShift this.HalfSize this this.HalfSize -this.HalfSize 
            
            member this.RightNeighbor = this.RelativeMatrix this.Size 0
            member this.LeftNeighbor  = this.RelativeMatrix 0 -this.Size
            member this.RightGrounded = this.RelativeMatrix (snd this.Top - fst this.Top - 2 * this.Size)  0
            member this.LeftGrounded  = this.RelativeMatrix 0 -(snd this.Top - fst this.Top - 2 * this.Size)
    
        let create top size = T(top, size)
        let print (matrix: T) = 
            let row, col = matrix.Top
            printf " (top: %d, %d; size: %d) " row col matrix.Size


////    type MyMatrix(sizeX: int, sizeY: int, generator: int -> int -> double) = 
//    type MyMatrix(matrix: Matrix<float>) =
////        let data  = Matrix.init sizeX sizeY generator
//        let data = matrix
//        
//        member this.GetLength i = 
//            match i with
//            | 0 -> data.NumRows   
//            | 1 -> data.NumCols   
//            | _ -> raise <| IndexOutOfRangeException()
//
//        member this.Item
//            with get (i, j) = data.[i, j]
////            and  set (i, j) value = data.[i, j] <- value
//
//        member this.GetSlice (rowStart: int option, rowFinish: int option,
//                              colStart: int option, colFinish: int option) =
//            let rowStart = 
//                match rowStart with
//                | Some(v) -> v
//                | None -> 0
//            let rowFinish = 
//                match rowFinish with
//                | Some(v) -> v
//                | None -> data.NumRows - 1
//            let colStart = 
//                match colStart with
//                | Some(v) -> v
//                | None -> 0
//            let colFinish = 
//                match colFinish with
//                | Some(v) -> v
//                | None -> data.NumCols - 1
//            data.[rowStart..rowFinish, colStart..colFinish]
//            
//        member this.AddValueToCell (i, j) value = data.[i, j] <- data.[i, j] + value
//
//    let myMatrixInit sizeX sizeY generator =
//        new MyMatrix(Matrix.init sizeX sizeY (fun i j -> generator (i,j)))
//        
//    let emptyMatrixOfSize n = myMatrixInit n n (fun cell -> 0.)
//        
//    let subMatrixMult (nt1Matrix: MyMatrix) (nt2Matrix: MyMatrix) (from1: SubMatrix.T) (from2: SubMatrix.T) actualColCount =
//        let from1Matrix = nt1Matrix.[fst from1.Left..(fst from1.Right - 1), 
//                                     snd from1.Left..(snd from1.Right - 1)]
//        let from2Matrix = nt2Matrix.[fst from2.Left..(fst from2.Right - 1),
//                                     (snd from2.Left)..(snd from2.Right - 1 - (from1.Size - actualColCount))]
//
//        new MyMatrix(from1Matrix * from2Matrix)

    type MyMatrix(matrix: float32 [], nrow: int, ncol: int) =
        let ncol = ncol
        let nrow = nrow
        let getSingleIndex i j = i * ncol + j
        let data = matrix
        
        member this.GetLength i = 
            match i with
            | 0 -> nrow
            | 1 -> ncol   
            | _ -> raise <| IndexOutOfRangeException()

        member this.Item
            with get (i, j) = double data.[getSingleIndex i j]
//            and  set (i, j) value = data.[getSingleIndex i j] <- (float32 value)

        member this.GetSlice (rowStart: int option, rowFinish: int option,
                              colStart: int option, colFinish: int option) =
            let rowStart = 
                match rowStart with
                | Some(v) -> v
                | None -> 0
            let rowFinish = 
                match rowFinish with
                | Some(v) -> v
                | None -> nrow - 1
            let colStart = 
                match colStart with
                | Some(v) -> v
                | None -> 0
            let colFinish = 
                match colFinish with
                | Some(v) -> v
                | None -> ncol - 1
            let subNcol = colFinish - colStart + 1
            let subNrow = rowFinish - rowStart + 1
            let rebaseIndex x =
                let subi = x / subNcol
                let subj = x - subNcol * subi
                getSingleIndex (subi + rowStart) (subj + colStart)            
            Array.init (subNcol * subNrow) (fun x -> data.[rebaseIndex x])

        member this.AddValueToCell (i, j) (value: double) = 
            let x = getSingleIndex i j
            data.[x] <- data.[x] + (float32 value)

    let myMatrixInit nrow ncol generator =
        let splitIndex x =
            let i = x / ncol
            let j = x - ncol * i
            i, j
        new MyMatrix(Array.init (ncol * nrow) (fun x -> splitIndex x |> generator |> float32), nrow, ncol)
        
    let emptyMatrixOfSize n = myMatrixInit n n (fun cell -> 0.)
        
    type MatrixMultiplicator() = 
        let localWorkSize = 2
        let platformName = "*"
        let deviceType = DeviceType.Default

        let provider =
            try  ComputeProvider.Create(platformName, deviceType)
            with 
            | ex -> failwith ex.Message

        let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

        let command = 
            <@
                fun (r:_2D) resultColumns vectorLength (a:array<_>) (b:array<_>) (c:array<_>) -> 
                    let ti = r.GlobalID0
                    let tj = r.GlobalID1                    
                    let mutable buf = c.[ti * resultColumns + tj]
                    for k in 0 .. vectorLength - 1 do
                        buf <- buf + (a.[ti * vectorLength + k] * b.[k * resultColumns + tj])
                    c.[ti * resultColumns + tj] <- buf
            @>

        member this.multiplicate (nt1Matrix: MyMatrix) (nt2Matrix: MyMatrix) (from1: SubMatrix.T) (from2: SubMatrix.T) actualColCount =
            let from1Matrix = nt1Matrix.[fst from1.Left..(fst from1.Right - 1), 
                                         snd from1.Left..(snd from1.Right - 1)]
            let from2Matrix = nt2Matrix.[fst from2.Left..(fst from2.Right - 1),
                                         (snd from2.Left)..(snd from2.Right - 1 - (from1.Size - actualColCount))]

    //        if from1.Size >= 4
    //        then
    //        else

            let resultRows = from1.Size
            let resultColumns = actualColCount
            let vectorLength = from1.Size
        
            let result = Array.zeroCreate(from1.Size * actualColCount)

            let kernel, kernelPrepare, kernelRun = provider.Compile command
    
            let d = new _2D(from1.Size, actualColCount, min 4 from1.Size, 1)
            kernelPrepare d resultColumns vectorLength from1Matrix from2Matrix result
         
            commandQueue.Add(kernelRun()).Finish() |> ignore        
            commandQueue.Add(result.ToHost provider).Finish() |> ignore

            new MyMatrix(result, from1.Size, actualColCount)

        member this.releaseResources =
            commandQueue.Dispose()
            provider.Dispose()
            provider.CloseAllBuffers()

        
//    let subMatrixMult (nt1Matrix: MyMatrix) (nt2Matrix: MyMatrix) (from1: SubMatrix.T) (from2: SubMatrix.T) actualColCount =
//        let left1Fst = fst from1.Left
//        let left1Snd = snd from1.Left
//        let left2Fst = fst from2.Left
//        let left2Snd = snd from2.Left
//        let calcCell i j =
//            [0..from1.Size-1] |> List.fold (fun acc k -> acc + nt1Matrix.[i + left1Fst, k + left1Snd] 
//                                                                * nt2Matrix.[k + left2Fst, j + left2Snd]) 
//                                            0. 
//        new MyMatrix(from1.Size, actualColCount, calcCell) 
        

    type MultiplicationTask = {where: SubMatrix.T; from1: SubMatrix.T; from2: SubMatrix.T}
    let printTask task = 
        printf "where: "
        SubMatrix.print task.where
        printf "from1: "
        SubMatrix.print task.from1
        printf "from2: "
        SubMatrix.print task.from2
        printfn ""