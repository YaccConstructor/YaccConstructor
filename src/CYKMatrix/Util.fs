module Util
    
    open System
    open System.Collections.Generic
    open OpenCL.Net
    open Microsoft.FSharp.Math

    type GPUOptions = {
        PlatformName: string 
        DeviceType: DeviceType
        MinMatrixSize: int
        doParallelFlush: bool
    }

    type FastOptions = {
        MinMatrixSize: int
    }

    type ParallelOptions = {
        MinMatrixSize: int
    }

    module Options =
        type T = {
            GPU: GPUOptions option  
            Fast: FastOptions option
            Parallel: ParallelOptions option
        }

        let empty = { GPU = None; Fast = None; Parallel = None }
    

    type NonTerminal = NonTerminal of string

    module Probability =
//        type InnerType = bool
        type InnerType = float32
        type T = Float32Probability of InnerType

        let innerValue (Float32Probability v) = v
        let fromInnerValue v = v |> Float32Probability
        
//        let create = fromInnerValue
//        let unwrap = innerValue
        let create = float32 >> fromInnerValue
        let unwrap = innerValue >> double
        
//        let zero = create false
        let zero = create 0.
        
        let innerZero = innerValue zero
        let isZero v = innerValue v = innerZero

//        let inline innerSumm v1 v2 = v1 || v2
//        let inline innerMult v1 v2 = v1 && v2
        let innerSummQuote = <@ fun (v1: InnerType) (v2: InnerType) -> v1 + v2 @>
        let innerMultQuote = <@ fun (v1: InnerType) (v2: InnerType) -> v1 * v2 @>
        let innerSumm v1 v2 = v1 + v2
        let innerMult v1 v2 = v1 * v2

        let summ (Float32Probability v1) (Float32Probability v2) = innerSumm v1 v2 |> fromInnerValue
        let multiplicate (Float32Probability v1) (Float32Probability v2) = innerMult v1 v2 |> fromInnerValue


    type RulesHolder(complexRules: Dictionary<(NonTerminal * NonTerminal), (NonTerminal * Probability.T) list>,
                     simpleRules: Dictionary<char, (NonTerminal * Probability.T) list>,
                     epsilonRules: NonTerminal list)  =  
                                         
        member this.SimpleTails = simpleRules.Keys |> Array.ofSeq
        member this.IsSimpleTail = simpleRules.ContainsKey
        member this.HeadsBySimpleTail c = 
            if simpleRules.ContainsKey c
            then simpleRules.[c]
            else []

        member this.ComplexTails = complexRules.Keys |> Array.ofSeq
        member this.IsComplexTail = complexRules.ContainsKey
        member this.HeadsByComplexTail c = 
            if complexRules.ContainsKey c
            then complexRules.[c]
            else []

    module Cell =
        type T(i, j) =
            member this.Row = i
            member this.Column = j
            member this.StringLength = j - i
        
        let create i j = T(i, j) 

        let shift (initial: T) rowShift colShift = create (initial.Row + rowShift) (initial.Column + colShift)


    module SubMatrix = 

        type T(top: Cell.T, size: int) =
            member this.Top  = top
            member this.Size = size

            member this.HalfSize = int(float(this.Size) / 2.)
            member this.RelativeDot = Cell.shift this.Top

            member this.Right  = this.RelativeDot this.Size  0
            member this.Left   = this.RelativeDot 0 -this.Size
            member this.Bottom = this.RelativeDot this.Size -this.Size  
            
            member this.maxStringLength = this.Top.StringLength - 1
            member this.minStringLength = this.Bottom.StringLength + 1
            
        let create top size = T(top, size)      
        let shift (matrix: T) rowShift colShift =
            let shiftedTop = Cell.shift matrix.Top rowShift colShift
            create shiftedTop matrix.Size

        let matrixWithShift matrixSize (initial: T) rowShift colShift =
            let newTop = Cell.shift initial.Top rowShift colShift
            T(newTop, matrixSize)

        let getOnlyCell (matrix: T) = 
            assert (matrix.Size = 1)
            matrix.Left

        type T with
            member this.RelativeMatrix = shift this

            member this.TopSubmatrix    = create this.Top this.HalfSize  
            member this.RightSubmatrix  = shift this.TopSubmatrix this.HalfSize  0
            member this.LeftSubmatrix   = shift this.TopSubmatrix 0 -this.HalfSize
            member this.BottomSubmatrix = shift this.TopSubmatrix this.HalfSize -this.HalfSize 
            
            member this.RightNeighbor = this.RelativeMatrix this.Size 0
            member this.LeftNeighbor  = this.RelativeMatrix 0 -this.Size
            member this.RightGrounded = this.RelativeMatrix (this.Top.StringLength - 2 * this.Size)  0
            member this.LeftGrounded  = this.RelativeMatrix 0 -(this.Top.StringLength - 2 * this.Size)
    
        let print (matrix: T) = 
            let top = matrix.Top
            printf " (top: %d, %d; size: %d) " top.Row top.Column matrix.Size    

                
    type MultiplicationTask = {where: SubMatrix.T; from1: SubMatrix.T; from2: SubMatrix.T}


    module ProbabilityMatrix =
        type T(matrix: Probability.InnerType [], nrow: int, ncol: int) =
            let ncol = ncol
            let nrow = nrow
            let getSingleIndex (cell: Cell.T) = cell.Row * ncol + cell.Column

            //todo: optimize not used space
            let data = matrix

            member this.innerValue = data
        
            member this.GetLength i = 
                match i with
                | 0 -> nrow
                | 1 -> ncol   
                | _ -> raise <| IndexOutOfRangeException()

            member this.Item
                with get cell = Probability.fromInnerValue data.[getSingleIndex cell]
                
            member this.getDataFromCell (cell: Cell.T) =
                let cellOutOfMatrix = cell.Row < 0 || cell.Column < 0 || cell.Row >= nrow || cell.Column >= ncol

                if cellOutOfMatrix
                then Probability.innerZero
                else data.[getSingleIndex cell]                                         

            // todo: recurring code
            member this.GetInnerSubMatrix (submatrix: SubMatrix.T) isTransponed =
                let cellStart = submatrix.Left 
                let cellFinish = Cell.shift submatrix.Right -1 -1
                let subNcol = cellFinish.Column - cellStart.Column + 1
                let subNrow = cellFinish.Row - cellStart.Row + 1

                let getCell x =
                    let subi = x / subNcol
                    let subj = x - subNcol * subi

                    if isTransponed
                    then Cell.create (subj + cellStart.Row) (subi + cellStart.Column)
                    else Cell.create (subi + cellStart.Row) (subj + cellStart.Column)
                                          
                Array.init (subNcol * subNrow) (fun x -> getCell x |> this.getDataFromCell)

            member this.GetFastSubMatrix (submatrix: SubMatrix.T) isTransponed : Matrix<float> =
                let cellStart = submatrix.Left 
                let cellFinish = Cell.shift submatrix.Right -1 -1
                let subNcol = cellFinish.Column - cellStart.Column + 1
                let subNrow = cellFinish.Row - cellStart.Row + 1

                let getCell (subi, subj) =
                    if isTransponed
                    then Cell.create (subj + cellStart.Row) (subi + cellStart.Column)
                    else Cell.create (subi + cellStart.Row) (subj + cellStart.Column) 
                      
                Matrix.init subNrow subNcol (fun x y -> getCell (x, y) |> this.getDataFromCell |> float)

            member this.AddValueToCell cell prob = 
                let x = getSingleIndex cell
                data.[x] <- Probability.innerSumm data.[x] <| Probability.innerValue prob
                
        let create nrow ncol matrix =
            T(matrix, nrow, ncol)

        let init nrow ncol generator =
            let splitIndex x =
                let i = x / ncol
                let j = x - ncol * i
                Cell.create i j
            new T(Array.init (ncol * nrow) (fun x -> splitIndex x |> generator |> Probability.innerValue), nrow, ncol)
        
        let empty n = init n n (fun cell -> Probability.zero)