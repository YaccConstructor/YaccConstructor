module Util
    
    open System
    open System.Collections.Generic
    open OpenCL.Net
    open Microsoft.FSharp.Math

    open FSharp.Quotations.Evaluator
    open Microsoft.FSharp.Quotations

    type Info<'T> = {
        MinMatrixSize: int
        Options: 'T
    }       

    type GPUBrahma = {
        PlatformName: string 
        DeviceType: DeviceType
        doParallelFlush: bool
        MinCells: int
    } 

    type GPUCuda = {
        doParallelFlush: bool
    }

    type CPUFast = Unit
    type CPUParallel = Unit

    type Algorithm = Okhotin | Modified

    module Options =

        type T = {
            _1DBrahma: GPUBrahma Info option  
            Brahma: GPUBrahma Info option  
            Cuda: GPUCuda Info option  
            Fast: CPUFast Info option
            Parallel: CPUParallel Info option
            algorithm: Algorithm
        }

        let empty algorithm = { Brahma = None; _1DBrahma = None; Cuda = None; Fast = None; Parallel = None; algorithm = algorithm }
        let createOne minMatrixSize options = { MinMatrixSize = minMatrixSize; Options = options}
        let create is_1DBrahma algorithm fast parall cuda brahma = 
            if is_1DBrahma
            then { Brahma = None; _1DBrahma = brahma; Cuda = cuda; Fast = fast; Parallel = parall; algorithm = algorithm }
            else { Brahma = brahma; _1DBrahma = None; Cuda = cuda; Fast = fast; Parallel = parall; algorithm = algorithm }

        let map f (option: _ Info) = {
            MinMatrixSize = option.MinMatrixSize
            Options = f option.Options
        }
    

    type NonTerminal = NonTerminal of string

    module Probability =
//        type InnerType = bool

        module InnerType =
            type T = float

            let create = float
            let unwrap: T -> float = float

            let zero = create 0.

            let zeroQuote = <@ float 0. @>            
            
        type T = FloatProbability of InnerType.T

        let innerValue (FloatProbability v) = v
        let fromInnerValue v = v |> FloatProbability
                
        let innerSumm v1 v2 = v1 + v2
        let innerMult v1 v2 = v1 * v2

//        let inline innerSumm v1 v2 = v1 || v2
//        let inline innerMult v1 v2 = v1 && v2
        let innerSummQuote = <@ fun (v1: InnerType.T) (v2: InnerType.T) -> v1 + v2 @>
        let innerMultQuote = <@ fun (v1: InnerType.T) (v2: InnerType.T) -> v1 * v2 @>

        let create = InnerType.create >> fromInnerValue
        let unwrap = innerValue >> double
        
        let isZero v = innerValue v = InnerType.zero
        let zero = fromInnerValue InnerType.zero
        //todo: compile quote

        let summ (FloatProbability v1) (FloatProbability v2) = innerSumm v1 v2 |> fromInnerValue
        let multiplicate (FloatProbability v1) (FloatProbability v2) = innerMult v1 v2 |> fromInnerValue


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

        let transpone (cell: T) = create cell.Column cell.Row
        let shift rowShift colShift (initial: T) = create (initial.Row + rowShift) (initial.Column + colShift)


    module SubMatrix = 

        type T(top: Cell.T, size: int) =
            member this.Top  = top
            member this.Size = size

            member this.HalfSize = int(float(this.Size) / 2.)
            member this.RelativeDot x y = Cell.shift x y this.Top

            member this.Right  = this.RelativeDot this.Size  0
            member this.Left   = this.RelativeDot 0 -this.Size
            member this.Bottom = this.RelativeDot this.Size -this.Size  
            
            member this.maxStringLength = this.Top.StringLength - 1
            member this.minStringLength = this.Bottom.StringLength + 1
            
        let create top size = T(top, size)      
        let shift (matrix: T) rowShift colShift =
            let shiftedTop = Cell.shift rowShift colShift matrix.Top 
            create shiftedTop matrix.Size

        let matrixWithShift matrixSize (initial: T) rowShift colShift =
            let newTop = Cell.shift rowShift colShift initial.Top
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
            
            member this.CellByX x =
                let i = x / this.Size
                let j = x - i * this.Size
                Cell.create i j
            
            member this.XByCell (cell: Cell.T) =
                cell.Row * this.Size + cell.Column
    
        let print (matrix: T) = 
            let top = matrix.Top
            printf " (top: %d, %d; size: %d) " top.Row top.Column matrix.Size    

                
    type MultiplicationTask = {where: SubMatrix.T; from1: SubMatrix.T; from2: SubMatrix.T}


    module ProbabilityMatrix =
        type T(matrix: Probability.InnerType.T [], nrow: int, ncol: int) =
            let ncol = ncol
            let nrow = nrow
            let getSingleIndex (cell: Cell.T) = cell.Row * ncol + cell.Column
                
            let isOutOfStorage (cell: Cell.T) =
                cell.Row < 0 || cell.Column < 0 || cell.Row >= nrow || cell.Column >= ncol
                            
            let cellBySingleIndex size x =
                let subi = x / size
                let subj = x - size * subi
                Cell.create subi subj 

            //todo: optimize not used space
            let data = matrix

            member this.innerValue = data
        
            member this.GetLength i = 
                match i with
                | 0 -> nrow
                | 1 -> ncol   
                | _ -> raise <| IndexOutOfRangeException()  

            member this.GetInnerFromCell (cell: Cell.T) =
                if isOutOfStorage cell
                then Probability.InnerType.zero
                else data.[getSingleIndex cell]

            member this.Item
                with get cell = Probability.fromInnerValue <| this.GetInnerFromCell cell    
               
            member this.SubMatrixValuesGetter (submatrix: SubMatrix.T) isTransponed  =
                let leftCell = submatrix.Left 

                if isTransponed then Cell.transpone else id 
                >> Cell.shift leftCell.Row leftCell.Column
                >> this.GetInnerFromCell              

            member this.GetSubArray fromInner (submatrix: SubMatrix.T) isTransponed =
                let valueGetter = this.SubMatrixValuesGetter submatrix isTransponed                                          
                Array.init (submatrix.Size * submatrix.Size) (submatrix.CellByX >> valueGetter >> fromInner) 

            member this.CopyToArray fromInner (submatrix: SubMatrix.T) isTransponed (where: _ []) shift =
                let valueGetter = this.SubMatrixValuesGetter submatrix isTransponed  
                for i in 0..submatrix.Size * submatrix.Size do
                    where.[shift + i] <- (submatrix.CellByX i) |> valueGetter |> fromInner

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