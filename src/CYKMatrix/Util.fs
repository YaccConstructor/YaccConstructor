module Util
    
    open System
    open System.Collections.Generic
    open OpenCL.Net
    open Microsoft.FSharp.Math

    type Info<'T> = {
        MinMatrixSize: int
        Options: 'T
    }       

    type GPUBrahma = {
        PlatformName: string 
        DeviceType: DeviceType
        doParallelFlush: bool
    } 

    type GPUCuda = {
        doParallelFlush: bool
    }

    type CPUFast = Unit
    type CPUParallel = Unit

    type Algorithm = Okhotin | Modified

    module Options =

        type T = {
            newBrahma: GPUBrahma Info option  
            Brahma: GPUBrahma Info option  
            Cuda: GPUCuda Info option  
            Fast: CPUFast Info option
            Parallel: CPUParallel Info option
            algorithm: Algorithm
        }

        let empty algorithm = { Brahma = None; newBrahma = None; Cuda = None; Fast = None; Parallel = None; algorithm = algorithm }
        let createOne minMatrixSize options = { MinMatrixSize = minMatrixSize; Options = options}
        let create isNewBrahma algorithm fast parall cuda brahma = 
            if isNewBrahma
            then { Brahma = None; newBrahma = brahma; Cuda = cuda; Fast = fast; Parallel = parall; algorithm = algorithm }
            else { Brahma = brahma; newBrahma = None; Cuda = cuda; Fast = fast; Parallel = parall; algorithm = algorithm }

        let map f (option: _ Info) = {
            MinMatrixSize = option.MinMatrixSize
            Options = f option.Options
        }
    

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
        let createInnerType (x: float) = float32 x
        
//        let zero = create false
        let zero = create 0.
        
        let innerZero = innerValue zero
        let isZero v = innerValue v = innerZero

//        let inline innerSumm v1 v2 = v1 || v2
//        let inline innerMult v1 v2 = v1 && v2
        let innerSummQuote = <@ fun (v1: InnerType) (v2: InnerType) -> v1 + v2 @>
        let innerMultQuote = <@ fun (v1: InnerType) (v2: InnerType) -> v1 * v2 @>
        let innerZeroQuote = <@ float32 0. @>

        let innerSumm v1 v2 = v1 + v2
        let innerMult v1 v2 = v1 * v2
        //todo: compile quote

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
    
        let print (matrix: T) = 
            let top = matrix.Top
            printf " (top: %d, %d; size: %d) " top.Row top.Column matrix.Size    

                
    type MultiplicationTask = {where: SubMatrix.T; from1: SubMatrix.T; from2: SubMatrix.T}


    module ProbabilityMatrix =
        type T(matrix: Probability.InnerType [], nrow: int, ncol: int) =
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
                
            member this.getInnerFromCell (cell: Cell.T) =
                if isOutOfStorage cell
                then Probability.innerZero
                else data.[getSingleIndex cell]

            member this.Item
                with get cell = Probability.fromInnerValue <| this.getInnerFromCell cell    
                
            member this.SubMatrixValuesGetter (submatrix: SubMatrix.T) isTransponed fromInner =
                let leftCell = submatrix.Left 

                if isTransponed then Cell.transpone else id 
                >> Cell.shift leftCell.Row leftCell.Column
                >> this.getInnerFromCell 
                >> fromInner                             

            member this.GetSubArrayWithType fromInner (submatrix: SubMatrix.T) isTransponed  =
                let valueGetter = this.SubMatrixValuesGetter submatrix isTransponed fromInner                                         
                Array.init (submatrix.Size * submatrix.Size) ((cellBySingleIndex submatrix.Size) >> valueGetter)

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