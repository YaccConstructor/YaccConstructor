module Util
    
    open System
    open System.Collections.Generic
    open OpenCL.Net
//    open Microsoft.FSharp.Math

//    open FSharp.Quotations.Evaluator
//    open Microsoft.FSharp.Quotations
    
    let log2 s = (log (double s)) / (log 2.) |> ceil |> int

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
    type Mode = Test | Work

    module Options =

        type T = {
            _1DBrahma: GPUBrahma Info option  
            Brahma: GPUBrahma Info option  
            Cuda: GPUCuda Info option  
            Fast: CPUFast Info option
            Parallel: CPUParallel Info option
            algorithm: Algorithm
            mode: Mode
        }

        let empty algorithm = { Brahma = None; _1DBrahma = None; Cuda = None; Fast = None; Parallel = None; algorithm = algorithm; mode = Work }
        let createOne minMatrixSize options = { MinMatrixSize = minMatrixSize; Options = options }
        let create is_1DBrahma algorithm fast parall cuda brahma = 
            if is_1DBrahma
            then { Brahma = None; _1DBrahma = brahma; Cuda = cuda; Fast = fast; Parallel = parall; algorithm = algorithm; mode = Work }
            else { Brahma = brahma; _1DBrahma = None; Cuda = cuda; Fast = fast; Parallel = parall; algorithm = algorithm; mode = Work }

        let map f (option: _ Info) = {
            MinMatrixSize = option.MinMatrixSize
            Options = f option.Options
        }

        let emptyTest algorithm = { Brahma = None; _1DBrahma = None; Cuda = None; Fast = None; Parallel = None; algorithm = algorithm; mode = Test }
    

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
                     simpleRules: Dictionary<int, (NonTerminal * Probability.T) list>,
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

    type BooleanRulesHolder(complexRules: ((NonTerminal * Probability.T) * (NonTerminal * NonTerminal * bool) []) [],
                            simpleRules: ((NonTerminal * Probability.T) * int) [],
                            epsilonRules: NonTerminal [],
                            allConjucts: (NonTerminal * NonTerminal) [])  =  
                                         
        member this.SimpleRules = simpleRules
        member this.ComplexRules = complexRules
        member this.EpsilonRules = epsilonRules
        member this.AllConjucts = allConjucts

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
        type T(matrix: Probability.InnerType.T [], size: int) =

            //todo: optimize not used space
            let data = matrix
            member this.InnerValue = data

            member this.Nrow = size
            member this.Ncol = size
            member this.Size = size

            member this.WholeMatrix = SubMatrix.create (Cell.create 0 this.Ncol) this.Size
                
            member this.IsOutOfStorage (cell: Cell.T) =
                cell.Row < 0 || cell.Column < 0 || cell.Row >= this.Nrow || cell.Column >= this.Ncol

            member this.GetInnerFromCell (cell: Cell.T) =
                if this.IsOutOfStorage cell
                then Probability.InnerType.zero
                else data.[this.WholeMatrix.XByCell cell]

            member this.Item
                with get cell = Probability.fromInnerValue <| this.GetInnerFromCell cell    
               
            member this.SubMatrixValuesGetter isTransponed (submatrix: SubMatrix.T) =
                let leftCell = submatrix.Left 

                if isTransponed then Cell.transpone else id 
                >> Cell.shift leftCell.Row leftCell.Column
                >> this.GetInnerFromCell              

            member this.GetSubArray fromInner isTransponed (submatrix: SubMatrix.T) =
                let valueGetter = this.SubMatrixValuesGetter isTransponed submatrix                                          
                Array.init (submatrix.Size * submatrix.Size) (submatrix.CellByX >> valueGetter >> fromInner) 

            member this.CopyToArray fromInner isTransponed (submatrix: SubMatrix.T) (where: _ []) shift =
                let valueGetter = this.SubMatrixValuesGetter isTransponed submatrix  
                for i in 0..submatrix.Size * submatrix.Size do
                    where.[shift + i] <- (submatrix.CellByX i) |> valueGetter |> fromInner

            member this.AddValueToCell cell prob = 
                let x = this.WholeMatrix.XByCell cell
                data.[x] <- Probability.innerSumm data.[x] <| Probability.innerValue prob
        
            member this.AddSubmatrixByGetter (where: SubMatrix.T) getValueFromCell =
                for i in [0 .. where.Size - 1] do
                    let actualColCount = (min where.Top.Column this.Size) - where.Left.Column
                    for j in [0 .. actualColCount - 1] do
                        let matrixCell = Cell.create i j
                        let realCell = Cell.shift where.Left.Row where.Left.Column matrixCell
                        this.AddValueToCell realCell <| getValueFromCell matrixCell                        
            
            member this.AddSubmatrix where (matrix: T) = this.AddSubmatrixByGetter where (fun cell -> matrix.[cell])
                
        let create size matrix = T(matrix, size)

        let init size generator =
            let wholeSubmatrix = SubMatrix.create (Cell.create 0 size) size
            new T(Array.init (size * size) (fun x -> wholeSubmatrix.CellByX x |> generator |> Probability.innerValue), size)
        
        let empty n = init n (fun cell -> Probability.zero)