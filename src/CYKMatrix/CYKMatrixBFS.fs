module CYKMatrixBFS

    open Util
    open System.Collections.Generic
    open Array.Parallel

    // todo: сделать красиво
    module SubMatrix = 

        // todo: private?
        let dotWithShift initial rowShift colShift =
            let initRow, initCol = initial
            (initRow + rowShift, initCol + colShift)

        type T = {Top: int * int; Size: int} with
            member this.HalfSize = int(float(this.Size) / 2.)
            member this.RelativeDot = dotWithShift this.Top

            member this.Right  = this.RelativeDot this.Size  0
            member this.Left   = this.RelativeDot 0 -this.Size
            member this.Bottom = this.RelativeDot this.Size -this.Size        

        let matrixWithShift matrixSize (initial: T) rowShift colShift =
            let newTop = dotWithShift initial.Top rowShift colShift
            {Top = newTop; Size = matrixSize}

        type T with
            member this.RelativeMatrix = matrixWithShift this.Size this

            member this.TopMatrix    = matrixWithShift this.HalfSize this 0  0 
            member this.RightMatrix  = matrixWithShift this.HalfSize this this.HalfSize  0
            member this.LeftMatrix   = matrixWithShift this.HalfSize this 0 -this.HalfSize
            member this.BottomMatrix = matrixWithShift this.HalfSize this this.HalfSize -this.HalfSize 
    
        let create top size = {Top = top; Size = size}
        let print matrix = 
            let row, col = matrix.Top
            printf " (top: %d, %d; size: %d) " row col matrix.Size



    type MultiplicationTask = {where: SubMatrix.T; from1: SubMatrix.T; from2: SubMatrix.T}
    let printTask task = 
        printf "where: "
        SubMatrix.print task.where
        printf "from1: "
        SubMatrix.print task.from1
        printf "from2: "
        SubMatrix.print task.from2
        printfn ""        


    let recognize (strToParse: string) 
                  (allRules: RulesHolder) 
                  (nonterminals : NonTerminal [])
                  S 
                  maxSearchLength 
                  doParallel
                  = 


        let stringSize = String.length strToParse

        let matrixSizeExponent = (log (double stringSize + 1.)) / (log 2.) |> ceil |> int
        let matrixSize = (1 <<< matrixSizeExponent)

    

        let emptyMatrixOfSize n = Array2D.init n n (fun x y -> 0.)
    
        // bottom-left triangle and diagonal of tMatrixes and pMatrixes are not used
        // upper-right triangle of size (stringSize - maxSearchLength) is not used
        let tMatrix = new Map<NonTerminal, double [,]>
                            (
                                nonterminals 
                                |> Seq.map (fun x -> x, emptyMatrixOfSize (stringSize + 1))
                            )

        let pMatrix = new Map<NonTerminal * NonTerminal, double [,]>
                            (
                                allRules.ComplexTails
                                |> Seq.map (fun x -> x, emptyMatrixOfSize (stringSize + 1))
                            )                                                        

        let addProbToMatrix (matrix: Map<_, double [,]>) row column nontermProb = 
            let key, prob = nontermProb
            (matrix.Item key).[row, column] <- (matrix.Item key).[row, column] + prob
                  
        // todo: wrapper?      
        let updateTMatrixCell row column nontermProbs = 
            nontermProbs |> List.iter (addProbToMatrix tMatrix row column)

        let addToP nts (matrix: double [,]) (where: SubMatrix.T) =
            for i in [0 .. where.Size - 1] do
                let actualColCount = (min (snd where.Top) (stringSize + 1)) - snd where.Left
                for j in [0 .. actualColCount - 1] do
                    addProbToMatrix pMatrix (i + fst where.Left) (j + snd where.Left) (nts, matrix.[i, j])

        let subMatrixMult nt1 nt2 (from1: SubMatrix.T) (from2: SubMatrix.T) =
            let calcCell i j =
                [0..from1.Size-1] |> List.fold (fun acc k -> acc + tMatrix.[nt1].[i + fst from1.Left, k + snd from1.Left] * 
                                                                   tMatrix.[nt2].[k + fst from2.Left, j + snd from2.Left]) 0. 
            let actualCol2Count = (min (snd from2.Top) (stringSize + 1)) - snd from2.Left
            Array2D.init from1.Size actualCol2Count calcCell    

        let performMultiplication tasks = 
            let crossproduct xs ys = 
                xs |> Array.collect (fun x -> ys |> Array.map (fun y -> x, y))


            let fullTasks = crossproduct tasks (Array.ofSeq allRules.ComplexTails) |> Array.ofSeq

            let performOne (task, nts) = 
                let (nt1, nt2) = nts
                let {where=where; from1=from1; from2=from2} = task
                addToP (nt1, nt2) (subMatrixMult nt1 nt2 from1 from2) where

            if doParallel
            then fullTasks |> Array.Parallel.iter performOne
            else fullTasks |> Array.iter performOne

        let layerIsRedundant (layer: SubMatrix.T []) =
            if Array.length layer = 0 
            then true
            else 
                let layerIsOutOfSearchZone = snd layer.[0].Bottom - fst layer.[0].Bottom + 1 >= maxSearchLength + 1
                layerIsOutOfSearchZone
        
        let rec completeLayer layer matricesSize = 
            if matricesSize = 1 then
                let headProbsFromTail (tail, tailProb) = 
                    if allRules.IsComplexTail tail then 
                        allRules.HeadByComplexTail tail |> List.map (fun (head, headProb) -> head, headProb * tailProb)
                    else 
                        []

                let tails row col = pMatrix |> Map.map (fun _ probs -> probs.[row, col-1]) |> Map.filter (fun _ prob -> prob > 0.)
                let heads (row, col) = tails row col |> Map.toList |> List.map headProbsFromTail |> List.concat

                layer 
                |> Array.map (fun (matrix: SubMatrix.T) -> matrix.Top)
                |> Array.map (fun top -> top, heads top)
                |> Array.iter (fun (top, heads) -> heads |> updateTMatrixCell (fst top) (snd top - 1))

            else
                let halfMatricesSize = int(matricesSize / 2)
                let zeroSubLayer = layer |> Array.map (fun (matrix: SubMatrix.T) -> matrix.BottomMatrix)                   
                completeLayer zeroSubLayer halfMatricesSize
                completeVLayer layer matricesSize

        and completeVLayer layer matricesSize =
            let halfMatricesSize = int(matricesSize / 2)
            let reducedLayer = 
                if snd layer.[Array.length layer - 1].Top > stringSize + 1 
                then layer.[0..Array.length layer - 2] 
                else layer

            let firstSubLayer = 
                if Array.length layer = Array.length reducedLayer
                then
                    layer 
                    |> Array.collect (fun matrix -> [|matrix.LeftMatrix; matrix.RightMatrix|])
                else
                    let reducedMatrix = layer.[Array.length layer - 1]
                    let allButOne = 
                        reducedLayer 
                        |> Array.collect (fun matrix -> [|matrix.LeftMatrix; matrix.RightMatrix|])
                    Array.append allButOne [| reducedMatrix.LeftMatrix |]   

            if not <| layerIsRedundant firstSubLayer
            then
                let toMult = 
                    //todo: annotation?
                    let getMults i (matrix: SubMatrix.T) =
                        if (i % 2) = 1
                        then
                            matrix.RelativeMatrix 0 -(snd matrix.Top - fst matrix.Top - matricesSize), 
                            matrix.RelativeMatrix halfMatricesSize 0
                        else
                            matrix.RelativeMatrix 0 -halfMatricesSize, 
                            matrix.RelativeMatrix (snd matrix.Top - fst matrix.Top - matricesSize) 0
                    firstSubLayer
                    |> Array.mapi getMults
                         
                let firstMultTasks = Array.map2 (fun w (f1, f2) -> {where=w; from1=f1; from2=f2})
                                                firstSubLayer 
                                                toMult
                                            
                performMultiplication firstMultTasks
                completeLayer firstSubLayer halfMatricesSize
                      
                let secondSubLayer =                 
                    reducedLayer 
                    |> Array.map (fun matrix -> matrix.TopMatrix)

                if not <| layerIsRedundant secondSubLayer
                then
                    let secondToMult = 
                        secondSubLayer 
                        |> Array.map (fun matrix -> matrix.RelativeMatrix 0 -(snd matrix.Top - fst matrix.Top - matricesSize), 
                                                    matrix.RelativeMatrix halfMatricesSize 0) 
                    let thirdToMult = 
                        secondSubLayer 
                        |> Array.map (fun matrix -> matrix.RelativeMatrix 0 -halfMatricesSize, 
                                                    matrix.RelativeMatrix (snd matrix.Top - fst matrix.Top - matricesSize) 0)
                    let secondMultTasks = Array.map2 (fun w (f1, f2) -> {where=w; from1=f1; from2=f2}) secondSubLayer secondToMult 
                    let thirdMultTasks = Array.map2 (fun w (f1, f2) -> {where=w; from1=f1; from2=f2}) secondSubLayer thirdToMult 
                        
                    performMultiplication secondMultTasks
                    performMultiplication thirdMultTasks       
                    completeLayer secondSubLayer halfMatricesSize


        and constructLayer layerNum = 
            let matricesSize = 1 <<< layerNum
            let halfMatricesSize = int(matricesSize / 2)
   
            let matricesCount = (double stringSize + 1.) / double(matricesSize) - 1. |> ceil |> int
            let firstInLayer = SubMatrix.create (0, 1 <<< (layerNum + 1)) matricesSize
            let layer = Array.init matricesCount (fun i -> firstInLayer.RelativeMatrix (i * matricesSize) (i * matricesSize))
            layer
            
        let layerNumBound = (log (double maxSearchLength + 1.)) / (log 2.) |> ceil |> int

        let nonTermsForChars = strToParse |> List.ofSeq |> List.map allRules.HeadBySimpleTail
        nonTermsForChars |> List.iteri (fun i p -> updateTMatrixCell i (i + 1) p)

        for i in 1..(layerNumBound - 1) do
            let layer = constructLayer i
            let matricesSize = 1 <<< i
            
            if Array.length layer > 0 
            then completeVLayer layer matricesSize
            
        tMatrix.Item S