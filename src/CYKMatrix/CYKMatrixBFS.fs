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

            member this.TopSubmatrix    = matrixWithShift this.HalfSize this 0  0 
            member this.RightSubmatrix  = matrixWithShift this.HalfSize this this.HalfSize  0
            member this.LeftSubmatrix   = matrixWithShift this.HalfSize this 0 -this.HalfSize
            member this.BottomSubmatrix = matrixWithShift this.HalfSize this this.HalfSize -this.HalfSize 
            
            member this.RightNeighbor = this.RelativeMatrix this.Size 0
            member this.LeftNeighbor  = this.RelativeMatrix 0 -this.Size
            member this.RightGrounded = this.RelativeMatrix (snd this.Top - fst this.Top - 2 * this.Size)  0
            member this.LeftGrounded  = this.RelativeMatrix 0 -(snd this.Top - fst this.Top - 2 * this.Size)
    
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
        
        
//    let multiplicationCounter = ref 0


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

        // swich to dictionary (?)
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
        let addProbToTMatrix cell nontermProbs =
            let row, column = cell 
            nontermProbs |> List.iter (addProbToMatrix tMatrix row column)

        let addToP nts (matrix: double [,]) (where: SubMatrix.T) =
            let whereMatrix = pMatrix.[nts]
            let iShift = fst where.Left
            let jShift = snd where.Left
            for i in [0 .. where.Size - 1] do
                let actualColCount = (min (snd where.Top) (stringSize + 1)) - snd where.Left
                for j in [0 .. actualColCount - 1] do
                    whereMatrix.[i + iShift, j + jShift] 
                            <- whereMatrix.[i + iShift, j + jShift] + 
                               matrix.[i, j]
//                    addProbToMatrix pMatrix (i + fst where.Left) (j + snd where.Left) (nts, matrix.[i, j])

        let subMatrixMult (nt1Matrix: double [,]) (nt2Matrix: double [,]) (from1: SubMatrix.T) (from2: SubMatrix.T) =
            let left1Fst = fst from1.Left
            let left1Snd = snd from1.Left
            let left2Fst = fst from2.Left
            let left2Snd = snd from2.Left
            let calcCell i j =
                [0..from1.Size-1] |> List.fold (fun acc k -> acc + nt1Matrix.[i + left1Fst, k + left1Snd] * 
                                                                   nt2Matrix.[k + left2Fst, j + left2Snd]) 0. 
            let actualCol2Count = (min (snd from2.Top) (stringSize + 1)) - snd from2.Left
            Array2D.init from1.Size actualCol2Count calcCell  
            
        let performMultiplication tasks = 
//            multiplicationCounter := !multiplicationCounter + (Array.length tasks)
            let crossproduct xs ys = 
                xs |> Array.collect (fun x -> ys |> Array.map (fun y -> x, y))

            let fullTasks = crossproduct tasks (Array.ofSeq allRules.ComplexTails) |> Array.ofSeq

            let performOne (task, nts) = 
                let (nt1, nt2) = nts
                let {where=where; from1=from1; from2=from2} = task
                addToP (nt1, nt2) (subMatrixMult tMatrix.[nt1] tMatrix.[nt2] from1 from2) where

            if doParallel
            then fullTasks |> Array.Parallel.iter performOne
            else fullTasks |> Array.iter performOne
            

        let updateTMatrixCells cells =
                let headProbsFromTail (tail, tailProb) = 
                    allRules.HeadsByComplexTail tail |> List.map (fun (head, headProb) -> head, headProb * tailProb)

                let tails cell = pMatrix |> Map.map (fun _ probs -> probs.[fst cell, snd cell]) |> Map.filter (fun _ prob -> prob > 0.)
                let heads cell = tails cell |> Map.toList |> List.map headProbsFromTail |> List.concat

                cells
                |> Array.iter (fun cell -> heads cell |> addProbToTMatrix cell)

        let layerIsRedundant (layer: SubMatrix.T []) =
            if Array.length layer = 0 
            then true
            else 
                let layerIsOutOfSearchZone = snd layer.[0].Bottom - fst layer.[0].Bottom + 1 >= maxSearchLength + 1
                layerIsOutOfSearchZone

        
        let rec completeLayer (layer: SubMatrix.T []) = 
            let matricesSize = layer.[0].Size

            if matricesSize = 1 then
                layer 
                |> Array.map (fun (matrix: SubMatrix.T) -> matrix.Left)
                |> updateTMatrixCells

            else
                let zeroSubLayer = layer |> Array.map (fun (matrix: SubMatrix.T) -> matrix.BottomSubmatrix)                   
                completeLayer zeroSubLayer
                completeVLayer layer

        and completeVLayer layer =
            let matricesSize = layer.[0].Size
            let halfMatricesSize = int(matricesSize / 2)

            let needToShortenNextLayers = snd layer.[Array.length layer - 1].LeftSubmatrix.Top > stringSize 

            let firstSubLayerWithExtras = layer |> Array.collect (fun matrix -> [|matrix.LeftSubmatrix; matrix.RightSubmatrix|])
            let firstSubLayer = 
                if needToShortenNextLayers
                then firstSubLayerWithExtras.[0..(Array.length firstSubLayerWithExtras) - 2] 
                else firstSubLayerWithExtras 
            
            if not <| layerIsRedundant firstSubLayer
            then
                let firstMultTasks = 
                    firstSubLayer
                    |> Array.mapi (fun i matrix -> if (i % 2) = 0
                                                   then {where=matrix; from1=matrix.LeftGrounded; from2=matrix.RightNeighbor}
                                                   else {where=matrix; from1=matrix.LeftNeighbor; from2=matrix.RightGrounded} )
                                            
                performMultiplication firstMultTasks
                completeLayer firstSubLayer

                let secondSubLayer = 
                    (if needToShortenNextLayers then layer.[0..(Array.length layer) - 2] else layer)
                    |> Array.map (fun matrix -> matrix.TopSubmatrix)  

                if not <| layerIsRedundant secondSubLayer
                then
                    let secondMultTasks = 
                        secondSubLayer 
                        |> Array.map (fun matrix -> {where=matrix; from1=matrix.LeftGrounded; from2=matrix.RightNeighbor}) 
                    let thirdMultTasks  = 
                        secondSubLayer 
                        |> Array.map (fun matrix -> {where=matrix; from1=matrix.LeftNeighbor; from2=matrix.RightGrounded})
                        
                    performMultiplication secondMultTasks
                    performMultiplication thirdMultTasks       
                    completeLayer secondSubLayer


        let constructLayer layerNum = 
            let matricesSize = 1 <<< layerNum   
            let matricesCount = (double stringSize + 1.) / double(matricesSize) - 1. |> ceil |> int

            let firstInLayer = SubMatrix.create (0, 2 * matricesSize) matricesSize
            let layer = Array.init matricesCount (fun i -> firstInLayer.RelativeMatrix (i * matricesSize) (i * matricesSize))
            layer
            
        let layerSearchUpperBound = ((log (double maxSearchLength + 1.)) / (log 2.) |> ceil |> int)
        let layerSizeUpperBound = matrixSizeExponent - 1

        let nonTermsForChars = strToParse |> List.ofSeq |> List.map allRules.HeadsBySimpleTail
        nonTermsForChars |> List.iteri (fun i ntProbs -> addProbToTMatrix (i, i + 1) ntProbs)

        for i in 1..(min layerSearchUpperBound layerSizeUpperBound) do
            let layer = constructLayer i
            
            if Array.length layer > 0 
            then completeVLayer layer
            
        tMatrix.Item S