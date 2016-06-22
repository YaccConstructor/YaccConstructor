module CYKMatrixBFS

    open Util
    open System.Collections.Generic
    open Array.Parallel
          
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
    
        let emptyMatrixOfSize n = myMatrixInit n n (fun x y -> 0.)
    
        // bottom-left triangle and diagonal of tMatrixes and pMatrixes are not used
        // upper-right triangle of size (stringSize - maxSearchLength) is not used

        // swich to dictionary (?)
        let tMatrix = new Map<NonTerminal, MyMatrix>
                            (
                                nonterminals 
                                |> Seq.map (fun x -> x, emptyMatrixOfSize (stringSize + 1))
                            )

        let pMatrix = new Map<NonTerminal * NonTerminal, MyMatrix>
                            (
                                allRules.ComplexTails
                                |> Seq.map (fun x -> x, emptyMatrixOfSize (stringSize + 1))
                            )
                  
        // todo: wrapper?      
        let addProbToTMatrix cell nontermProbs =
            nontermProbs |> List.iter (fun (key, prob) -> tMatrix.[key].AddValueToCell cell prob)

        let addProbsToPSubMatrix nts (matrix: MyMatrix) (where: SubMatrix.T) =
            let whereMatrix = pMatrix.[nts]
            let iShift = fst where.Left
            let jShift = snd where.Left
            for i in [0 .. where.Size - 1] do
                let actualColCount = (min (snd where.Top) (stringSize + 1)) - snd where.Left
                for j in [0 .. actualColCount - 1] do
                    whereMatrix.AddValueToCell (i + iShift, j + jShift) matrix.[i, j]
//                    addProbToMatrix pMatrix (i + fst where.Left) (j + snd where.Left) (nts, matrix.[i, j]) 
            
        let performMultiplication tasks = 
//            multiplicationCounter := !multiplicationCounter + (Array.length tasks)

            let performOneTask nts nt1Matrix nt2Matrix task = 
                let {where=where; from1=from1; from2=from2} = task
                let actualColCount = (min (snd from2.Top) (stringSize + 1)) - snd from2.Left
                addProbsToPSubMatrix nts (subMatrixMult nt1Matrix nt2Matrix from1 from2 actualColCount) where

            let performForOneNts nts = 
                let nt1, nt2 = nts
                tasks 
                |> Array.iter (fun task -> performOneTask nts tMatrix.[nt1] tMatrix.[nt2] task)
                
            if doParallel
            then Array.ofSeq allRules.ComplexTails |> Array.Parallel.iter performForOneNts
            else Array.ofSeq allRules.ComplexTails |> Array.iter          performForOneNts
            

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