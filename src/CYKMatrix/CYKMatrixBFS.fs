module CYKMatrixBFS

    open Util
    open TPMatrices
          
//    let multiplicationCounter = ref 0

    let recognize options
                  strToParse
                  (allRules: RulesHolder) 
                  nonterminals 
                  S 
                  maxSearchLength = 
                  
        let stringSize = String.length strToParse
        
        // bottom-left triangle and diagonal of tMatrixes and pMatrixes are not used
        // upper-right triangle of size (stringSize - maxSearchLength) is not used
        let matrices = new MatrixHolder(nonterminals, allRules.ComplexTails, stringSize, options)

        let matrixSizeExponent = (log (double stringSize + 1.)) / (log 2.) |> ceil |> int
        let matrixSize = (1 <<< matrixSizeExponent)    

        let layerIsRedundant (layer: SubMatrix.T []) =
            if Array.length layer = 0 
            then true
            else 
                let oneMatrix = layer.[0]
                oneMatrix.minStringLength > maxSearchLength
        
        let rec completeLayer (layer: SubMatrix.T []) = 
            let matricesSize = layer.[0].Size

            if matricesSize = 1 then
                let headProbsFromTail (tail, tailProb) = 
                    // todo: boolean grammar
                    allRules.HeadsByComplexTail tail 
                    |> List.map (fun (head, headProb) -> head, Probability.multiplicate headProb tailProb)
                
                layer 
                |> Array.map SubMatrix.getOnlyCell 
                |> matrices.refreshTCells (headProbsFromTail)

            else
                let zeroSubLayer = layer |> Array.map (fun matrix -> matrix.BottomSubmatrix)                   
                completeLayer zeroSubLayer
                completeVLayer layer

        and completeVLayer layer =
            let matricesSize = layer.[0].Size
            let halfMatricesSize = int(matricesSize / 2)

            let needToShortenNextLayers = layer.[Array.length layer - 1].LeftSubmatrix.Top.Column > stringSize 

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
                                            
                matrices.performMultiplication firstMultTasks allRules.ComplexTails
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
                        
                    matrices.performMultiplication secondMultTasks allRules.ComplexTails
                    matrices.performMultiplication thirdMultTasks allRules.ComplexTails      
                    completeLayer secondSubLayer


        let constructLayer layerNum = 
            let matricesSize = 1 <<< layerNum   
            let matricesCount = (double stringSize + 1.) / double(matricesSize) - 1. |> ceil |> int

            let firstTopCell = Cell.create 0 (2 * matricesSize)
            let firstInLayer = SubMatrix.create firstTopCell matricesSize
            let layer = Array.init matricesCount (fun i -> firstInLayer.RelativeMatrix (i * matricesSize) (i * matricesSize))
            layer
            
        let layerSearchUpperBound = (log (double maxSearchLength + 1.)) / (log 2.) |> ceil |> int
        let layerSizeUpperBound = matrixSizeExponent - 1

        let stringOfNonterminals = strToParse |> List.ofSeq |> List.map allRules.HeadsBySimpleTail
        matrices.initTDiagonalWith stringOfNonterminals

        for i in 1..(min layerSearchUpperBound layerSizeUpperBound) do
            let layer = constructLayer i
            
            if Array.length layer > 0 
            then completeVLayer layer
            
        // todo: release ????
        matrices.releaseResources ()
        matrices.getProbabilities S