module CYKMatrixOkhotin

    open Util
    open TPMatrices
    open System
    open System.Threading
    open System.Threading.Tasks

//    let multiplicationCounter = ref 0

    let recognize options
                  (inputToParse: int<AbstractAnalysis.Common.token> list)
                  (allRules: RulesHolder)  
                  nonterminals
                  S 
                  maxSearchLength = 
                                    
        let inputSize = inputToParse.Length

        let inputSizeExponent = (System.Math.Log (double inputSize + 1.)) / (System.Math.Log 2.) |> System.Math.Ceiling |> int
        let roundedSize = (1 <<< inputSizeExponent) - 1
    
        // bottom-left triangle and diagonal of tMatrix and pMatrix are not used
        // upper-right triangle of size (inputSize - maxSearchLength) is not used
        let matrices = new MatrixHolder(nonterminals, allRules.ComplexTails, inputSize, options)
        
        let matrixSizeExponent = (log (double inputSize + 1.)) / (log 2.) |> ceil |> int
        let matrixSize = (1 <<< matrixSizeExponent)                  
                                    
        let completeP where from1 from2 = 
            let submatrixFromTuple (l1, m1, l2, m2) =
                let top = Cell.create l1 m2
                let size = m1 - l1
                SubMatrix.create top size

            let task = 
                { where = submatrixFromTuple where
                  from1 = submatrixFromTuple from1
                  from2 = submatrixFromTuple from2 }

            matrices.performMultiplication [| task |] allRules.ComplexTails
    
        let rec compute l m =
            let mid = int (l + m) / 2
            if m - l >= 4 && mid < inputSize + 1 then 
                let doLeft = Action( fun () -> compute l mid )
                let doRight = Action( fun () -> compute mid m )
                Parallel.Invoke(doLeft, doRight)
            elif m - l >= 4 then
                compute l mid

            if mid < inputSize + 1 then
                completeT (l, mid, mid, m)

        and completeT (l1, m1, l2, m2) =
            assert (m1 - l1 = m2 - l2)

            if m1 - l1 = 1 && m1 = l2 then
                let currentChar = inputToParse.[l1]
                let nonterminals = allRules.HeadsBySimpleTail currentChar
                let cell = Cell.create l1 (l1 + 1)

                matrices.updateTCellWith cell nonterminals

            else if m1 - l1 = 1 && m1 < l2 then
                assert (m2 <= inputSize + 1)
                let cell = Cell.create l1 l2

                let headProbsFromTail (tail, tailProb) = 
                    if allRules.IsComplexTail tail then 
                        allRules.HeadsByComplexTail tail |> List.map (fun (head, headProb) -> head, Probability.multiplicate headProb tailProb)
                    else 
                        []

                matrices.refreshTCells headProbsFromTail [| cell |]
                 
            else if m1 - l1 > 1 then
                assert (l2 < inputSize + 1)

                let mid1 = (l1 + m1) / 2
                let mid2 = (l2 + m2) / 2

                let b1 = (l1, mid1, mid1, m1)

                let c  = (mid1, m1, l2, mid2)
                let d1 = (l1, mid1, l2, mid2)
            
                let b2 = (l2, mid2, mid2, m2)
                let d2 = (mid1, m1, mid2, m2)
                let  e = (l1, mid1, mid2, m2)

                let underMaxSearchLength l m = m - l < maxSearchLength

                completeT c

                if underMaxSearchLength mid1 l2 then
                    completeP d1 b1 c 
                    completeT d1

                    if mid2 <= inputSize then
                        completeP d2 c b2 
                        completeT d2 

                        if underMaxSearchLength mid1 mid2 then
                            completeP e b1 d2 
                            completeP e d1 b2 
                            completeT e  

        compute 0 (roundedSize + 1) |> ignore
        
        matrices.releaseResources ()
        (matrices.getProbabilities S), matrices.MultiplicationCounter