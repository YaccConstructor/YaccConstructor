module CYKMatrix

    open Util

//    let multiplicationCounter = ref 0

    let recognize (strToParse: string) 
                  (allRules: RulesHolder)  
                  (nonterminals : NonTerminal [])
                  S 
                  maxSearchLength = 


        let stringSize = String.length strToParse

        let strSizeExponent = (System.Math.Log (double stringSize + 1.)) / (System.Math.Log 2.) |> System.Math.Ceiling |> int
        let roundedSize = (1 <<< strSizeExponent) - 1
    
        // bottom-left triangle and diagonal of tMatrix and pMatrix are not used
        // upper-right triangle of size (stringSize - maxSearchLength) is not used
        let tMatrix = new Map<NonTerminal, ProbabilityMatrix.T>
                            (
                                nonterminals 
                                |> Seq.map (fun x -> x, ProbabilityMatrix.empty (stringSize + 1))
                            )

        let pMatrix = new Map<NonTerminal * NonTerminal, ProbabilityMatrix.T>
                            (
                                allRules.ComplexTails
                                |> Seq.map (fun x -> x, ProbabilityMatrix.empty (stringSize + 1))
                            ) 

        let addToP nts (matrix: ProbabilityMatrix.T) (l1, m1, l2, m2) =
            let where = pMatrix.[nts]
            for i in [l1..m1-1] do
                let rightBound = min m2 (stringSize + 1)
                for j in [l2..rightBound-1] do
                    let realCell = Cell.create i j
                    let matrixCell = Cell.shift realCell -l1 -l2
                    where.AddValueToCell realCell matrix.[matrixCell]      

        let subMatrixMult (matrixA: ProbabilityMatrix.T) (matrixB: ProbabilityMatrix.T) (al1, am1, al2, am2) (bl1, bm1, bl2, bm2) = 
            let aHight = am1 - al1
            let aLength = aHight
            let calcCell (cell: Cell.T) =
                let aCell k = Cell.create (cell.Row + al1) (k + al2)
                let bCell k = Cell.create (k + bl1) (cell.Column + bl2)
                [0..aLength-1] 
                |> List.fold 
                    (fun acc k -> Probability.summ acc <| Probability.multiplicate matrixA.[aCell k] matrixB.[bCell k]) 
                    Probability.zero 
            let bUpperBound = min bm2 (stringSize + 1)
            ProbabilityMatrix.init aHight (bUpperBound - bl2) calcCell                 
                                    
        let completeP where from1 from2 = 
//            multiplicationCounter := !multiplicationCounter + 1
            let completeOnePair (nt1, nt2) =
                    addToP (nt1, nt2) (subMatrixMult tMatrix.[nt1] tMatrix.[nt2] from1 from2) where
            pMatrix |> Map.iter (fun nts _ -> completeOnePair nts)

    
        let rec compute l m =
            let mid = int (l + m) / 2
            if m - l >= 4 then 
                compute l mid
                if mid < stringSize + 1 then
                    compute mid m

            if mid < stringSize + 1 then
                completeT (l, mid, mid, m)

        and completeT (l1, m1, l2, m2) =
            assert (m1 - l1 = m2 - l2)

            let addProbToMatrix (matrix: Map<_, ProbabilityMatrix.T>) cell key prob = 
                matrix.[key].AddValueToCell cell prob
                
            let updateTMatrixCell cell (nonTerm, prob) = addProbToMatrix tMatrix cell nonTerm prob

            if m1 - l1 = 1 && m1 = l2 then
                let currentChar = strToParse.[l1]
                let nonTerms = allRules.HeadsBySimpleTail currentChar
                let cell = Cell.create l1 (l1 + 1)

                nonTerms
                |> List.iter (updateTMatrixCell cell)

            else if m1 - l1 = 1 && m1 < l2 then
                assert (m2 <= stringSize + 1)
                let cell = Cell.create l1 l2

                let headsFromTail (tail, tailProb) = 
                    if allRules.IsComplexTail tail then 
                        allRules.HeadsByComplexTail tail |> List.map (fun (head, headProb) -> head, Probability.multiplicate headProb tailProb)
                    else 
                        []

                let tails = pMatrix |> Map.map (fun _ probs -> probs.[cell]) |> Map.filter (fun _ prob -> not <| Probability.isZero prob)
                let heads = tails |> Map.toList |> List.map headsFromTail |> List.concat

                heads 
                |> List.iter (updateTMatrixCell cell)

            else if m1 - l1 > 1 then
                assert (l2 < stringSize + 1)

                let mid1: int = int (l1 + m1) / 2
                let mid2: int = int (l2 + m2) / 2

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

                    if mid2 <= stringSize then
                        completeP d2 c b2 
                        completeT d2 

                        if underMaxSearchLength mid1 mid2 then
                            completeP e b1 d2 
                            completeP e d1 b2 
                            completeT e  

        compute 0 (roundedSize + 1) |> ignore
        
//        printfn "okhotin mult count: %i" !multiplicationCounter
//        multiplicationCounter := 0
        tMatrix.Item S