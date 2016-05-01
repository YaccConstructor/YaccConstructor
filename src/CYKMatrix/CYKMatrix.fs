module CYKMatrix

    open Util
    open System.Collections.Generic

    let recognize (strToParse: string) 
                  (complexRules: Dictionary<(NonTerminal * NonTerminal), (NonTerminal * double) list>)
                  (simpleRules: Dictionary<char, (NonTerminal * double) list>)
                  (epsilonRules: NonTerminal list) 
                  (nonterminals : NonTerminal [])
                  S 
                  maxSearchLength = 

        let stringSize = String.length strToParse

        let strSizeExponent = (System.Math.Log (double stringSize)) / (System.Math.Log 2.) |> System.Math.Ceiling |> int
        let roundedSize = (1 <<< strSizeExponent) - 1
    

        let emptyMatrixOfSize n = Array2D.init n n (fun x y -> 0.)
    
        // bottom-left triangle and diagonal of tMatrix and pMatrix are not used
        // upper-right triangle of size (stringSize - maxSearchLength) is not used
        let tMatrix = new Map<NonTerminal, double [,]>
                            (
                                nonterminals 
                                |> Seq.map (fun x -> x, emptyMatrixOfSize (stringSize + 1))
                            )

        let pMatrix = new Map<NonTerminal * NonTerminal, double [,]>
                            (
                                complexRules.Keys
                                |> Seq.map (fun x -> x, emptyMatrixOfSize (stringSize + 1))
                            ) 

        let addToP nts (matrix: double [,]) (l1, m1, l2, m2) =
            let where = pMatrix.[nts]
            for i in [l1..m1-1] do
                let rightBound = min m2 (stringSize + 1)
                for j in [l2..rightBound-1] do
                    where.[i, j] <- (where.[i, j] + matrix.[i-l1, j-l2])         

        let subMatrixMult (matrixA: double [,]) (matrixB: double [,]) (al1, am1, al2, am2) (bl1, bm1, bl2, bm2) = 
            let aHight = am1 - al1
            let aLength = aHight
            let calcCell i j =
                [0..aLength-1] |> List.fold (fun acc k -> acc + matrixA.[i + al1, k + al2] * matrixB.[k + bl1, j + bl2]) 0. 
            let bUpperBound = min bm2 (stringSize + 1)
            Array2D.init aHight (bUpperBound - bl2) calcCell                    
                                    
        let completeP where from1 from2 = 
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

            let addProbToMatrix (matrix: Map<_, double [,]>) row column key prob = 
                    (matrix.Item key).[row, column] <- (matrix.Item key).[row, column] + prob
                
            let updateTMatrixCell row column (nonTerm, prob) = addProbToMatrix tMatrix row column nonTerm prob

            if m1 - l1 = 1 && m1 = l2 then
                let currentChar = strToParse.[l1]
                let nonTerms = simpleRules.Item currentChar

                nonTerms
                |> List.iter (updateTMatrixCell l1 (l1 + 1))

            else if m1 - l1 = 1 && m1 < l2 then
                assert (m2 <= stringSize + 1)

                let headsFromTail (tail, tailProb) = 
                    if complexRules.ContainsKey tail then 
                        complexRules.Item tail |> List.map (fun (head, headProb) -> head, headProb * tailProb)
                    else 
                        []

                let tails = pMatrix |> Map.map (fun _ probs -> probs.[l1, l2]) |> Map.filter (fun _ prob -> prob > 0.)
                let heads = tails |> Map.toList |> List.map headsFromTail |> List.concat

                heads 
                |> List.iter (updateTMatrixCell l1 l2)

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

        tMatrix.Item S