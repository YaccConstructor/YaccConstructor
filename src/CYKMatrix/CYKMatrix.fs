open System.Collections.Generic

type NonTerminal = NonTerminal of string

type ComplexRule = { 
    Head: NonTerminal;
    LeftTail: NonTerminal;
    RightTail: NonTerminal; 
    probability: double;
}

type SimpleRule = {
    Head: NonTerminal;
    Tail: char;
    probability: double;
} 

type EpsRule =  {
    Head: NonTerminal;
    probability: double;
} 

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
            compute l mid |> ignore
            if mid < stringSize + 1 then
                compute mid m |> ignore

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

            completeT c |> ignore

            if underMaxSearchLength mid1 l2 then
                completeP d1 b1 c |> ignore
                completeT d1 |> ignore

                if mid2 <= stringSize then
                    completeP d2 c b2 |> ignore
                    completeT d2 |> ignore

                    if underMaxSearchLength mid1 mid2 then
                        completeP e b1 d2 |> ignore
                        completeP e d1 b2 |> ignore
                        completeT e  |> ignore

    compute 0 (roundedSize + 1) |> ignore

    tMatrix.Item S


[<EntryPoint>]
let main args = 
    let A = NonTerminal "A"
    let B = NonTerminal "B"
    let BB = NonTerminal "BB"
    let S = NonTerminal "S"
    let nonterminals = [|A; B; S; BB|]


    let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * double) list>()
    [(A, BB), [S, 1.]; (B, B), [BB, 1.; B, 0.2]; (A, A), [A, 0.8]] |> Seq.iter crl.Add
    let srl = new Dictionary<char, (NonTerminal * double) list>()
    ['a', [(A, 0.2)]; 'b', [B, 0.4]; 'c', [B, 0.4]] |> Seq.iter srl.Add
    let erl: NonTerminal list = []

//    S -> A BB, 1.
//
//    BB -> B B, 1.
//
//    A -> A A, 0.8
//    A -> 'a', 0.2
//
//    B -> B B, 0.2
//    B -> 'b', 0.4
//    B -> 'b', 0.4

    let printMatrix (matrix: double [,]) strLen searchLen =
        let rowLength = matrix.GetLength(0)
        let colLength = matrix.GetLength(1)

        for i in [0..rowLength-1] do
            for j in [0..colLength-1] do
                if i <= strLen && j <= strLen && j > i && j-i <= searchLen then
                    printf "%.8f  " matrix.[i, j]
                else
                    assert (matrix.[i, j] = 0.)
                    printf "----------  "
            printfn ""
        printfn ""

    let isAnswerValid (matrix: double [,]) strLen searchLen = 
        let rowLength = matrix.GetLength(0)
        let colLength = matrix.GetLength(1)
        if rowLength <> colLength || rowLength <> strLen + 1 then
            false
        else
            let notRealCell (i, j) =
                    i > strLen 
                    || j > strLen 
                    || j <= i 
                    || j-i > searchLen 

            [1..rowLength-1]
            |> List.map (fun i -> [1..colLength-1] |> List.map (fun j -> (i,j))) 
            |> List.concat
            |> List.filter notRealCell
            |> List.forall (fun (i, j) -> matrix.[i,j] = 0.)

    let check str searchLen = 
        let toCheck = recognize str crl srl erl nonterminals S searchLen
        assert (isAnswerValid toCheck (String.length str) searchLen)
        printMatrix toCheck (String.length str) searchLen 
//        printfn "%.10f" toCheck |> ignore
//        assert (abs(res - toCheck) < 1e-14) 

//    check "abb"     0.032        2 |> ignore    
//    check "aaabbcc" 0.0000524288 3 |> ignore

    check "abb"     2 |> ignore    
    check "abb"     3 |> ignore    
    check "aaabbcc" 3 |> ignore


    System.Console.ReadLine() |> ignore
    0