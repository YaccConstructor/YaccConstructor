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

// works only for string of 2^n-1 length
let recognize (strToParse: string) 
              (crl: Dictionary<NonTerminal * NonTerminal, (NonTerminal * double) list>)
              (srl: Dictionary<char, (NonTerminal * double) list>)
              (erl: NonTerminal list) 
              (nonterminals : NonTerminal [])
              S = 

    let n = String.length strToParse

    let tMatrix = new Map<NonTerminal, double [,]>(
                            nonterminals 
                            |> Seq.map (fun x -> (x, Array2D.init (n + 1) (n + 1) (fun x y -> 0.))))

    let pMatrix = new Map<NonTerminal * NonTerminal, double [,]>(
                            crl.Keys
                            |> Seq.map (fun x -> (x, Array2D.init (n + 1) (n + 1) (fun x y -> 0.)))) 

    let addToP nts (matrix: double [,]) (l1, m1, l2, m2) =
        let where = pMatrix.[nts]
        for i in [l1..m1-1] do
            for j in [l2..m2-1] do
                where.[i, j] <- (where.[i, j] + matrix.[i-l1, j-l2])         

    let subMatrixMult (matrixA: double [,]) (matrixB: double [,]) (al1, am1, al2, am2) (bl1, bm1, bl2, bm2) = 
        let n = am1 - al1
        let calcCell i j =
            [0..n-1] |> List.fold (fun acc k -> acc + matrixA.[i + al1, k + al2] * matrixB.[k + bl1, j + bl2]) 0. 
        Array2D.init n n calcCell                    
                                    
    let completeP where from1 from2 = 
        let completeOnePair (nt1, nt2) =
            addToP (nt1, nt2) (subMatrixMult tMatrix.[nt1] tMatrix.[nt2] from1 from2) where |> ignore
        pMatrix |> Map.toSeq |> Seq.map (fun (nts, _) -> completeOnePair nts) |> List.ofSeq |> ignore

    
    let rec compute l m =
        let mid = (l + m) / 2
        if m - l >= 4 then 
            compute l mid |> ignore
            compute mid m |> ignore
        completeT (l, mid, mid, m)

    and completeT (l1, m1, l2, m2) =
        assert (m1 - l1 = m2 - l2)

        if m1 - l1 = 1 && m1 = l2 then
            let currentChar = strToParse.[l1]
            srl.Item currentChar |> List.map (fun (nt, prob) -> (tMatrix.Item nt).[l1, l1 + 1] <- prob) |> ignore
        else if m1 - l1 = 1 && m1 < l2 then
            let headsFromTail (tail, tailProb) = 
                if crl.ContainsKey tail then 
                    crl.Item tail |> List.map (fun (head, headProb) -> (head, headProb * tailProb)) 
                else 
                    []

            let tails = pMatrix |> Map.map (fun _ probs -> probs.[l1, l2]) |> Map.filter (fun _ prob -> prob > 0.)
            let heads = tails |> Map.toSeq |> Seq.map headsFromTail |> Seq.concat

            heads |> Seq.map (fun (head, prob) -> (tMatrix.Item head).[l1, l2] <- (tMatrix.Item head).[l1, l2] + prob) |> List.ofSeq |> ignore

        else if m1 - l1 > 1 then
            let mid1: int = int (l1 + m1) / 2
            let mid2: int = int (l2 + m2) / 2
            let b1 = (l1, mid1, mid1, m1)
            let b2 = (l2, mid2, mid2, m2)
            let c  = (mid1, m1, l2, mid2)
            let d1 = (l1, mid1, l2, mid2)
            let d2 = (mid1, m1, mid2, m2)
            let  e = (l1, mid1, mid2, m2)
            completeT c |> ignore
            completeP d1 b1 c |> ignore
            completeT d1 |> ignore
            completeP d2 c b2 |> ignore
            completeT d2 |> ignore
            completeP e b1 d2 |> ignore
            completeP e d1 b2 |> ignore
            completeT e  |> ignore

    compute 0 (n + 1) |> ignore
    (tMatrix.Item S).[0,n]


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

    let check str res = 
        let toCheck = recognize str crl srl erl nonterminals S
//        printfn "%.10f" toCheck |> ignore
        assert (abs(res - toCheck) < 1e-14) 
      

    check "abb"     0.032        |> ignore    
    check "aaabbcc" 0.0000524288 |> ignore


    System.Console.ReadLine() |> ignore
    0