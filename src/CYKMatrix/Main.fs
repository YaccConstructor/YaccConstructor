module Main

    open Util
    open System.Collections.Generic
    open Printf

    [<EntryPoint>]
    let main args = 

        let A = NonTerminal "A"
        let B = NonTerminal "B"
        let BB = NonTerminal "BB"
        let S = NonTerminal "S"
        let nonterminals = [|A; B; S; BB|]

        let rawHeadsToProbs = List.map (fun (nt, prob) -> nt, Probability.create prob)

//        let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
//        [(A, BB), [S, true]; (S, S), [S, true]; (B, B), [BB, true; B, true]; (A, A), [A, true]] 
//        |> List.map (fun (nts, heads) -> nts, rawHeadsToProbs heads)
//        |> Seq.iter crl.Add        
        let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
        [(A, BB), [S, 0.5]; (S, S), [S, 0.5]; (B, B), [BB, 1.; B, 0.2]; (A, A), [A, 0.8]] 
        |> List.map (fun (nts, heads) -> nts, rawHeadsToProbs heads)
        |> Seq.iter crl.Add
//        let srl = new Dictionary<char, (NonTerminal * Probability.T) list>()
//        ['a', [A, true]; 'b', [B, true]; 'c', [B, true]] 
//        |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
//        |> Seq.iter srl.Add
        let srl = new Dictionary<char, (NonTerminal * Probability.T) list>()
        ['a', [A, 0.2]; 'b', [B, 0.4]; 'c', [B, 0.4]] 
        |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
        |> Seq.iter srl.Add
        let erl: NonTerminal list = []
        
    //    S -> A BB, 0.5
    //    S -> S S,  0.5
    //
    //    BB -> B B, 1.
    //
    //    A -> A A, 0.8
    //    A -> 'a', 0.2
    //
    //    B -> B B, 0.2
    //    B -> 'b', 0.4
    //    B -> 'c', 0.4
    
        let rules = new RulesHolder(crl, srl, erl)

        let printMatrix (matrix: ProbabilityMatrix.T) strLen searchLen =
            let rowLength = matrix.GetLength(0)
            let colLength = matrix.GetLength(1)

            for i in [0..rowLength-1] do
                for j in [0..colLength-1] do
                    if i <= strLen && j <= strLen && j > i && j-i <= searchLen then
//                    if i <= strLen && j <= strLen && j > i then
//                        printf "%b  " <| Probability.unwrap matrix.[i, j]
                        printf "%.8f  " <| Probability.unwrap matrix.[i, j]
                    else
                        assert (Probability.isZero matrix.[i, j])
//                        printf "%.8f  " matrix.[i, j]
                        printf "----------  "
                printfn ""
            printfn ""

        let isAnswerValid (matrix: ProbabilityMatrix.T) strLen searchLen = 
            let rowLength = matrix.GetLength(0)
            let colLength = matrix.GetLength(1)
            if rowLength <> colLength || rowLength <> strLen + 1 then
                false
            else
                let redundantCell (i, j) =
                        i > strLen 
                        || j > strLen 
                        || j <= i 
                        || j-i > searchLen 

                [0..rowLength-1]
                |> List.map (fun i -> [0..colLength-1] |> List.map (fun j -> (i,j))) 
                |> List.concat
                |> List.filter redundantCell
                |> List.forall (fun (i, j) -> Probability.isZero matrix.[i,j])

        let check str searchLen =             
            let toCheck    = CYKMatrix.recognize str rules nonterminals S searchLen
            let toCheckBFS = CYKMatrixBFS.recognize str rules nonterminals S searchLen false
            assert (isAnswerValid toCheck (String.length str) searchLen)
            assert (isAnswerValid toCheckBFS (String.length str) searchLen)
            let sameCells (i, j) = 
//                (Probability.unwrap toCheck.[i, j]) = (Probability.unwrap toCheckBFS.[i, j])
                (Probability.unwrap toCheck.[i, j]) - (Probability.unwrap toCheckBFS.[i, j]) < 0.0000001
            let sameAnswers =
                Seq.forall (fun i -> (Seq.forall (fun j -> sameCells (i,j)) [0 .. toCheck.GetLength(0) - 1]))
                           [0 .. toCheck.GetLength(0) - 1] 
            assert sameAnswers
            printMatrix toCheck (String.length str) searchLen 
            printMatrix toCheckBFS (String.length str) searchLen 

        let checkOneType task check taskType str searchLen =        
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            List.iter (fun _ -> task str searchLen |> ignore) [1..10] 
//            let toCheck = task str searchLen           
            stopWatch.Stop()
            printfn "type: %s, str length: %i, search length: %i, time(ms): %f." taskType (String.length str) searchLen stopWatch.Elapsed.TotalMilliseconds
            
        let checkTime str searchLen =
            checkOneType (fun str searchLen -> CYKMatrixBFS.recognize str rules nonterminals S searchLen true)
                         (fun toCheck -> isAnswerValid toCheck)
                         "parallel"
                         str
                         searchLen
            checkOneType (fun str searchLen -> CYKMatrix.recognize str rules nonterminals S searchLen)
                         (fun toCheck -> isAnswerValid toCheck)
                         "okhotin"
                         str
                         searchLen
            checkOneType (fun str searchLen -> CYKMatrixBFS.recognize str rules nonterminals S searchLen false)
                         (fun toCheck -> isAnswerValid toCheck)
                         "not parallel"
                         str
                         searchLen

//        check "abb"      2
        check "abb"      3    
//        check "aaabbcc"  5
//        check "aaabb"    5
//        check "aaaaabbb" 6
//        check "aaaabbbbbb" 6
//        check "aaaabbbbbbbbbbb" 10
//        check "aaaabbbbbbbbbbbbbbb" 10
//        check "aaaabb" 6
//        check "aaaabb" 6
//        check "aaaabb" 5
//        check "aaaabb" 4
//        check "aaaabb" 3
//        check "aaaabb" 2
//        check "aaaabb" 1
//        check "aaaabb" 0
//        
//        checkTime (String.replicate 300 "abb") 30
//        checkTime ((String.replicate 511 "abbb") + "abb") 50
         
//        check "aabb"
//        check "abb"    
//        check "aaabbcc"
//        check "baaabbcc"
//        check "aaaabbcc"

        System.Console.ReadLine() |> ignore
        0

