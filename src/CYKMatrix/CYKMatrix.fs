module CYKMatrix

open Util

let recognize (options: Options.T) inputToParse allRules nonterminals S maxSearchLength  = 
    let result, multiplicationCount =
        match options.algorithm with
        | Okhotin -> CYKMatrixOkhotin.recognize options inputToParse allRules nonterminals S maxSearchLength 
        | Modified -> CYKMatrixBFS.recognize options inputToParse allRules nonterminals S maxSearchLength 

    match options.mode with
    | Work -> ()
    | Test -> 
        if inputToParse.Length > maxSearchLength 
        then failwith "testing multiplications count is only availible for inputToParse.Length = maxSearchLength"
        else if (1 <<< log2 (inputToParse.Length + 1)) <> inputToParse.Length + 1
        then failwith "testing multiplications count is only availible for inputToParse.Length = 2^k - 1"        

    let testMultiplicationCount (arr: _ []) =
        for ki in 0 .. arr.Length - 1 do
            let i = (log2 <| inputToParse.Length + 1) - ki 
            let multCount = (1 <<< (2 * i - 1)) - (1 <<< i)
            if multCount <> arr.[ki] 
            then failwith "incorrect multiplication count"
            printfn "matrices of size2^%d: %d" i arr.[ki]

    Option.iter testMultiplicationCount multiplicationCount

    result




