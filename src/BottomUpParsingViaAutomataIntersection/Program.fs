module YC.Parsers.BottomUp

open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic

[<Struct>]
type Grammar =
    val grammar:HashSet<int> [,]
    val startFinal: array<int*HashSet<int>*HashSet<int>>
    new (grm, strtFnl) = {grammar = grm; startFinal = strtFnl}

let closure (atm:Single.SparseMatrix) =
    let mutable zeros = atm.NonZerosCount
    let mutable res = atm
    let _go = ref true
    while !_go do
        res <- (res.Add (res.Multiply res) :?> Single.SparseMatrix)
        let newZeros = res.NonZerosCount
        _go := not (zeros = newZeros )
        zeros <- newZeros
    res

let main (input:HashSet<int> [,]) (grammar:Grammar) =
    let inputSize = Array2D.length1 input
    let grammarSize = Array2D.length1 grammar.grammar    
    let mutable intersectionResult = 
        Single.SparseMatrix.Create((inputSize * grammarSize), (inputSize * grammarSize), 0.0f)
    let stateRemap,backStateRemap =
        let m = new Dictionary<_,_>()
        let n = new ResizeArray<_>()
        let k = ref 0
        for i in 0 .. inputSize-1 do
            for j in 0 .. grammarSize-1 do
                m.Add((i,j),!k)
                n.Add((i,j))
                incr k
        n,m

    let getEdgesCount a = 
        let mutable i = 0
        Array2D.iter (fun (t:HashSet<_>) -> i <- i + t.Count) a
        i

    let mutable inputEdges = getEdgesCount input
    let mutable _go = true

    while _go do
        for _startInput in 0..inputSize-1 do
            for _endInput in 0..inputSize-1 do
                if input.[_startInput, _endInput].Count <> 0 || _startInput = _endInput
                then 
                    for _startGrammar in 0..grammarSize-1 do
                        for _endGrammar in 0..grammarSize-1 do
                            if input.[_startInput, _endInput].Overlaps grammar.grammar.[_startGrammar, _endGrammar]
                               || (_startInput = _endInput && _startGrammar = _endGrammar)
                            then intersectionResult.[backStateRemap.[(_startInput,_startGrammar)] ,backStateRemap.[(_endInput,_endGrammar)]] <- 1.0f

        let cls = closure intersectionResult
        cls
        |> Matrix.iteriSkipZeros (fun i j n ->
            let _startInput,_startGrammar = stateRemap.[i] 
            let _endInput,_endGrammar = stateRemap.[j]
            let startFinal = grammar.startFinal |> Array.tryFind (fun (_,s,_) -> s.Contains _startGrammar)
            match startFinal with
            | Some (n,s,f) when f.Contains _endGrammar -> input.[_startInput,_endInput].Add n |> ignore
            | _ -> ()
            )
        let newEdgesCount = getEdgesCount input
        _go <- not (newEdgesCount = inputEdges)
        inputEdges <- newEdgesCount

    input