module YC.Parsers.BottomUp

open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic

[<Struct>]
type Grammar =
    val grammar:HashSet<int> [,]
    val startFinal: ResizeArray<int*HashSet<int>*HashSet<int>>
    new (grm, strtFnl) = {grammar = grm; startFinal = strtFnl}

let closure (atm:Matrix<_>) =
    let zeros =  Matrix.foldSkipZeros (fun i j -> i + 1) 0 atm |> ref
    let mutable res = atm
    let _go = ref true
    while !_go do
        res <- res.Add (res.Multiply res)
        let newZeros = Matrix.foldSkipZeros (fun i j -> i + 1) 0 res
        _go := not (!zeros = newZeros )
        zeros := newZeros
    res

let main (input:HashSet<int> [,]) (grammar:Grammar) =
    let inputSize = Array2D.length1 input
    let grammarSize = Array2D.length1 grammar.grammar    
    let mutable intersectionResult = 
        SparseMatrix.create (inputSize * grammarSize) (inputSize * grammarSize) 0.0
    let stateRemap =
        let m = new ResizeArray<_>()
        for i in 0 .. inputSize-1 do
            for j in 0 .. grammarSize-1 do
                m.Add((i,j))
        m

    let getEdgesCount a = 
        let mutable i = 0
        Array2D.iter (fun (t:HashSet<_>) -> i <- i + t.Count) a
        i

    let mutable inputEdges = getEdgesCount input
    let mutable _go = true

    while _go do
        intersectionResult 
        |> Matrix.iteri (fun i j n -> 
            let _startInput,_startGrammar = stateRemap.[i] 
            let _endInput,_endGrammar = stateRemap.[j]
            if input.[_startInput, _endInput].Overlaps grammar.grammar.[_startGrammar, _endGrammar]
               || (_startInput = _endInput && _startGrammar = _endGrammar)
            then intersectionResult.[i,j] <- 1.0
            )
        let cls = closure intersectionResult
        cls
        |> Matrix.iteriSkipZeros (fun i j n ->
            let _startInput,_startGrammar = stateRemap.[i] 
            let _endInput,_endGrammar = stateRemap.[j]
            let startFinal = grammar.startFinal |> Seq.tryFind (fun (_,s,_) -> s.Contains _startGrammar)
            match startFinal with
            | Some (n,s,f) when f.Contains _endGrammar -> input.[_startInput,_endInput].Add n |> ignore
            | _ -> ()
            )
        let newEdgesCount = getEdgesCount input
        _go <- not (newEdgesCount = inputEdges)
        inputEdges <- newEdgesCount

    input