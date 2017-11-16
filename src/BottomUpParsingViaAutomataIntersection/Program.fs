module YC.Parsers.BottomUp

open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic
open MatrixKernels

[<Struct>]
type Grammar =
    val grammar:HashSet<int> [,]
    val startFinal: array<int*HashSet<int>*HashSet<int>>
    new (grm, strtFnl) = {grammar = grm; startFinal = strtFnl}

let main (input:HashSet<int> [,]) (grammar:Grammar) =
    let inputSize = Array2D.length1 input
    let grammarSize = Array2D.length1 grammar.grammar    
    let intersectionResult = Double.SparseMatrix.Create((inputSize * grammarSize), (inputSize * grammarSize), 0.0)
    let csr = (intersectionResult.Storage :?> Storage.SparseCompressedRowMatrixStorage<_>)    
    let gpuSparseMatrix = new MySparseMatrix(intersectionResult.RowCount, intersectionResult.NonZerosCount, csr.Values, csr.RowPointers, csr.ColumnIndices)
    let gpuSparseMatrix1 = new MySparseMatrix(intersectionResult.RowCount, intersectionResult.NonZerosCount, csr.Values, csr.RowPointers, csr.ColumnIndices)

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

    let closure () =    
        let mutable res = sparseCudaGeam gpuSparseMatrix gpuSparseMatrix1 intersectionResult.RowCount
        let mutable zeros = res.Nnz
        let _go = ref true
        let ctr = ref 0
        while !_go do
            incr ctr
            res <- sparseCudaGeam res (sparseCudaGemm res res intersectionResult.RowCount) intersectionResult.RowCount
            let newZeros = res.Nnz
            _go := not (zeros = newZeros)
            zeros <- newZeros
        printfn "!!!%A" !ctr
        csr.ColumnIndices <- res.CsrColInd
        res.CsrRow.CopyTo(csr.RowPointers,0)
        csr.Values <- res.CsrVal
        
    let mutable _go = true

    let toUpdate = new ResizeArray<_>()

    while _go do
        for _startInput in 0..inputSize-1 do
            for _endInput in 0..inputSize-1 do
                if input.[_startInput,_endInput].Count <> 0 || _startInput = _endInput
                then 
                    for _startGrammar in 0..grammarSize-1 do
                        for _endGrammar in 0..grammarSize-1 do
                            if ((_startInput = _endInput && _startGrammar = _endGrammar)
                               || input.[_startInput,_endInput].Overlaps grammar.grammar.[_startGrammar, _endGrammar])
                            then 
                                 toUpdate.Add(backStateRemap.[(_startInput,_startGrammar)], backStateRemap.[(_endInput,_endGrammar)], 1.0)

        let upCsr = Storage.SparseCompressedRowMatrixStorage.OfIndexedEnumerable(intersectionResult.RowCount,intersectionResult.RowCount,toUpdate) 
        gpuSparseMatrix1.Update (toUpdate.Count, upCsr.Values, upCsr.RowPointers, upCsr.ColumnIndices)    
        closure ()
        toUpdate.Clear()
        _go <- false
        intersectionResult
        |> Matrix.iteriSkipZeros (fun i j n ->
            let _startInput,_startGrammar = stateRemap.[i] 
            let _endInput,_endGrammar = stateRemap.[j]
            let startFinal = grammar.startFinal |> Array.tryFind (fun (_,s,_) -> s.Contains _startGrammar)
            match startFinal with
            | Some (n,s,f) when f.Contains _endGrammar -> _go <- input.[_startInput,_endInput].Add n || _go
            | _ -> ()
            )

    input

let parse (input:AbstractAnalysis.Common.IParserInput) (grammar:Yard.Core.IL.Definition<_,_>) =
    let grammar =
        let expandMeta = new Yard.Core.Conversions.ExpandMeta.ExpandMeta()
        let g = expandMeta.ConvertGrammar grammar.grammar
        let fsa = new Yard.Generators.Common.FSA.FSA(g.Head.rules)
        let encoder = 
            let mapper = new Dictionary<_,_>()
            fsa.Alphabet |> Seq.iteri (fun i x -> mapper.Add(x,i))
            mapper        
        let matrix = Array2D.init fsa.States.Length fsa.States.Length (fun i j -> new HashSet<_>([|encoder.[fst fsa.States.[i].[j]]|]))
        let startFinal = 
            let res = 
                fsa.Alphabet
                |> Seq.choose (function | Yard.Generators.Common.FSA.Common.EdgeSymbol.Nonterm x -> Some x | _ -> None)
                |> Seq.map (fun nt -> encoder.[Yard.Generators.Common.FSA.Common.EdgeSymbol.Nonterm nt], new HashSet<_>([|nt|]), new HashSet<_>())
                |> Array.ofSeq
            fsa.LastStates
            
        0
    0

let pp grammar =
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let grammar = fe.ParseGrammar grammar
    let expandMeta = new Yard.Core.Conversions.ExpandMeta.ExpandMeta()
    let g = expandMeta.ConvertGrammar grammar.grammar
    let fsa = new Yard.Generators.Common.FSA.FSA(g.Head.rules)
    printfn "%A" fsa
    
do pp 
    @"C:\gsv\projects\YC\YaccConstructor\src\YC.GrammarZOO\Bio\16s\R16St.yrd "
    //@"C:\gsv\projects\YC\YaccConstructor\y.yrd"
