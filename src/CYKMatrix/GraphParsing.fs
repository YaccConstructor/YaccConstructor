module GraphParsing
    open Util
    open TPMatrices
    open System
    open System.Collections.Generic

    module Graph = 
        type T(_graph: Dictionary<int * int, ResizeArray<char>>, _numberOfVertices: int) =
            member this.graph = _graph

            member this.numberOfVertices = _numberOfVertices

            member this.IsCorrectVertex (v: int) =
                (1 <= v) && (v <= this.numberOfVertices)

            member this.AddEdge (v1: int, v2: int, label: char) =
                assert (this.IsCorrectVertex(v1) && this.IsCorrectVertex(v2))
                if not <| this.graph.ContainsKey (v1, v2)
                then
                    this.graph.Add((v1, v2), new ResizeArray<char>())
                if not <| this.graph.[(v1,v2)].Contains label
                then
                    this.graph.[(v1,v2)].Add(label)

            member this.GetEdges () =
                this.graph.Keys

            member this.GetLabels (v1: int, v2: int) =
                assert (this.IsCorrectVertex(v1) && this.IsCorrectVertex(v2))
                this.graph.[(v1, v2)]


    type ParsingMatrix = Dictionary<NonTerminal, ProbabilityMatrix.T>

    let unionArrays (arr1: float []) (arr2: float []) (size: int) =
        Array.init size (fun i -> if arr1.[i] > 0.0 then 1.0 else arr2.[i])

    let unionWithOneCell (arr: float []) (v1: int) (v2: int) (matrixSize: int) =
        Array.init (matrixSize * matrixSize) (fun i -> if arr.[i] > 0.0
                                                       then 1.0
                                                       else 
                                                           let row = i / matrixSize
                                                           let col = i - row * matrixSize
                                                           if (v1 - 1 = row) && (v2 - 1 = col)
                                                           then 1.0
                                                           else 0.0)

    let multArrays (from1: Probability.InnerType.T []) (from2: Probability.InnerType.T []) matricesSize actualColCount =        
            let calculateCell (n, i, j) = 
                let skipMatrices = n * matricesSize * matricesSize
                let skipRows = skipMatrices + i * matricesSize
                let skipColumns = skipMatrices + j * matricesSize                
                Array.fold2 (fun b v1 v2 -> Probability.innerSumm b <| Probability.innerMult v1 v2)
                            Probability.InnerType.zero
                            from1.[skipRows..skipRows + matricesSize - 1] 
                            from2.[skipColumns..skipColumns + matricesSize - 1]

            // todo: recurr
            let getNIJ x = 
                let n = x / (matricesSize * matricesSize)
                let i = (x - n * matricesSize * matricesSize) / matricesSize
                let j = x - n * matricesSize * matricesSize - i * matricesSize
                n, i, j
            Array.init (matricesSize * actualColCount) (fun x -> calculateCell <| getNIJ x)

    let initParsingMatrix (graph:Graph.T)
                  (allRules: RulesHolder)
                  nonterminals =
        let parsingMatrix = new ParsingMatrix ()
        do 
            (
                nonterminals 
                |> Seq.map (fun x -> x, ProbabilityMatrix.empty (graph.numberOfVertices))
            )
            |> Seq.iter parsingMatrix.Add

        for (v1, v2) in graph.GetEdges() do
            let labels = graph.GetLabels(v1, v2)
            for label in labels do
                if allRules.IsSimpleTail label
                then
                    let simpleNonterminals = allRules.HeadsBySimpleTail label
                    for (simpleNonterminal, _) in simpleNonterminals do
                        let arr = parsingMatrix.[simpleNonterminal].GetSubArray id false parsingMatrix.[simpleNonterminal].WholeMatrix
                        let updatedArr = unionWithOneCell arr v1 v2 graph.numberOfVertices
                        let updatedMatrix = ProbabilityMatrix.create parsingMatrix.[simpleNonterminal].Size updatedArr
                        parsingMatrix.Remove(simpleNonterminal) |> ignore
                        parsingMatrix.Add(simpleNonterminal, updatedMatrix)
        
        parsingMatrix

    let squareParsingMatrix (parsingMatrix: ParsingMatrix)
                            (allRules: RulesHolder) =
        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do

            let arr1 = parsingMatrix.[nt1].GetSubArray id false parsingMatrix.[nt1].WholeMatrix
            let arr2 = parsingMatrix.[nt2].GetSubArray id true parsingMatrix.[nt2].WholeMatrix
            let resultArray = multArrays arr1 arr2 parsingMatrix.[nt1].Size parsingMatrix.[nt1].Size
            let resultMatix = ProbabilityMatrix.create parsingMatrix.[nt1].Size resultArray

            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                let curArray = parsingMatrix.[nonTerm].GetSubArray id false parsingMatrix.[nonTerm].WholeMatrix
                let updatedArray = unionArrays (resultMatix.GetSubArray id false resultMatix.WholeMatrix) curArray curArray.Length
                let updatedMatrix = ProbabilityMatrix.create parsingMatrix.[nt1].Size updatedArray
                parsingMatrix.Remove(nonTerm) |> ignore
                parsingMatrix.Add(nonTerm, updatedMatrix)

    let recognizeGraph (graph:Graph.T)
                  (allRules: RulesHolder)
                  nonterminals
                  S =

        let parsingMatrix = initParsingMatrix graph allRules nonterminals

        for i in [1..graph.numberOfVertices] do
            squareParsingMatrix parsingMatrix allRules

        parsingMatrix.[S]

