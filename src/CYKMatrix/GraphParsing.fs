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

    let initParsingMatrix (graph:Graph.T)
                  (allRules: RulesHolder)
                  nonterminals =
        let unionWithOneCell (arr: float []) (v1: int) (v2: int) (matrixSize: int) =
            Array.init (matrixSize * matrixSize) (fun i -> if arr.[i] > 0.0
                                                           then 1.0
                                                           else 
                                                               let row = i / matrixSize
                                                               let col = i - row * matrixSize
                                                               if (v1 - 1 = row) && (v2 - 1 = col)
                                                               then 1.0
                                                               else 0.0)
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

    let naiveSquareMatrix (matrix: ParsingMatrix) (allRules: RulesHolder) isChanged =
        let unionArrays (arr1: float []) (arr2: float []) (size: int) =
            let modificator i =
                if arr1.[i] <> arr2.[i]
                then isChanged := true

                if arr1.[i] > 0.0
                then 1.0
                else arr2.[i]
            Array.init size modificator

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

        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let size = matrix.[nt1].Size
            let arr1 = matrix.[nt1].GetSubArray id false matrix.[nt1].WholeMatrix
            let arr2 = matrix.[nt2].GetSubArray id true matrix.[nt2].WholeMatrix
            let resultArray = multArrays arr1 arr2 size size
            let resultMatix = ProbabilityMatrix.create size resultArray

            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                let curArray = matrix.[nonTerm].GetSubArray id false matrix.[nonTerm].WholeMatrix
                let updatedArray = unionArrays (resultMatix.GetSubArray id false resultMatix.WholeMatrix) curArray curArray.Length
                let updatedMatrix = ProbabilityMatrix.create matrix.[nonTerm].Size updatedArray
                matrix.Remove(nonTerm) |> ignore
                matrix.Add(nonTerm, updatedMatrix)


    let recognizeGraph (graph:Graph.T)
                  squareMatrix
                  (allRules: RulesHolder)
                  nonterminals
                  S =

        let parsingMatrix = initParsingMatrix graph allRules nonterminals
        let isChanged = ref true
        let mutable multCount = 0

        while !isChanged do
            isChanged := false
            squareMatrix parsingMatrix allRules isChanged
            multCount <- multCount + 1

        (parsingMatrix.[S], multCount)

