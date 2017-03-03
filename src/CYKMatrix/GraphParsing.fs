module GraphParsing
    open Util
    open TPMatrices
    open System
    open System.Collections.Generic
    open Yard.Core
    open Yard.Core.IL
    open Yard.Core.IL.Production
    open Yard.Core.IL.Definition
    open Yard.Core.Helpers
    open Conversions.TransformAux
    open QuickGraph
    open Alea.CUDA
    open Alea.CUDA.CULib
    open Alea.CUDA.Utilities
    open MathNet.Numerics.LinearAlgebra.Double

    type ParsingMatrix<'MatrixType> = Dictionary<NonTerminal, 'MatrixType>

    let initParsingMatrix<'MatrixType, 'InnerType when 'InnerType : comparison> (graph: AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>)
                  (allRules: RulesHolder)
                  nonterminals
                  createEmptyMatrix 
                  (getInnerValue : 'MatrixType -> 'InnerType[]) 
                  (innerOne: 'InnerType) =
        let vertexToInt = new Dictionary<_,_>()
        let mutable procVertices = 0
        let parsingMatrix = new ParsingMatrix<'MatrixType> ()
        do 
            (
                nonterminals 
                |> Seq.map (fun x -> x, createEmptyMatrix (graph.VertexCount))
            )
            |> Seq.iter parsingMatrix.Add

        for vertex in graph.Vertices do
            vertexToInt.Add(vertex, procVertices)
            procVertices <- procVertices + 1

        for edg in graph.Edges do
            let label = edg.Tag
            if allRules.IsSimpleTail label
            then
                let simpleNonterminals = allRules.HeadsBySimpleTail label
                for (simpleNonterminal, _) in simpleNonterminals do
                    let data = getInnerValue parsingMatrix.[simpleNonterminal]
                    let row = vertexToInt.[edg.Source]
                    let col = vertexToInt.[edg.Target]
                    let updadetInd = graph.VertexCount * row + col
                    data.[updadetInd] <- innerOne
        
        parsingMatrix, vertexToInt

    let naiveSquareMatrix<'MatrixType, 'InnerType when 'InnerType : comparison> getInnerValue toArray (innerSum: 'InnerType -> 'InnerType -> 'InnerType)
                (innerMult: 'InnerType -> 'InnerType -> 'InnerType) (innerZero: 'InnerType) (innerOne: 'InnerType)
                (matrix: ParsingMatrix<'MatrixType>) (allRules: RulesHolder) isChanged matrixSize =
        let unionArrays (curArr: 'InnerType []) (updArr: 'InnerType []) (arrSize: int) =
            for ind in 0..arrSize - 1 do
                if curArr.[ind] = innerZero && updArr.[ind] > innerZero
                then
                    isChanged := true
                    curArr.[ind] <- innerOne

        let multArrays (from1: 'InnerType []) (from2: 'InnerType []) =        
                let calculateCell x =
                    let i = x / matrixSize
                    let j = x - i * matrixSize 
                    let skipRows = i * matrixSize
                    let skipColumns = j * matrixSize                
                    Array.fold2 (fun b v1 v2 -> innerSum b <| innerMult v1 v2)
                                innerZero
                                from1.[skipRows..skipRows + matrixSize - 1] 
                                from2.[skipColumns..skipColumns + matrixSize - 1]

                Array.init (matrixSize * matrixSize) (fun x -> calculateCell <| x)

        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let arr1 = toArray matrix.[nt1] false
            let arr2 = toArray matrix.[nt2] true
            let resultArray = multArrays arr1 arr2

            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                unionArrays (getInnerValue matrix.[nonTerm]) resultArray (matrixSize*matrixSize)

    let sparseSquareMatrix (matrix: ParsingMatrix<SparseMatrix>) (allRules: RulesHolder) isChanged matrixSize =
        let unionArrays (curArr: float []) (updArr: float []) (arrSize: int) =
            for ind in 0..arrSize - 1 do
                if curArr.[ind] = 0.0 && updArr.[ind] > 0.0
                then
                    isChanged := true
                    curArr.[ind] <- 1.0

        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let matrix1 = matrix.[nt1]
            let matrix2 = matrix.[nt2]
            let resultMatrix = matrix1.Multiply(matrix2)
            let resultArray = resultMatrix.AsRowMajorArray()
            
            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                let curArray = matrix.[nonTerm].AsRowMajorArray()
                unionArrays curArray resultArray (matrixSize*matrixSize)

(*    let worker = Worker.Default

    let cublasSquareMatrix (matrix: ParsingMatrix) (allRules: RulesHolder) isChanged =
        let unionArrays (curArr: Probability.InnerType.T []) (updArr: Probability.InnerType.T []) (arrSize: int) =
            for ind in 0..arrSize - 1 do
                if curArr.[ind] = 0.0 && updArr.[ind] > 0.0
                then
                    isChanged := true
                    curArr.SetValue(1.0, ind)

        let multArrays (from1: Probability.InnerType.T []) (from2: Probability.InnerType.T []) (matricesSize:int) =        

                let transa = cublasOperation_t.CUBLAS_OP_N
                let transb = cublasOperation_t.CUBLAS_OP_N

                let dalpha = 1.
                let dbeta = 0.

                let multiplicationResult =               
                    let (mult1:DeviceMemory<float>) = worker.Malloc(matricesSize * matricesSize)
                    let (mult2:DeviceMemory<float>) = worker.Malloc(matricesSize * matricesSize)
                    let (result:DeviceMemory<float>) = worker.Malloc(matricesSize * matricesSize)
                    mult1.Scatter(from1)
                    mult2.Scatter(from2) 

                    CUBLAS.Default.Dgemm(transa, 
                                            transb, 
                                            matricesSize, 
                                            matricesSize, 
                                            matricesSize, 
                                            dalpha, 
                                            mult2.Ptr, 
                                            matricesSize, 
                                            mult1.Ptr, 
                                            matricesSize, 
                                            dbeta, 
                                            result.Ptr, 
                                            matricesSize)

                    let resultArr = result.Gather()  
                    resultArr

                multiplicationResult

        let nontermPairs = allRules.ComplexTails
        for (nt1, nt2) in nontermPairs do
            let size = matrix.[nt1].Size
            let arr1 = matrix.[nt1].GetSubArray id false matrix.[nt1].WholeMatrix
            let arr2 = matrix.[nt2].GetSubArray id true matrix.[nt2].WholeMatrix
            let resultArray = multArrays arr1 arr2 size

            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                unionArrays matrix.[nonTerm].InnerValue resultArray (size*size)*) 

    let recognizeGraph<'MatrixType, 'InnerType when 'InnerType : comparison> (graph:AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>)
                  (squareMatrix:ParsingMatrix<'MatrixType> -> RulesHolder -> bool ref -> int  -> unit)
                  (allRules: RulesHolder)
                  nonterminals
                  S 
                  createEmptyMatrix 
                  getInnerValue 
                  (innerOne: 'InnerType) =
        let parsingMatrix, vertexToInt = initParsingMatrix<'MatrixType, 'InnerType> graph allRules nonterminals createEmptyMatrix getInnerValue innerOne
        let matrixSize = graph.VertexCount
        let isChanged = ref true
        let mutable multCount = 0

        while !isChanged do
            isChanged := false
            squareMatrix parsingMatrix allRules isChanged matrixSize
            multCount <- multCount + 1

        (parsingMatrix.[S], vertexToInt, multCount)    

    let graphParse<'MatrixType, 'InnerType when 'InnerType : comparison> (graph:AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>)
                  squareMatrix
                  (loadIL:t<Source.t, Source.t>)
                  tokenToInt 
                  createEmptyMatrix 
                  getInnerValue 
                  (innerOne: 'InnerType) =
        let grammar = loadIL.grammar
        let mutable tokensCount = 0
        let S = ref (NonTerminal "")
        let nonterminals = new ResizeArray<NonTerminal>()
        let crl = new Dictionary<NonTerminal * NonTerminal, ResizeArray<NonTerminal*Probability.T>>()
        let srl = new Dictionary<int<AbstractAnalysis.Common.token>, ResizeArray<NonTerminal*Probability.T>>()
        let crl_result = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
        let srl_result = new Dictionary<int<AbstractAnalysis.Common.token>, (NonTerminal * Probability.T) list>()
        let erl_result: NonTerminal list = []

        let probOne = Probability.create 1.0

        for module' in grammar do
            for r in module'.rules do
                let nonterm = NonTerminal <| Source.toString r.name
                if not <| nonterminals.Contains nonterm
                then
                    nonterminals.Add nonterm
                    if r.isStart
                    then
                        S := nonterm

                match r.body with
                | PSeq([elem],_,_) ->
                    match elem.rule with
                    | PToken src ->
                        let token = Source.toString src
                        let intToken = tokenToInt token
                        if not <| srl.ContainsKey intToken
                        then
                            srl.Add(intToken, new ResizeArray<NonTerminal*Probability.T>())
                        if not <| srl.[intToken].Contains (nonterm, probOne)
                        then
                            srl.[intToken].Add (nonterm, probOne)
                    | _ ->
                        failwith "Given grammar is not in normal form."
                        
                | PSeq([e1; e2],_,_) ->
                    match e1.rule, e2.rule with 
                    | PRef (name1, _), PRef (name2, _) ->
                        let nonterm1 = NonTerminal <| Source.toString name1
                        if not <| nonterminals.Contains nonterm1
                        then
                            nonterminals.Add nonterm1
                        let nonterm2 = NonTerminal <| Source.toString name2
                        if not <| nonterminals.Contains nonterm2
                        then
                            nonterminals.Add nonterm2
                        if not <| crl.ContainsKey (nonterm1, nonterm2)
                        then
                            crl.Add((nonterm1, nonterm2), new ResizeArray<NonTerminal*Probability.T>())
                        if not <| crl.[(nonterm1, nonterm2)].Contains (nonterm, probOne)
                        then
                            crl.[(nonterm1, nonterm2)].Add (nonterm, probOne)                     
                    | _ -> failwith "Given grammar is not in normal form."               
                | _ -> failwith "Given grammar is not in normal form."

        for key in crl.Keys do
            let list = Seq.toList crl.[key]
            crl_result.Add(key, list)
        for key in srl.Keys do
            let list = Seq.toList srl.[key]
            srl_result.Add(key, list)
        
        let rulesHolder = new RulesHolder(crl_result, srl_result, erl_result)

        recognizeGraph<'MatrixType, 'InnerType> graph squareMatrix rulesHolder nonterminals !S  createEmptyMatrix getInnerValue innerOne

