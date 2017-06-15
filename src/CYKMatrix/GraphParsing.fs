module GraphParsing
    open MatrixKernels
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
    open Microsoft.FSharp.Core.Operators
    open MathNet.Numerics.LinearAlgebra.Double

               

    type Message = bool

    let recognizeGraphP<'InnerType when 'InnerType : comparison> (graph:AbstractAnalysis.Common.SimpleInputGraph<int>)
                  (matrixInitializator:AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> seq<NonTerminal> -> (ParsingMatrix<SparseMatrix> * Dictionary<int,int>))
                  (allRules: RulesHolder)
                  nonterminals
                  S =
        let parsingMatrixCurrent, vertexToInt = matrixInitializator graph allRules nonterminals
        let matrixSize = graph.VertexCount
        let isChanged = ref true
        let mutable multCount = 0
        let parsingMatrixNew = new ParsingMatrix<SparseMatrix>()
        for nont in parsingMatrixCurrent.Keys do
            parsingMatrixNew.Add(nont, new SparseMatrix(matrixSize))
            parsingMatrixCurrent.[nont].CopyTo(parsingMatrixNew.[nont])


        let nontermPairs = allRules.ComplexTails

        let splitCount = 4

        let splitedNontermPairs = nontermLockFreeSplit allRules nonterminals splitCount

        let flags = Array.init splitCount (fun _ -> ref false)
        let values = Array.init splitCount (fun _ -> ref false)
        
        let mbp flg vl nontermPairs_mbp = new MailboxProcessor<Message>(fun inbox ->
            let rec loop n =
                async {                                  
                        let! message = inbox.Receive();
                        for (nt1, nt2) in nontermPairs_mbp do
                            let matrix1 = parsingMatrixCurrent.[nt1]
                            let matrix2 = parsingMatrixCurrent.[nt2]
                            let resultMatrix = matrix1.Multiply(matrix2)          
                            for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                                    let nonZ = parsingMatrixCurrent.[nonTerm].NonZerosCount
                                    //lock parsingMatrix (fun () ->
                                    parsingMatrixNew.[nonTerm].PointwiseMaximum(resultMatrix, parsingMatrixNew.[nonTerm])
                                    //)
                                    if (nonZ <> parsingMatrixNew.[nonTerm].NonZerosCount)
                                    then 
                                        vl := true
                        flg:= true
                        do! loop (n + 1)
                }
            loop (0))
        
        let mailBoxes = Array.init splitCount (fun i -> mbp flags.[i] values.[i] splitedNontermPairs.[i])

        for i in 0..(mailBoxes.Length-1) do
            mailBoxes.[i].Start()

        while !isChanged do
            isChanged := false
            for i in 0..(mailBoxes.Length-1) do
                mailBoxes.[i].Post(false)

            while not <| Array.TrueForAll (flags, (fun fl -> !fl)) do ()

            for i in 0..(flags.Length-1) do
                flags.[i] := false

            isChanged := Array.Exists (values, (fun vl -> !vl))

            for i in 0..(values.Length-1) do
                values.[i] := false
            
            for nont in parsingMatrixNew.Keys do
                parsingMatrixNew.[nont].CopyTo(parsingMatrixCurrent.[nont])

            multCount <- multCount + 1            

        (parsingMatrixNew.[S], vertexToInt, multCount)


    let initRulesFromIL loadIL tokenToInt =
        let grammar = loadIL.grammar
        let mutable tokensCount = 0
        let S = ref (NonTerminal "")
        let nonterminals = new ResizeArray<NonTerminal>()
        let crl = new Dictionary<NonTerminal * NonTerminal, ResizeArray<NonTerminal*Probability.T>>()
        let srl = new Dictionary<int, ResizeArray<NonTerminal*Probability.T>>()
        let crl_result = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
        let srl_result = new Dictionary<int, (NonTerminal * Probability.T) list>()
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

        (rulesHolder, nonterminals, S)
 
    let graphParseParallel<'InnerType when 'InnerType : comparison> (graph:AbstractAnalysis.Common.SimpleInputGraph<int>)
                  (matrixInitializator:AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> seq<NonTerminal> -> (ParsingMatrix<SparseMatrix> * Dictionary<int,int>))
                  (loadIL:t<Source.t, Source.t>)
                  tokenToInt =

        let (rulesHolder, nonterminals, S) = initRulesFromIL loadIL tokenToInt

        recognizeGraphP<'InnerType> graph matrixInitializator rulesHolder nonterminals !S


    let recognizeGraph<'MatrixType, 'InnerType when 'InnerType : comparison> graph
                  (matrixInitializator:AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> seq<NonTerminal> -> (ParsingMatrix<'MatrixType> * Dictionary<int,int>))
                  (squareMatrix:ParsingMatrix<'MatrixType> -> RulesHolder -> bool ref -> int  -> unit)
                  (allRules: RulesHolder)
                  nonterminals
                  S =
        let parsingMatrix, vertexToInt = matrixInitializator graph allRules nonterminals
        printfn "Matrix initialized"
        let matrixSize = graph.VertexCount
        let isChanged = ref true
        let mutable multCount = 0

        while !isChanged do
            isChanged := false
            squareMatrix parsingMatrix allRules isChanged matrixSize
            printfn "Multiplication done"
            multCount <- multCount + 1

        (parsingMatrix.[S], vertexToInt, multCount)    

    let graphParse<'MatrixType, 'InnerType when 'InnerType : comparison> (graph:AbstractAnalysis.Common.SimpleInputGraph<int>)
                  (matrixInitializator:AbstractAnalysis.Common.SimpleInputGraph<int> -> RulesHolder -> seq<NonTerminal> -> (ParsingMatrix<'MatrixType> * Dictionary<int,int>))
                  squareMatrix
                  (loadIL:t<Source.t, Source.t>)
                  tokenToInt =

        let (rulesHolder, nonterminals, S) = initRulesFromIL loadIL tokenToInt

        recognizeGraph<'MatrixType, 'InnerType> graph matrixInitializator squareMatrix rulesHolder nonterminals !S

