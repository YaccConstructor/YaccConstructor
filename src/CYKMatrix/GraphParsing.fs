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

    type Message = bool

    let recognizeGraph<'MatrixType, 'InnerType when 'InnerType : comparison> graph
                    (mHandler : IMatrixHandler<'MatrixType, 'InnerType>)
                    (allRules: RulesHolder)
                    nonterminals
                    S 
                    parallelProcesses =
        if (parallelProcesses = 1)
        then
            let parsingMatrix, vertexToInt = mHandler.ParsingMatrixInitializator graph allRules nonterminals
            //printfn "Matrix initialized"
            let matrixSize = graph.VertexCount
            let isChanged = ref true
            let mutable multCount = 0

            while !isChanged do
                isChanged := false
                for (nt1, nt2) in allRules.ComplexTails do
                    let matrix1 = parsingMatrix.[nt1]
                    let matrix2 = parsingMatrix.[nt2]
                    let resultMatrix = mHandler.Multiply matrix1 matrix2          
                    for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                        let nonZ = mHandler.getNonZerosCount parsingMatrix.[nonTerm]
                        let updatedMatrix = mHandler.Add parsingMatrix.[nonTerm] resultMatrix
                        parsingMatrix.Remove(nonTerm) |> ignore
                        parsingMatrix.Add(nonTerm, updatedMatrix)
                        if (nonZ <> mHandler.getNonZerosCount parsingMatrix.[nonTerm])
                        then 
                            isChanged := true
                //printfn "Iteration done"
                multCount <- multCount + 1

            (parsingMatrix, S, vertexToInt, multCount)
        else
            let parsingMatrixCurrent, vertexToInt = mHandler.ParsingMatrixInitializator graph allRules nonterminals
            let matrixSize = graph.VertexCount
            let isChanged = ref true
            let mutable multCount = 0
            let parsingMatrixNew = new ParsingMatrix<'MatrixType>()
            let emptyMatrix = mHandler.createEmptyMatrix matrixSize
            for nont in parsingMatrixCurrent.Keys do
                parsingMatrixNew.Add(nont, mHandler.Add parsingMatrixCurrent.[nont] emptyMatrix)

            let nontermPairs = allRules.ComplexTails

            let splitedNontermPairs = nontermLockFreeSplit allRules nonterminals parallelProcesses

            let flags = Array.init parallelProcesses (fun _ -> ref false)
            let values = Array.init parallelProcesses (fun _ -> ref false)
        
            let mbp flg vl nontermPairs_mbp = new MailboxProcessor<Message>(fun inbox ->
                let rec loop n =
                    async {
                            let! message = inbox.Receive();
                            for (nt1, nt2) in nontermPairs_mbp do
                                let matrix1 = parsingMatrixCurrent.[nt1]
                                let matrix2 = parsingMatrixCurrent.[nt2]
                                let resultMatrix = mHandler.Multiply matrix1 matrix2          
                                for (nonTerm, _) in allRules.HeadsByComplexTail (nt1, nt2) do
                                        let nonZ = mHandler.getNonZerosCount parsingMatrixCurrent.[nonTerm]
                                        let updatedMatrix = mHandler.Add parsingMatrixNew.[nonTerm] resultMatrix
                                        parsingMatrixNew.Remove(nonTerm) |> ignore
                                        parsingMatrixNew.Add(nonTerm, updatedMatrix)
                                        if (nonZ <> mHandler.getNonZerosCount parsingMatrixNew.[nonTerm])
                                        then vl := true
                            flg:= true
                            do! loop (n + 1)
                    }
                loop (0))
        
            let mailBoxes = Array.init parallelProcesses (fun i -> mbp flags.[i] values.[i] splitedNontermPairs.[i])

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
                    let updatedMatrix = mHandler.Add parsingMatrixNew.[nont] emptyMatrix
                    parsingMatrixCurrent.Remove(nont) |> ignore
                    parsingMatrixCurrent.Add(nont, updatedMatrix)

                multCount <- multCount + 1            

            (parsingMatrixNew, S, vertexToInt, multCount)
            
            

    let graphParse<'MatrixType, 'InnerType when 'InnerType : comparison> (graph:AbstractAnalysis.Common.SimpleInputGraph<int>)
                  mHandler
                  (loadIL:t<Source.t, Source.t>)
                  tokenToInt 
                  parallelProcesses =

        let (rulesHolder, nonterminals, S) = initRulesFromIL loadIL tokenToInt

        recognizeGraph<'MatrixType, 'InnerType> graph mHandler rulesHolder nonterminals !S parallelProcesses
