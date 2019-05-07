module GraphParsing
    open MatrixKernels
    open Util
    open TPMatrices
    open System
    open System.Collections.Generic
    open YC.Core
    open IL
    open Helpers
    //open Conversions.TransformAux
    //open QuickGraph
    open Microsoft.FSharp.Core.Operators
    open MathNet.Numerics.LinearAlgebra.Double
    open MySparseGraphParsingImpl
    open YC.Parsing.Common.GraphInput

    let initRulesFromIL loadIL tokenToInt =
        let grammar = loadIL.grammar
        let mutable tokensCount = 0
        let S = ref (NonTerminal "")
        let nonterminals = new ResizeArray<NonTerminal>()
        let complexConjRules = new ResizeArray<(NonTerminal * Probability.T) * (NonTerminal * NonTerminal * bool) []>()
        let simpleConjRules = new ResizeArray<(NonTerminal * Probability.T) * int<token>>()
        let epsilonConjRules: NonTerminal [] = [||]
        let probOne = Probability.create 1.0
        
        let rec parseConjuncts e1 e2 (conjunctsArr:ResizeArray<NonTerminal * NonTerminal * bool>) =
            match e1, e2 with
            | PSeq([n1; n2],_,_), PSeq([n3; n4],_,_) ->
                match n1.rule, n2.rule, n3.rule, n4.rule with 
                | PRef (name1, _), PRef (name2, _), PRef (name3, _), PRef (name4, _) ->
                    let nonterm1 = NonTerminal <| sourceToString name1
                    if not <| nonterminals.Contains nonterm1
                    then nonterminals.Add nonterm1
                    let nonterm2 = NonTerminal <| sourceToString name2
                    if not <| nonterminals.Contains nonterm2
                    then nonterminals.Add nonterm2
                    let nonterm3 = NonTerminal <| sourceToString name3
                    if not <| nonterminals.Contains nonterm3
                    then nonterminals.Add nonterm3
                    let nonterm4 = NonTerminal <| sourceToString name4
                    if not <| nonterminals.Contains nonterm4
                    then nonterminals.Add nonterm4
                    conjunctsArr.AddRange([|(nonterm1,nonterm2,true);(nonterm3,nonterm4,true)|])
                | _ -> failwith "Given grammar is not in conjunctive normal form."

            | PSeq([n1; n2],_,_), PConj(conj1, conj2) ->
                match n1.rule, n2.rule with 
                | PRef (name1, _), PRef (name2, _) ->
                    let nonterm1 = NonTerminal <| sourceToString name1
                    if not <| nonterminals.Contains nonterm1
                    then
                        nonterminals.Add nonterm1
                    let nonterm2 = NonTerminal <| sourceToString name2
                    if not <| nonterminals.Contains nonterm2
                    then
                        nonterminals.Add nonterm2
                    conjunctsArr.Add((nonterm1,nonterm2,true))
                    parseConjuncts conj1 conj2 conjunctsArr
                | _ -> failwith "Given grammar is not in conjunctive normal form."

            | _ -> failwith "Given grammar is not in conjunctive normal form."


        for module' in grammar do
            for r in module'.rules do
                let nonterm = NonTerminal <| sourceToString r.name
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
                        let token = sourceToString src
                        let intToken = tokenToInt token
                        simpleConjRules.Add((nonterm, probOne), intToken)
                    | _ ->
                        failwith "Given grammar is not in conjunctive normal form."
                        
                | PSeq([e1; e2],_,_) ->
                    match e1.rule, e2.rule with 
                    | PRef (name1, _), PRef (name2, _) ->
                        let nonterm1 = NonTerminal <| sourceToString name1
                        if not <| nonterminals.Contains nonterm1
                        then
                            nonterminals.Add nonterm1
                        let nonterm2 = NonTerminal <| sourceToString name2
                        if not <| nonterminals.Contains nonterm2
                        then
                            nonterminals.Add nonterm2
                        complexConjRules.Add((nonterm, probOne),[|(nonterm1, nonterm2, true)|])
                    | _ -> failwith "Given grammar is not in conjunctive normal form."

                | PConj(e1, e2) ->
                    let newConjArr = new ResizeArray<NonTerminal * NonTerminal * bool>()
                    parseConjuncts e1 e2 newConjArr
                    complexConjRules.Add((nonterm, probOne), newConjArr.ToArray())

                | _ -> failwith "Given grammar is not in conjunctive normal form."

        let boolRulesHolder = new BooleanRulesHolder(complexConjRules.ToArray(), simpleConjRules.ToArray(), epsilonConjRules)

        (boolRulesHolder, nonterminals, S)

    type Message = bool

    let recognizeGraph<'MatrixType, 'InnerType when 'InnerType : comparison> (graph : SimpleInputGraph<int<token>>)
                    (mHandler : IMatrixHandler<'MatrixType, 'InnerType>)
                    (allRules: BooleanRulesHolder)
                    nonterminals
                    S 
                    parallelProcesses =

        let conjMatrices = new Dictionary<NonTerminal*NonTerminal, 'MatrixType>()
        let allConjuncts = allRules.AllConjuncts

        (*if (parallelProcesses = 1)
        then*)
        let parsingMatrix, vertexToInt = mHandler.ParsingMatrixInitializator graph allRules nonterminals
        //printfn "Matrix initialized"
        let matrixSize = graph.VertexCount //1xsize
        //let matrixSize = 8*graph.VertexCount //8xsize
        let isChanged = ref true
        let mutable multCount = 0

        while !isChanged do
            isChanged := false
            for (nt1, nt2) in allConjuncts do
                let matrix1 = parsingMatrix.[nt1]
                let matrix2 = parsingMatrix.[nt2]
                let resultMatrix = mHandler.Multiply matrix1 matrix2
                conjMatrices.Remove(nt1, nt2) |> ignore
                conjMatrices.Add((nt1, nt2), resultMatrix)
            for (nonTerm,_), conjuncts in allRules.ComplexRules do
                let nonZ = mHandler.getNonZerosCount parsingMatrix.[nonTerm]
                let resultMatrices = conjuncts |> Array.map (fun (n1,n2,_) -> conjMatrices.[n1,n2])
                if resultMatrices.Length = 1
                then
                    let resultConjMatrix = resultMatrices.[0]
                    let updatedMatrix = mHandler.Add parsingMatrix.[nonTerm] resultConjMatrix
                    parsingMatrix.Remove(nonTerm) |> ignore
                    parsingMatrix.Add(nonTerm, updatedMatrix)
                else
                    let resultConjMatrix = resultMatrices |> Array.fold (fun acc elem -> mHandler.Conj acc elem) resultMatrices.[0]
                    let updatedMatrix = mHandler.Add parsingMatrix.[nonTerm] resultConjMatrix
                    parsingMatrix.Remove(nonTerm) |> ignore
                    parsingMatrix.Add(nonTerm, updatedMatrix)
                if (nonZ <> mHandler.getNonZerosCount parsingMatrix.[nonTerm])
                then 
                    isChanged := true
            //printfn "Iteration done"
            multCount <- multCount + 1

        (parsingMatrix, S, vertexToInt, multCount)
        (*else
            let parsingMatrixCurrent, vertexToInt = mHandler.ParsingMatrixInitializator graph allRules nonterminals
            let matrixSize = graph.VertexCount
            let isChanged = ref true
            let mutable multCount = 0
            let parsingMatrixNew = new ParsingMatrix<'MatrixType>()
            let emptyMatrix = mHandler.createEmptyMatrix matrixSize
            for nont in parsingMatrixCurrent.Keys do
                parsingMatrixNew.Add(nont, mHandler.Add parsingMatrixCurrent.[nont] emptyMatrix)

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

            (parsingMatrixNew, S, vertexToInt, multCount)*)
            
            

    let graphParse<'MatrixType, 'InnerType when 'InnerType : comparison> (graph:SimpleInputGraph<int<token>>)
                  mHandler
                  (loadIL:Definition<Source, Source>)
                  tokenToInt 
                  parallelProcesses =

        let (rulesHolder, nonterminals, S) = initRulesFromIL loadIL tokenToInt

        recognizeGraph<'MatrixType, 'InnerType> graph mHandler rulesHolder nonterminals !S parallelProcesses

    
    let graphParseGPU (graph:SimpleInputGraph<int<token>>)
                  (loadIL:Definition<Source, Source>)
                  tokenToInt =

        let (allRules, nonterminals, S) = initRulesFromIL loadIL tokenToInt
        let sparseHandler = (new MySparseHandler(graph.VertexCount)) :> IMatrixHandler<MySparseMatrix, float>
        let parsingMatrix, vertexToInt = sparseHandler.ParsingMatrixInitializator graph allRules nonterminals
        //printfn "Matrix initialized"
        let resultMatrix, multCount = cusparseTransitiveClosure parsingMatrix allRules nonterminals graph.VertexCount //1xsize
        //let resultMatrix, multCount = cusparseTransitiveClosure parsingMatrix allRules nonterminals (8*graph.VertexCount) //8xsize
        (resultMatrix, !S, vertexToInt, multCount)

    let graphParseSemiNaiveGPU (graph:SimpleInputGraph<int<token>>)
                  (loadIL:Definition<Source, Source>)
                  tokenToInt =

        let (allRules, nonterminals, S) = initRulesFromIL loadIL tokenToInt
        let sparseHandler = (new MySparseHandler(graph.VertexCount)) :> IMatrixHandler<MySparseMatrix, float>
        let parsingMatrix, vertexToInt = sparseHandler.ParsingMatrixInitializator graph allRules nonterminals
        //printfn "Matrix initialized"
        let resultMatrix, multCount = cusparseTransitiveClosureSemiNaive parsingMatrix allRules nonterminals graph.VertexCount //1xsize
        //let resultMatrix, multCount = cusparseTransitiveClosure parsingMatrix allRules nonterminals (8*graph.VertexCount) //8xsize
        (resultMatrix, !S, vertexToInt, multCount)

