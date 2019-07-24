module YC.GraphParsing.Tests.FormatPerformance

open System.Collections.Generic
open Util
open YC.Parsing.Common.GraphInput
open ImplementationTests
open MatrixKernels
open MySparseGraphParsingImpl


let parseGPU (graph:YC.Parsing.Common.GraphInput.SimpleInputGraph<int<token>>)
                  allRules
                  nonterminals
                  S
                  tokenToInt =
        let sparseHandler = (new MySparseHandler(graph.VertexCount)) :> IMatrixHandler<MySparseMatrix, float>
        let parsingMatrix, vertexToInt = sparseHandler.ParsingMatrixInitializator graph allRules nonterminals
        //printfn "Matrix initialized"
        let resultMatrix, multCount = cusparseTransitiveClosureSemiNaive parsingMatrix allRules nonterminals graph.VertexCount
        (resultMatrix, !S, vertexToInt, multCount)

let testGPU cnt graph allRules nonterminals S tokenizer =
        let S1 = ref (NonTerminal "")
        let start = System.DateTime.Now
        let root =
            [for i in 0..cnt-1 ->
                let (parsingMatrix, StartNonTerm, _, _) = parseGPU graph allRules nonterminals S tokenizer
                S1 := StartNonTerm
                parsingMatrix]
    
        let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
        let countOfPairs = mySparseAnalyzer root.[0] !S1
        root.[0], time, countOfPairs

let processFile file grammarFile cnt =
    let edges = new ResizeArray<int*int<token>*int>()
    let tokenToInt = new Dictionary<string, int>()
    let lines = System.IO.File.ReadLines(file)

    for line in lines do
        let cur = line.Split [|' '|]
        let f = cur.[0] |> int
        let l = cur.[1]
        if not <| tokenToInt.ContainsKey l
        then
            tokenToInt.Add(l, tokenToInt.Count+1)
        
        let t = cur.[2] |> int
        edges.Add(f,tokenToInt.[l]*1<token>,t)

    let edgs = edges.ToArray()
    let allVs = edgs |> Array.collect (fun (f,l,t) -> [|f * 1<positionInInput>; t * 1<positionInInput>|]) |> Set.ofArray |> Array.ofSeq
    let graph = new SimpleInputGraph<_>(allVs, id)
    
    [|for (f,l,t) in edgs -> [| new ParserEdge<_>(f, t, l)|] |]
    |> Array.concat
    |> graph.AddVerticesAndEdgeRange
    |> ignore

    //printfn("Graph loaded")

    let nonterminals = new ResizeArray<NonTerminal>()
    let crl = new ResizeArray<(NonTerminal*Probability.T)*((NonTerminal*NonTerminal*bool)[])>()
    let srl = new ResizeArray<(NonTerminal*Probability.T)*int<token>>()
    let erl = new ResizeArray<NonTerminal>()
    let grammarlines = System.IO.File.ReadLines(grammarFile)
    let S = ref (NonTerminal "")
    for line in grammarlines do
        let cur = line.Trim().Split [|' '|]
        if cur.Length = 1 then
            erl.Add(NonTerminal cur.[0])
            if not <| nonterminals.Contains(NonTerminal cur.[0]) then
                nonterminals.Add(NonTerminal cur.[0])
        elif cur.Length = 2 then
            if not <| tokenToInt.ContainsKey cur.[1]
            then
                tokenToInt.Add(cur.[1], tokenToInt.Count+1)
            srl.Add((NonTerminal cur.[0], Probability.create 1.0), tokenToInt.[cur.[1]]*1<token>)
            if not <| nonterminals.Contains(NonTerminal cur.[0]) then
                nonterminals.Add(NonTerminal cur.[0])
        else
            crl.Add((NonTerminal cur.[0], Probability.create 1.0),[|(NonTerminal cur.[1], NonTerminal cur.[2], true)|])
            if not <| nonterminals.Contains(NonTerminal cur.[0]) then
                nonterminals.Add(NonTerminal cur.[0])
            if not <| nonterminals.Contains(NonTerminal cur.[1]) then
                nonterminals.Add(NonTerminal cur.[1])
            if not <| nonterminals.Contains(NonTerminal cur.[2]) then
                nonterminals.Add(NonTerminal cur.[2])
    
    S := nonterminals.[0]

    let allRules = new BooleanRulesHolder(crl.ToArray(), srl.ToArray(), erl.ToArray())
    let tokenizer str =
        if tokenToInt.ContainsKey(str)
        then
            tokenToInt.[str] * 1<token>
        else
            -1<token>
    let root, time, countOfPairs = testGPU cnt graph allRules nonterminals S tokenizer

    System.IO.Path.GetFileNameWithoutExtension file, time, countOfPairs

let performTests (args:string[]) = 
    let grammarFile = args.[0]
    let matrixFile = args.[1]
    let cnt = args.[2] |> int
    printfn "%A" <| processFile matrixFile grammarFile cnt

[<EntryPoint>]
let main argv = 
    performTests argv
    0 // return an integer exit code
