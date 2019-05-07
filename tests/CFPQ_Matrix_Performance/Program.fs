module YC.GraphParsing.Tests.FormatPerformance

open Util
open YC.Parsing.Common.GraphInput
open ImplementationTests
open MatrixKernels
open MySparseGraphParsingImpl

let tokenizer str =
    match str with
    | "A" -> 1<token>
    | "B" -> 2<token>
    | "AR" -> 3<token>
    | "SCOR" -> 4<token>
    | "TR" -> 5<token>
    | "OTHER" -> 6<token>
    | "SCO" -> 7<token>
    | "T" -> 8<token>
    | _ -> -1<token>


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

let processFile file grammarFile cnt=
    let edges = new ResizeArray<int*int<token>*int>()
    let lines = System.IO.File.ReadLines(file)

    for line in lines do
        let cur = line.Split [|' '|]
        let f = cur.[0] |> int
        let l = tokenizer cur.[1]
        let t = cur.[2] |> int
        edges.Add(f,l,t)

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
    let erl: NonTerminal [] = [||]
    let grammarlines = System.IO.File.ReadLines(grammarFile)
    let S = ref (NonTerminal "")
    for line in grammarlines do
        let cur = line.Split [|' '|]
        if cur.Length = 2 then
            srl.Add((NonTerminal cur.[0], Probability.create 1.0), tokenizer cur.[1])
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

    let allRules = new BooleanRulesHolder(crl.ToArray(), srl.ToArray(), erl)

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
