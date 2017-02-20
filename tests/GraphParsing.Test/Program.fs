module GraphParsingTests

open System.IO
open QuickGraph
open NUnit.Framework
open YC.GraphParsing.Tests.RDFPerfomance
open Util
open System.Collections.Generic

let graphParsingPrint (matrix: ProbabilityMatrix.T) =
    let rowLength = matrix.Nrow
    let colLength = matrix.Ncol
    for i in [ 0..rowLength - 1 ] do
        for j in [ 0..colLength - 1 ] do
            let cell = Cell.create i j
            printf "%.8f  " <| Probability.unwrap matrix.[cell]
        printfn ""
    printfn ""

[<TestFixture>]
type ``Graph parsing tests``() =  
    member this._01_SimpleRecognizerTest () =
        let graph = new AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>()
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore

        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(0, 1, 2*1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(1, 0, 2*1<AbstractAnalysis.Common.token>)) |> ignore
        let A = NonTerminal "A"
        let B = NonTerminal "B"
        let S = NonTerminal "S"
        let nonterminals = [| A; B; S |]

        let rawHeadsToProbs = List.map (fun (nt, prob) -> nt, Probability.create prob)

        let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
        [ (A, B), [ S, 1.0 ]
          (A, A), [ B, 1.0 ] ]
        |> List.map (fun (nts, heads) -> nts, rawHeadsToProbs heads)
        |> Seq.iter crl.Add

        let srl = new Dictionary< int<AbstractAnalysis.Common.token>, (NonTerminal * Probability.T) list>()
        [ 2*1<AbstractAnalysis.Common.token>, [ A, 1.0 ] ]
        |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
        |> Seq.iter srl.Add

        let erl: NonTerminal list = []

        let rules = new RulesHolder(crl, srl, erl)

        let (recognizeMatrix, vertexToInt, multCount) = GraphParsing.recognizeGraph graph GraphParsing.naiveSquareMatrix rules nonterminals S
    
        printfn "Multiplacation count: %d" multCount
        graphParsingPrint recognizeMatrix

[<EntryPoint>]
let f x =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
    let t = new ``Graph parsing tests``()
//    t._01_SimpleRecognizerTest ()
    YC.GraphParsing.Tests.RDFPerfomance.performTests ()
    0
