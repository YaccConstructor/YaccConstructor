module GraphParsingTests

open System.IO
open QuickGraph
open NUnit.Framework
open YC.GraphParsing.Tests.RDFPerfomance
open Util
open System.Collections.Generic
open GraphParsing
open MathNet.Numerics.LinearAlgebra.Double

let graphParsingTestPath = "..\..\..\GraphParsing.Test"

//ProbabilityMatrix<float> functions
let createEmptyMatrixProbability = ProbabilityMatrix.empty
let matrixSetValueProbability (matrix: ProbabilityMatrix.T) (i: int) (j: int) (value: float) = matrix.InnerValue.[i*matrix.Size + j] <- value
let toArrayProbability (matrix: ProbabilityMatrix.T) (isTranspose: bool) = matrix.GetSubArray id isTranspose matrix.WholeMatrix
let innerSumFloat f1 f2 = f1 + f2
let innerMultFloat f1 f2 = f1 * f2
let innerZeroFloat = 0.0
let innerOneFloat = 1.0
let naiveSquareFunction = naiveSquareMatrix<ProbabilityMatrix.T, float> matrixSetValueProbability
                             <| toArrayProbability <| innerSumFloat <| innerMultFloat <| innerZeroFloat <| innerOneFloat
let graphParsingPrint (matrix: ProbabilityMatrix.T) =
    let rowLength = matrix.Nrow
    let colLength = matrix.Ncol
    for i in [ 0..rowLength - 1 ] do
        for j in [ 0..colLength - 1 ] do
            let cell = Cell.create i j
            printf "%.1f  " <| Probability.unwrap matrix.[cell]
        printfn ""
    printfn ""

//SparseMatrix<float> functions
let createEmptyMatrixSparse size = SparseMatrix.Create(size, size, 0.0)
let matrixSetValueSparse (matrix: SparseMatrix) (i: int) (j: int) (value: float) = matrix.At(i, j, value)
let sparsePrint (matrix: SparseMatrix) =
    for i in 0..(matrix.RowCount - 1) do
        for j in 0..(matrix.ColumnCount - 1) do
            printf "%.1f  " <| matrix.At(i, j)
        printfn ""
    printfn ""

[<TestFixture>]
type ``Graph parsing tests``() =  
    member this._01_SimpleNaiveRecognizerTest () =
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
        let (recognizeMatrix, vertexToInt, multCount) =
            GraphParsing.recognizeGraph<ProbabilityMatrix.T, float> <| graph <| naiveSquareFunction <| rules <| nonterminals <| S <| createEmptyMatrixProbability <| 
                matrixSetValueProbability <| innerOneFloat
        printfn "Multiplacation count: %d" multCount
        graphParsingPrint recognizeMatrix

    member this._02_SimpleNaiveRecognizerTest2 () =
        let graph = new AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>()
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(0, 1, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(1, 2, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(2, 0, 1<AbstractAnalysis.Common.token>)) |> ignore
        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1<AbstractAnalysis.Common.token>
                | _ -> -1<AbstractAnalysis.Common.token>

        let (parsingMatrix, _, multCount) = graphParse<ProbabilityMatrix.T, float> <| graph <| naiveSquareFunction <| loadIL
                                          <| tokenizer <| createEmptyMatrixProbability <| matrixSetValueProbability <| innerOneFloat
        printfn "Multiplacation count: %d" multCount
        graphParsingPrint parsingMatrix

    member this._03_SimpleNaiveLoopTest () =
        let graph = new AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>()
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(0, 0, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(0, 1, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(1, 2, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(1, 3, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(2, 3, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(3, 2, 1<AbstractAnalysis.Common.token>)) |> ignore
        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1<AbstractAnalysis.Common.token>
                | _ -> -1<AbstractAnalysis.Common.token>

        let (parsingMatrix, _, multCount) = graphParse<ProbabilityMatrix.T, float> <| graph <| naiveSquareFunction <| loadIL
                                          <| tokenizer <| createEmptyMatrixProbability <| matrixSetValueProbability <| innerOneFloat
        printfn "Multiplacation count: %d" multCount
        graphParsingPrint parsingMatrix

    member this._04_SimpleSparseRecognizerTest () =
        let graph = new AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>()
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(0, 1, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(1, 2, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(2, 0, 1<AbstractAnalysis.Common.token>)) |> ignore
        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1<AbstractAnalysis.Common.token>
                | _ -> -1<AbstractAnalysis.Common.token>

        let (parsingMatrix, _, multCount) = graphParse<SparseMatrix, float> <| graph <| sparseSquareMatrix <| loadIL
                                          <| tokenizer <| createEmptyMatrixSparse <| matrixSetValueSparse <| innerOneFloat
        printfn "Multiplacation count: %d" multCount
        sparsePrint parsingMatrix

    member this._05_SimpleSparseLoopTest () =
        let graph = new AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>()
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(0, 0, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(0, 1, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(1, 2, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(1, 3, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(2, 3, 1<AbstractAnalysis.Common.token>)) |> ignore
        graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(3, 2, 1<AbstractAnalysis.Common.token>)) |> ignore
        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1<AbstractAnalysis.Common.token>
                | _ -> -1<AbstractAnalysis.Common.token>

        let (parsingMatrix, _, multCount) = graphParse<SparseMatrix, float> <| graph <| sparseSquareMatrix <| loadIL
                                          <| tokenizer <| createEmptyMatrixSparse <| matrixSetValueSparse <| innerOneFloat
        printfn "Multiplacation count: %d" multCount
        sparsePrint parsingMatrix

[<EntryPoint>]
let f x =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
    let t = new ``Graph parsing tests``()
//    t._01_SimpleNaiveRecognizerTest ()
//    t._02_SimpleNaiveRecognizerTest2 ()
//    t._03_SimpleNaiveLoopTest ()
//    t._04_SimpleSparseRecognizerTest ()
//    t._05_SimpleSparseLoopTest ()
//    YC.GraphParsing.Tests.RDFPerfomance.performTests ()
    0
