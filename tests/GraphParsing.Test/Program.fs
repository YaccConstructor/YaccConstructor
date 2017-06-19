module GraphParsingTests

open System.IO
open QuickGraph
open NUnit.Framework
open YC.GraphParsing.Tests.RDFPerfomance
open YC.GraphParsing.Tests.BioPerfomance
open Util
open System.Collections.Generic
open GraphParsing
open MatrixKernels
open ProbabilityGraphParsingImpl
open SparseGraphParsingImpl
open MySparseGraphParsingImpl
open ImplementationTests
open MathNet.Numerics.LinearAlgebra.Double
open AbstractAnalysis.Common

let graphParsingTestPath = "..\..\..\GraphParsing.Test"

[<TestFixture>]
type ``Graph parsing tests``() =  
    member this._01_SimpleNaiveRecognizerTest () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 2)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 0, 2)) |> ignore

        let A = NonTerminal "A"
        let B = NonTerminal "B"
        let S = NonTerminal "S"
        let nonterminals = new ResizeArray<NonTerminal>([| A; B; S |])
        let rawHeadsToProbs = List.map (fun (nt, prob) -> nt, Probability.create prob)
        let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
        [ (A, B), [ S, 1.0 ]
          (A, A), [ B, 1.0 ] ]
        |> List.map (fun (nts, heads) -> nts, rawHeadsToProbs heads)
        |> Seq.iter crl.Add
        let srl = new Dictionary< int, (NonTerminal * Probability.T) list>()
        [ 2, [ A, 1.0 ] ]
        |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
        |> Seq.iter srl.Add
        let erl: NonTerminal list = []
        let rules = new RulesHolder(crl, srl, erl)
        let (recognizeMatrix, _, vertexToInt, multCount) =
            recognizeGraph<ProbabilityMatrix.T, float> graph (new ProbabilityNaiveHandler(graph.VertexCount)) rules nonterminals S 1
        assert (probabilityAnalyzer recognizeMatrix S = 2)
        assert (recognizeMatrix.[S].InnerValue.[1] > 0.0 && recognizeMatrix.[S].InnerValue.[2] > 0.0)
        printfn "Naive DenseCPU Multiplacation count: %d" multCount
        probabilityMatrixPrint recognizeMatrix.[S]

    member this._02_SimpleNaiveRecognizerTest2 () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 0, 1)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<ProbabilityMatrix.T, float> graph (new ProbabilityNaiveHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (probabilityAnalyzer parsingMatrix S = 3)
        assert (parsingMatrix.[S].InnerValue.[0] > 0.0 && parsingMatrix.[S].InnerValue.[4] > 0.0 && parsingMatrix.[S].InnerValue.[8] > 0.0)
        printfn "Naive DenseCPU Multiplacation count: %d" multCount
        probabilityMatrixPrint parsingMatrix.[S]

    member this._03_SimpleNaiveLoopTest () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 0, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 3, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 3, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(3, 2, 1)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<ProbabilityMatrix.T, float> graph (new ProbabilityNaiveHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (probabilityAnalyzer parsingMatrix S = 8)
        assert (parsingMatrix.[S].InnerValue.[0] > 0.0 && parsingMatrix.[S].InnerValue.[1] > 0.0 && parsingMatrix.[S].InnerValue.[0] > 0.0 && parsingMatrix.[S].InnerValue.[3] > 0.0
                && parsingMatrix.[S].InnerValue.[6] > 0.0 && parsingMatrix.[S].InnerValue.[7] > 0.0 && parsingMatrix.[S].InnerValue.[11] > 0.0 && parsingMatrix.[S].InnerValue.[14] > 0.0)
        printfn "Naive DenseCPU Multiplacation count: %d" multCount
        probabilityMatrixPrint parsingMatrix.[S]

    member this._04_SimpleSparseRecognizerTest () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 0, 1)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<SparseMatrix, float> graph (new SparseHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (sparseAnalyzer parsingMatrix S = 3)
        assert (parsingMatrix.[S].At(0,0) > 0.0 && parsingMatrix.[S].At(1,1) > 0.0 && parsingMatrix.[S].At(1,1) > 0.0)
        printfn "SparseCPU Multiplacation count: %d" multCount
        sparseMatrixPrint parsingMatrix.[S]

    member this._05_SimpleSparseLoopTest () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 0, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 3, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 3, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(3, 2, 1)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<SparseMatrix, float> graph (new SparseHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (sparseAnalyzer parsingMatrix S = 8)
        assert (parsingMatrix.[S].At(0,0) > 0.0 && parsingMatrix.[S].At(0,1) > 0.0 && parsingMatrix.[S].At(0,2) > 0.0 && parsingMatrix.[S].At(0,3)> 0.0
                && parsingMatrix.[S].At(1,2) > 0.0 && parsingMatrix.[S].At(1,3) > 0.0 && parsingMatrix.[S].At(2,3) > 0.0 && parsingMatrix.[S].At(3,2) > 0.0)
        printfn "SparseCPU Multiplacation count: %d" multCount
        sparseMatrixPrint parsingMatrix.[S]

    member this._06_SimpleCudaRecognizerTest () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 0, 1)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<ProbabilityMatrix.T, float> graph (new ProbabilityAleaCudaHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (probabilityAnalyzer parsingMatrix S = 3)
        assert (parsingMatrix.[S].InnerValue.[0] > 0.0 && parsingMatrix.[S].InnerValue.[4] > 0.0 && parsingMatrix.[S].InnerValue.[8] > 0.0)
        printfn "Alea CUDA, DenseGPU Multiplacation count: %d" multCount
        probabilityMatrixPrint parsingMatrix.[S]

    member this._07_SimpleCudaLoopTest () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 0, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 3, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 3, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(3, 2, 1)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "SimpleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<ProbabilityMatrix.T, float> graph (new ProbabilityAleaCudaHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (probabilityAnalyzer parsingMatrix S = 8)
        assert (parsingMatrix.[S].InnerValue.[0] > 0.0 && parsingMatrix.[S].InnerValue.[1] > 0.0 && parsingMatrix.[S].InnerValue.[0] > 0.0 && parsingMatrix.[S].InnerValue.[3] > 0.0
                && parsingMatrix.[S].InnerValue.[6] > 0.0 && parsingMatrix.[S].InnerValue.[7] > 0.0 && parsingMatrix.[S].InnerValue.[11] > 0.0 && parsingMatrix.[S].InnerValue.[14] > 0.0)
        printfn "Alea CUDA, DenseGPU Multiplacation count: %d" multCount
        probabilityMatrixPrint parsingMatrix.[S]

    member this._08_SimpleSparseCudaLoopTest () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 0, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 2)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 2)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 2, 5)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 0, 4)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "GPPerf1_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
            | "SCOR" -> 1
            | "TR" -> 2
            | "OTHER" -> 3
            | "SCO" -> 4
            | "T" -> 5
            | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<MySparseMatrix, float> graph (new MySparseHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (mySparseAnalyzer parsingMatrix S = 3)
        assert (parsingMatrix.[S].CsrRow.[1] = 2 && parsingMatrix.[S].CsrRow.[2] = 3)
        assert (parsingMatrix.[S].CsrColInd.[0] = 0 && parsingMatrix.[S].CsrColInd.[1] = 2 && parsingMatrix.[S].CsrColInd.[2] = 2)
        printfn "ManagedCuda, SparseGPU Multiplacation count: %d" multCount
        MySparsePrint parsingMatrix.[S]

[<EntryPoint>]
let f x =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
    let t = new ``Graph parsing tests``()
//    t._01_SimpleNaiveRecognizerTest ()
//    t._02_SimpleNaiveRecognizerTest2 ()
//    t._03_SimpleNaiveLoopTest ()
//    t._04_SimpleSparseRecognizerTest ()
//    t._05_SimpleSparseLoopTest ()
//    t._06_SimpleCudaRecognizerTest ()
//    t._07_SimpleCudaLoopTest ()
//    t._08_SimpleSparseCudaLoopTest ()
//    YC.GraphParsing.Tests.RDFPerfomance.performTests ()
//    YC.GraphParsing.Tests.BioPerfomance.performTests ()
    0
