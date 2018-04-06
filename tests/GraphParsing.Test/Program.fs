module GraphParsingTests

open System.IO
open QuickGraph
open NUnit.Framework
open YC.GraphParsing.Tests.RDFPerformance
open YC.GraphParsing.Tests.BioPerformance
open YC.GraphParsing.Tests.SimpleConjPerformance
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
open YC.GLL.Abstarct.Tests.RDFPerformance

// paste in all these paths ../../../ instead of ./ if local build
let graphParsingTestPath = @"./GraphParsing.Test"
let baseRDFPath = @"./data/RDF"
let RDFfiles = System.IO.Directory.GetFiles baseRDFPath
let [<Literal>] RDF_GPPERF1_GRAMMAR_FILE = @"./GraphParsing.Test/GPPerf1_cnf.yrd"
let [<Literal>] RDF_GPPERF2_GRAMMAR_FILE = @"./GraphParsing.Test/GPPerf2_cnf.yrd"
let [<Literal>] CONJ_RDF_GPPERF1_GRAMMAR_FILE = @"./GraphParsing.Test/GPPerf1_conj_cnf.yrd"
let [<Literal>] CONJ_RDF_GPPERF2_GRAMMAR_FILE = @"./GraphParsing.Test/GPPerf2_conj_cnf.yrd"

let testFileRDF test file grammarFile = 
    let cnt = 1
    let graph, triples = getParseInputGraph RDFtokenizer file
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile
    test cnt graph loadIL RDFtokenizer 1

let RDFGPPerf1Checker graphFile parsingResults =
    let (_,_,countOfPairs) = parsingResults
    match  System.IO.Path.GetFileNameWithoutExtension graphFile with 
    | "skos" ->
        assert(countOfPairs = 810)
    | "generations" ->
        assert(countOfPairs = 2164)
    | "travel" -> 
        assert(countOfPairs = 2499)
    | "univ-bench" -> 
        assert(countOfPairs = 2540)
    | "atom-primitive" -> 
        assert(countOfPairs = 15454)
    | "biomedical-measure-primitive" -> 
        assert(countOfPairs = 15156)
    | "foaf" -> 
        assert(countOfPairs = 4118)
    | "people-pets" -> 
        assert(countOfPairs = 9472)
    | "funding" -> 
        assert(countOfPairs = 17634)
    | "wine" -> 
        assert(countOfPairs = 66572)
    | "pizza" -> 
        assert(countOfPairs = 56195)   
    | _ -> ignore()

let RDFGPPerf2Checker graphFile parsingResults =
    let (_,_,countOfPairs) = parsingResults
    match  System.IO.Path.GetFileNameWithoutExtension graphFile with 
    | "skos" ->
        assert(countOfPairs = 1)
    | "generations" ->
        assert(countOfPairs = 0)
    | "travel" -> 
        assert(countOfPairs = 63)
    | "univ-bench" -> 
        assert(countOfPairs = 81)
    | "atom-primitive" -> 
        assert(countOfPairs = 122)
    | "biomedical-measure-primitive" -> 
        assert(countOfPairs = 2871)
    | "foaf" -> 
        assert(countOfPairs = 10)
    | "people-pets" -> 
        assert(countOfPairs = 37)
    | "funding" -> 
        assert(countOfPairs = 1158)
    | "wine" -> 
        assert(countOfPairs = 133)
    | "pizza" -> 
        assert(countOfPairs = 1262)   
    | _ -> ignore()

let RDFChecker parsingResults =
    for (graphFile, grammarFile, results) in parsingResults do
        match grammarFile with 
        | RDF_GPPERF1_GRAMMAR_FILE
        | CONJ_RDF_GPPERF1_GRAMMAR_FILE ->
            RDFGPPerf1Checker graphFile results
        | RDF_GPPERF2_GRAMMAR_FILE
        | CONJ_RDF_GPPERF2_GRAMMAR_FILE ->
            RDFGPPerf2Checker graphFile results
        | _ -> ignore()


[<TestFixture>]
type ``Graph parsing tests``() = 
    [<Test>] 
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
        let crl = [| (S, Probability.create 1.0),[|(A, B, true)|];(B, Probability.create 1.0),[|(A, A, true)|]|]
        let srl = [| (A, Probability.create 1.0), 2 |]
        let erl: NonTerminal [] = [||]
        let rules = new BooleanRulesHolder(crl, srl, erl)
        let (recognizeMatrix, _, vertexToInt, multCount) =
            recognizeGraph<ProbabilityMatrix.T, float> graph (new ProbabilityNaiveHandler(graph.VertexCount)) rules nonterminals S 1
        assert (probabilityAnalyzer recognizeMatrix S = 2)
        assert (recognizeMatrix.[S].InnerValue.[1] > 0.0 && recognizeMatrix.[S].InnerValue.[2] > 0.0)
        printfn "Naive DenseCPU Multiplacation count: %d" multCount
        probabilityMatrixPrint recognizeMatrix.[S]

    [<Test>]
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

    [<Test>]
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

    [<Test>]
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

    [<Test>]
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

    [<Ignore("GPU tests are ignored on the build server")>] 
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

    [<Ignore("GPU tests are ignored on the build server")>]  
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

    [<Ignore("GPU tests are ignored on the build server")>] 
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

    [<Test>]
    member this._09_Conj_abc_DirectedChain () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 2)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 3, 3)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "Conj_abc_bnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | "B" -> 2
                | "C" -> 3
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<SparseMatrix, float> graph (new SparseHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (sparseAnalyzer parsingMatrix S = 1)
        assert (parsingMatrix.[S].At(0,3) > 0.0)
        printfn "SparseCPU Multiplacation count: %d" multCount
        sparseMatrixPrint parsingMatrix.[S]

    [<Test>]
    member this._10_Conj_abc_SimpleLoop () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddVertex(4) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 0, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 2)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 3, 2)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 4, 3)) |> ignore
        graph.AddEdge(new ParserEdge<_>(3, 3, 3)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "Conj_abc_bnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | "B" -> 2
                | "C" -> 3
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<SparseMatrix, float> graph (new SparseHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (sparseAnalyzer parsingMatrix S = 2)
        assert (parsingMatrix.[S].At(0,3) > 0.0 && parsingMatrix.[S].At(0,4) > 0.0)
        printfn "SparseCPU Multiplacation count: %d" multCount
        sparseMatrixPrint parsingMatrix.[S]

    [<Test>]
    member this._11_Conj_SimpleAprox () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 3, 2)) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 2, 3)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 3, 4)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "Conj_simple_aprox.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | "B" -> 2
                | "C" -> 3
                | "D" -> 4
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<SparseMatrix, float> graph (new SparseHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (sparseAnalyzer parsingMatrix S = 1)
        assert (parsingMatrix.[S].At(0,3) > 0.0)
        printfn "SparseCPU Multiplacation count: %d" multCount
        sparseMatrixPrint parsingMatrix.[S]

    [<Test>]
    member this._12_APSP_SimpleNaiveRecognizerTest () =
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

        let (parsingMatrix,S,_,multCount) = graphParse<ProbabilityMatrix.T, float> graph (new ProbabilityMinPlusHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (probabilityAnalyzer parsingMatrix S = 3)
        assert (parsingMatrix.[S].InnerValue.[0] = 3.0 && parsingMatrix.[S].InnerValue.[4] = 3.0 && parsingMatrix.[S].InnerValue.[8] = 3.0)
        printfn "Naive APSP Multiplacation count: %d" multCount
        probabilityMatrixPrint parsingMatrix.[S]

    [<Test>]
    member this._13_APSP_SimpleLoopTest () =
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

        let (parsingMatrix,S,_,multCount) = graphParse<ProbabilityMatrix.T, float> graph (new ProbabilityMinPlusHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (probabilityAnalyzer parsingMatrix S = 3)
        assert (parsingMatrix.[S].InnerValue.[0] = 6.0 && parsingMatrix.[S].InnerValue.[2] = 4.0 && parsingMatrix.[S].InnerValue.[5] = 2.0)
        printfn "Naive APSP Multiplacation count: %d" multCount
        probabilityMatrixPrint parsingMatrix.[S]

    [<Test>]
    member this._14_PaperExampleTest () =
        let graph = new AbstractAnalysis.Common.SimpleInputGraph<int>([||], id)
        graph.AddVertex(0) |> ignore
        graph.AddVertex(1) |> ignore
        graph.AddVertex(2) |> ignore
        graph.AddVertex(3) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 1, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(1, 2, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(2, 0, 1)) |> ignore
        graph.AddEdge(new ParserEdge<_>(0, 3, 2)) |> ignore
        graph.AddEdge(new ParserEdge<_>(3, 0, 2)) |> ignore

        let grammarPath = System.IO.Path.Combine(graphParsingTestPath, "PaperExampleGrammar_cnf.yrd")
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let loadIL = fe.ParseGrammar grammarPath
        let tokenizer str =
            match str with
                | "A" -> 1
                | "B" -> 2
                | _ -> -1

        let (parsingMatrix,S,_,multCount) = graphParse<ProbabilityMatrix.T, float> graph (new ProbabilityNaiveHandler(graph.VertexCount)) loadIL tokenizer 1
        assert (probabilityAnalyzer parsingMatrix S = 6)
        assert (parsingMatrix.[S].InnerValue.[0] > 0.0 && parsingMatrix.[S].InnerValue.[3] > 0.0 && parsingMatrix.[S].InnerValue.[4] > 0.0
                && parsingMatrix.[S].InnerValue.[7] > 0.0 && parsingMatrix.[S].InnerValue.[8] > 0.0 && parsingMatrix.[S].InnerValue.[11] > 0.0)
        printfn "Naive DenseCPU Multiplacation count: %d" multCount
        probabilityMatrixPrint parsingMatrix.[S]

    member this._RDF_GPPerf1_DenseCPU () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF1_GRAMMAR_FILE, (testFileRDF testDenseCPU rdffile RDF_GPPERF1_GRAMMAR_FILE)))
        RDFChecker parsingResults

    [<Test>]
    member this._RDF_GPPerf1_SparseCPU () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF1_GRAMMAR_FILE, (testFileRDF testSparseCPU rdffile RDF_GPPERF1_GRAMMAR_FILE)))
        RDFChecker parsingResults
    
    [<Ignore("GPU tests are ignored on the build server")>]   
    member this._RDF_GPPerf1_DenseGPU1 () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF1_GRAMMAR_FILE, (testFileRDF testDenseGPU1 rdffile RDF_GPPERF1_GRAMMAR_FILE)))
        RDFChecker parsingResults
    
    [<Ignore("GPU tests are ignored on the build server")>] 
    member this._RDF_GPPerf1_DenseGPU2 () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF1_GRAMMAR_FILE, (testFileRDF testDenseGPU2 rdffile RDF_GPPERF1_GRAMMAR_FILE)))
        RDFChecker parsingResults
    
    [<Ignore("GPU tests are ignored on the build server")>]  
    member this._RDF_GPPerf1_SparseGPU () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF1_GRAMMAR_FILE, (testFileRDF testSparseGPU rdffile RDF_GPPERF1_GRAMMAR_FILE)))
        RDFChecker parsingResults

    member this._RDF_GPPerf2_DenseCPU () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF2_GRAMMAR_FILE, (testFileRDF testDenseCPU rdffile RDF_GPPERF2_GRAMMAR_FILE)))
        RDFChecker parsingResults

    [<Test>]
    member this._RDF_GPPerf2_SparseCPU () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF2_GRAMMAR_FILE, (testFileRDF testSparseCPU rdffile RDF_GPPERF2_GRAMMAR_FILE)))
        RDFChecker parsingResults
    
    [<Ignore("GPU tests are ignored on the build server")>]  
    member this._RDF_GPPerf2_DenseGPU1 () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF2_GRAMMAR_FILE, (testFileRDF testDenseGPU1 rdffile RDF_GPPERF2_GRAMMAR_FILE)))
        RDFChecker parsingResults

    [<Ignore("GPU tests are ignored on the build server")>]  
    member this._RDF_GPPerf2_DenseGPU2 () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF2_GRAMMAR_FILE, (testFileRDF testDenseGPU2 rdffile RDF_GPPERF2_GRAMMAR_FILE)))
        RDFChecker parsingResults

    [<Ignore("GPU tests are ignored on the build server")>]  
    member this._RDF_GPPerf2_SparseGPU () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, RDF_GPPERF2_GRAMMAR_FILE, (testFileRDF testSparseGPU rdffile RDF_GPPERF2_GRAMMAR_FILE)))
        RDFChecker parsingResults
    
    [<Test>]
    member this._Conj_RDF_GPPerf1_SparseCPU () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, CONJ_RDF_GPPERF1_GRAMMAR_FILE, (testFileRDF testSparseCPU rdffile CONJ_RDF_GPPERF1_GRAMMAR_FILE)))
        RDFChecker parsingResults

    [<Test>]
    member this._Conj_RDF_GPPerf2_SparseCPU () =
        let parsingResults = RDFfiles |> Array.map (fun rdffile -> (rdffile, CONJ_RDF_GPPERF2_GRAMMAR_FILE, (testFileRDF testSparseCPU rdffile CONJ_RDF_GPPERF2_GRAMMAR_FILE)))
        RDFChecker parsingResults



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
//    t._09_Conj_abc_DirectedChain ()
//    t._10_Conj_abc_SimpleLoop ()
//    t._11_Conj_SimpleAprox ()
//    t._12_APSP_SimpleNaiveRecognizerTest ()
//    t._13_APSP_SimpleLoopTest ()
//    t._14_PaperExampleTest ()
//    t._RDF_GPPerf1_DenseCPU ()
//    t._RDF_GPPerf1_SparseCPU ()
//    t._RDF_GPPerf1_DenseGPU1 ()
//    t._RDF_GPPerf1_DenseGPU2 ()
//    t._RDF_GPPerf1_SparseGPU ()
//    t._RDF_GPPerf2_DenseCPU ()
//    t._RDF_GPPerf2_SparseCPU ()
//    t._RDF_GPPerf2_DenseGPU1 ()
//    t._RDF_GPPerf2_DenseGPU2 ()
//    t._RDF_GPPerf2_SparseGPU ()
//    t._Conj_RDF_GPPerf1_SparseCPU ()
//    t._Conj_RDF_GPPerf2_SparseCPU ()
//    YC.GraphParsing.Tests.RDFPerformance.performTests ()
//    YC.GraphParsing.Tests.BioPerformance.performTests ()
//    YC.GraphParsing.Tests.AliasAndTaintPerformance.performTests ()
//    YC.GraphParsing.Tests.SimpleConjPerformance.performTests ()
    0
