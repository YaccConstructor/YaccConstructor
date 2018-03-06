module YC.GraphParsing.Tests.SimpleConjPerformance

open Yard.Core
open Util

open QuickGraph
open AbstractAnalysis.Common
open CYKMatrix
open GraphParsing
open MatrixKernels
open MathNet.Numerics.LinearAlgebra.Double
open ImplementationTests

let SimpleTokenizer str =
    match str with
    | "A" -> 1
    | "B" -> 2
    | "C" -> 3
    | "D" -> 4
    | _ -> -1

let processGraph (verticesCount:int) grammarFile =
    let cnt = 1
    let graph = new SimpleInputGraph<int>(verticesCount, id)
    for v1 in [1 .. verticesCount] do
        for v2 in [1 .. verticesCount] do
            if v1 <> v2 then 
                graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, SimpleTokenizer "A") ) |> ignore
                graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, SimpleTokenizer "B") ) |> ignore
                graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, SimpleTokenizer "C") ) |> ignore
                graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, SimpleTokenizer "D") ) |> ignore
    //printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile

    let root, time, countOfPairs = testSparseGPU cnt graph loadIL SimpleTokenizer 1

    verticesCount, time, countOfPairs

let performTests () =
    [|10; 10; 10; 100; 200; 400; 600; 800; 1000|] 
    |> Array.map (fun n -> processGraph n @"../../../GraphParsing.Test/Conj_abc_cnf.yrd")
    |> Array.sortBy (fun (x,_,_) -> x)
    |> Array.iter (printfn "%A")
