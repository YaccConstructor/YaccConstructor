module YC.GraphParsing.Tests.SimpleConjPerformance

open Yard.Core
open Yard.Core.Conversions
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
    | _ -> -1

let denseTest (verticesCount:int) grammarFile =
    let cnt = 1
    let graph = new SimpleInputGraph<int>(verticesCount, (fun x -> x * 1<token>))
    for v1 in [0 .. verticesCount - 1] do
        for v2 in [0 .. verticesCount - 1] do
            if v1 <> v2 then 
                graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, SimpleTokenizer "A") ) |> ignore
                graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, SimpleTokenizer "B") ) |> ignore
                graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, SimpleTokenizer "C") ) |> ignore
    //printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile

    let root, time, countOfPairs = testSparseCPU cnt graph loadIL SimpleTokenizer 1

    verticesCount, time, countOfPairs


let densityTest (verticesCount:int) (edgesCount:int) grammarFile =
    let cnt = 1
    let graph = new SimpleInputGraph<int>(verticesCount, (fun x -> x * 1<token>))

    let rnd = System.Random()

    for i in [1..edgesCount] do
        let v1 = rnd.Next(0, verticesCount)
        let v2 = rnd.Next(0, verticesCount)
        let labelToken = rnd.Next(1, 4)
        graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, labelToken) ) |> ignore

    //printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile

    let root, time, countOfPairs = testSparseCPU cnt graph loadIL SimpleTokenizer 1

    verticesCount, edgesCount, time, countOfPairs

let applyConversion (conversion:Conversion) (loadIL:IL.Definition<IL.Source, IL.Source>) = 
        {
            loadIL
                with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
        }
 

let expandBrackets = new Conversions.ExpandBrackets.ExpandBrackets()
let expandMeta = new Conversions.ExpandMeta.ExpandMeta()
let expandEbnf = new Conversions.ExpandEbnfStrict.ExpandEbnf()
let expandInnerAlt = new Conversions.ExpandInnerAlt.ExpandInnerAlt()
let expandRepeat = new Conversions.ExpandRepet.ExpandExpand()
let expandTopLevelAlt = new Conversions.ExpandTopLevelAlt.ExpandTopLevelAlt()
let conversionBNFconj = new Conversions.CNFandBNF.BNFconj()

let densityBNFTest (verticesCount:int) (edgesCount:int) grammarFile =
    let cnt = 1
    let graph = new SimpleInputGraph<int>(verticesCount, (fun x -> x * 1<token>))

    let rnd = System.Random()

    for i in [1..edgesCount] do
        let v1 = rnd.Next(0, verticesCount)
        let v2 = rnd.Next(0, verticesCount)
        let labelToken = rnd.Next(1, 4)
        graph.AddVerticesAndEdge(new ParserEdge<_>(v1, v2, labelToken) ) |> ignore

    //printfn("Graph loaded")
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile

    let result = loadIL |> applyConversion expandEbnf        
                        |> applyConversion expandMeta        
                        |> applyConversion expandRepeat
                        |> applyConversion expandInnerAlt        
                        |> applyConversion expandBrackets
                        |> applyConversion expandTopLevelAlt
                        |> applyConversion conversionBNFconj

    let root, time, countOfPairs = testSparseCPU cnt graph result SimpleTokenizer 1

    verticesCount, edgesCount, time, countOfPairs

let performTests () =
    (*printfn "DenseTest:"
    [|10; 10; 10; 100; 200; 400; 600; 800; 1000|] 
    |> Array.map (fun n -> denseTest n @"../../../GraphParsing.Test/Conj_abc_bnf.yrd")
    |> Array.sortBy (fun (x,_,_) -> x)
    |> Array.iter (printfn "%A")
    *)
    
    for density in [0.1; 0.25; 0.75; 1.0; 2.0; 10.0] do
        let x = int(System.Math.Round((float(2) * density)))
        let round (x:float) = int <| System.Math.Round x
        printfn "DensityTest: %f" density
        [|10; 10; 10; 100; 200; 400; 600; 800; 1000|] 
        |> Array.map (fun n -> densityTest n (round (density * float (n))) @"../../../GraphParsing.Test/Conj_abc_bnf.yrd")
        |> Array.sortBy (fun (x,_,_,_) -> x)
        |> Array.iter (printfn "%A")
    
    (*for density in [0.005(*; 0.01; 0.05; 0.1*)] do
        let x = int(System.Math.Round((float(2) * density)))
        let round (x:float) = int <| System.Math.Round x
        printfn "DensityTest: %f" density
        [|10; 10; 10; 100; 200; 400; 600(*; 800; 1000*)|] 
        |> Array.map (fun n -> densityBNFTest n (round (density * float (n))) @"../../../GraphParsing.Test/Conj_wcw.yrd")
        |> Array.sortBy (fun (x,_,_,_) -> x)
        |> Array.iter (printfn "%A")
    *)

    