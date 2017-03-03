module YC.GraphParsing.Tests.RDFPerfomance

open VDS.RDF
open VDS.RDF.Parsing
open YC.GLL.Abstarct.Tests.RDFPerformance
open Yard.Core
open Util

open QuickGraph
open CYKMatrix
open GraphParsing

let tokenizer str =
    match str with
    | "SCOR" -> 1<AbstractAnalysis.Common.token>
    | "TR" -> 2<AbstractAnalysis.Common.token>
    | "OTHER" -> 3<AbstractAnalysis.Common.token>
    | "SCO" -> 4<AbstractAnalysis.Common.token>
    | "T" -> 5<AbstractAnalysis.Common.token>
    | _ -> -1<AbstractAnalysis.Common.token>

let matrixAnalyzer (matrix:Util.ProbabilityMatrix.T) =
    let mutable counter = 0
    let dataSize = matrix.Size * matrix.Size
    for ind in 0..dataSize - 1 do
        if matrix.InnerValue.[ind] > 0.0
        then
            counter <- counter + 1
    counter

let processFile file grammarFile =
    let cnt = 1
    let g1, triples1 = 
        getParseInputGraph tokenizer file (fun _ -> new AdjacencyGraph<_,_>())

    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let loadIL = fe.ParseGrammar grammarFile
    (*let cnfConv = new Conversions.ToCNF.ToCNF()
    let cnfIL = 
        {
            loadIL
                with grammar = cnfConv.ConvertGrammar (loadIL.grammar, [||])
        }*)
    
    let createEmptyMatrix = ProbabilityMatrix.empty

    let getInnerValue (matrix: ProbabilityMatrix.T) = matrix.InnerValue

    let toArray (matrix: ProbabilityMatrix.T) (isTranspose: bool) = matrix.GetSubArray id isTranspose matrix.WholeMatrix

    let innerSum f1 f2 = f1 + f2

    let innerMult f1 f2 = f1 * f2

    let innerZero = 0.0

    let innerOne = 1.0
    
    let naiveSquareFunction = naiveSquareMatrix<ProbabilityMatrix.T, float> getInnerValue <| toArray <| innerSum <| innerMult <| innerZero <| innerOne

    let start = System.DateTime.Now
    let root1 =
        [for i in 0..cnt-1 ->
            let (parsingMatrix, _, _) = graphParse<ProbabilityMatrix.T, float> <| g1 <| naiveSquareFunction <| loadIL
                                          <| tokenizer <| createEmptyMatrix <| getInnerValue <| innerOne
            parsingMatrix]
    
    let time1 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let countOfPairs = matrixAnalyzer root1.[0]

    System.IO.Path.GetFileNameWithoutExtension file, triples1, time1, countOfPairs

let performTests () =
    let basePath = @"..\..\..\data\RDF"
    let files = System.IO.Directory.GetFiles basePath 
    files 
    |> Array.map (fun rdffile -> processFile rdffile "..\..\..\GraphParsing.Test\GPPerf1_cnf.yrd")
    |> Array.sortBy (fun (_,_,x,_) -> x)
    |> Array.iter (printfn "%A")
