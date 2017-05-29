module BioDataPerformance

open VDS.RDF
open VDS.RDF.Parsing

open QuickGraph
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

open System.IO
open System.Collections.Generic



let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) 
    then Some() 
    else None

let (|Equals|_|) x y = 
    if x = y 
    then Some() 
    else None

let getTokenFromTag tokenizer (tag:string) = 
    match tag with
    | Prefix "Protein_" () -> tokenizer "PROTEIN"
    | Prefix "Gene_" () -> tokenizer "GENE"
    | Prefix "Phenotype_" () -> tokenizer "PHENOTYPE"
    | Equals "interacts_with" () -> tokenizer "INTERACTS" 
    | Equals "belongs_to" () -> tokenizer "BELONGS" 
    | Equals "-belongs_to" () -> tokenizer "RBELONGS" 
    | Equals "codes_for" () -> tokenizer "CODESFOR"
    | Equals "-codes_for" () -> tokenizer "RCODESFOR"
    | Equals "refers_to" () -> tokenizer "REFERS"
    | Prefix "GO_" () -> tokenizer "GO"
    | Prefix "FamilyOrDomain_" () -> tokenizer "FAM_OR_DOM"
    | Equals "has_FamilyOrDomain" () -> tokenizer "HAS_FAM_OR_DOM"
    | _ -> tokenizer "OTHER"

let getEdgesVert file =
    let lines = File.ReadLines(file)
    [|
    for l in lines ->
        let elems = l.Split('\t')
        elems.[0], elems.[1], elems.[2]
    |]

let getEdges file = 
    let vMap = new System.Collections.Generic.Dictionary<_,_>()
    let mutable idV = -1
    let getId v = 
        if vMap.ContainsKey v 
        then true, vMap.[v] 
        else (idV <- idV + 2; vMap.Add(v, idV); false, idV)
    let mutable count = 0
    let lines = File.ReadLines(file)
    [|
    for l in lines ->
        let elems = l.Split('\t')
        let f = elems.[0]
        let lbl = elems.[1]
        let t = elems.[2]
            
        match (getId f), (getId t)  with
        | (false, fId), (false, tId) -> 
            [|fId, f, fId + 1;
            fId + 1, lbl, tId;
            tId, t, tId + 1;|]
                                            
        | (true, fId), (false, tId) ->
            [|fId + 1, lbl, tId;
            tId, t, tId + 1;|]

        | (false, fId), (true, tId) ->
            [|fId, f, fId + 1;
            fId + 1, lbl, tId;|]

        | (true, fId), (true, tId) -> 
            [|fId + 1, lbl, tId;|]
    |] |> Array.concat


let getParseInputGraph file =
    
    let edges = getEdges file

    let allVs = edges |> Array.collect (fun (f,l,t) -> [|f * 1<positionInInput>; t * 1<positionInInput>|]) |> Set.ofArray |> Array.ofSeq
     
    let graph = new SimpleInputGraph<_>(allVs, getTokenFromTag (fun x -> (int) GLL.BioCFG.stringToToken.[x]))
    
    edges
    |> Array.collect (fun (f,l,t) -> [|new ParserEdge<_>(f, t, l)|])
    |> graph.AddVerticesAndEdgeRange
    |> ignore

    graph, graph.EdgeCount

let getParseInputGraphVert file =
    let edges = getEdgesVert file
    let graph = new GraphLabelledVertex<string>([|"Gene_348"|], [||], getTokenFromTag (fun x -> (int) GLL.BioCFG.stringToToken.[x]))

    edges 
    |> Array.collect (fun (f,l,t) -> [|new TaggedEdge<_,_>(f, t, l)|])
    |> graph.AddEdges
    |> ignore

    graph, graph.EdgeCount
        
let processFile file =
    let g1, edges = 
        getParseInputGraphVert file 

    let start = System.DateTime.Now
    printfn "%A" edges
    let root1 =
        Yard.Generators.GLL.AbstractParser.getAllSPPFRoots GLL.BioCFG.parserSource g1
    root1.[0].AstToDot GLL.BioCFG.intToString "resultV.dot"
//    let root1 =
//        Yard.Generators.GLL.AbstractParser.getAllRangesForStartState GLL.GPPerf1.parserSource g1
//        |> Seq.length

    let time1 = (System.DateTime.Now - start).TotalMilliseconds

    printfn "roots %A" root1.Length
    edges, time1, root1

let performTests() =
    let allTriplesFile = @"..\..\..\data\BioData\result\allTriples.txt"
    let simpleInputFile = @"..\..\..\data\BioData\result\simpleInput.txt"
    processFile simpleInputFile
    |> printfn "%A"
    
    printfn "finished"
    System.Console.ReadKey() |> ignore