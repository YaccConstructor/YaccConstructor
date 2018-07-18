module BioDataPerformance

open VDS.RDF
open VDS.RDF.Parsing

open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Frontends.YardFrontend
open YaccConstructor.API
open AbstractParser
open QuickGraph
open System.Collections.Generic
open YC.GLL.SPPF

open System.IO
open System.Collections.Generic

let PrepareGrammarFromFile grammarFile = 
        let fe = new YardFrontend()
        let gen = new GLL()
        generate grammarFile fe gen None Seq.empty [||] [] :?> ParserSourceGLL

let PrintToDotVert (graph: AdjacencyGraph<_,TaggedEdge<_,_>>) name (tagToString: _ -> string) (*(tokenToString : 'token -> string) (numToToken : int -> 'token)*) = 
    use out = new System.IO.StreamWriter (name : string)
    out.WriteLine("digraph AST {")
    out.WriteLine "rankdir=LR"
    for i in graph.Vertices do
        let edges = graph.OutEdges i
        for e in edges do
            let tokenName = e.Tag |> tagToString 
            out.WriteLine (e.Source.ToString() + " -> " + e.Target.ToString() + "[label=\"" + tokenName + "\"]")
    out.WriteLine("}")
    out.Close() 

let SubgraphToGraphVert (subgraph: AdjacencyGraph<_,ParserEdge<_>>) (input: IParserInput) =
    let graphVert = new AdjacencyGraph<_,_>()
    for i in subgraph.Vertices do
        if i % 2 = 0
        then
            let vFrom = input.PositionToString (i * 1<positionInInput>)
            let edges1 = subgraph.OutEdges i
            for e1 in edges1 do
                let edges2 = subgraph.OutEdges e1.Target
                for e2 in edges2 do
                    let vTo = input.PositionToString (e2.Target * 1<positionInInput>) 
                    graphVert.AddVerticesAndEdge (new TaggedEdge<_,_>(vFrom, vTo, e2.Tag)) |> ignore
    graphVert

let PrintToDot (graph: AdjacencyGraph<_,ParserEdge<_>>) name (tagToString: _ -> string) (*(tokenToString : 'token -> string) (numToToken : int -> 'token)*) = 
    use out = new System.IO.StreamWriter (name : string)
    out.WriteLine("digraph AST {")
    out.WriteLine "rankdir=LR"
    for i in graph.Vertices do
        let edges = graph.OutEdges i
        for e in edges do
            let tokenName = e.Tag |> tagToString 
            out.WriteLine (e.Source.ToString() + " -> " + e.Target.ToString() + "[label=\"" + tokenName + "\"]")
    out.WriteLine("}")
    out.Close() 

let fst (f, _, _) = f

let snd (_, s, _) = s

let trd (_, _, t) = t

let SPPFToSubgraph (sppf : SPPF) (ps : ParserSourceGLL) =
    let tagToLabel x = ps.IntToString.Item (x |> int)
    let edges = GetTerminals sppf |> Seq.map(fun x -> new ParserEdge<_>(snd x |> int, trd x |> int, (fst x |> tagToLabel)))
    let subgraph = new AdjacencyGraph<int, ParserEdge<_>>()
    subgraph.AddVerticesAndEdgeRange(edges) |> ignore
    subgraph

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
    | Prefix "HomoloGene_" () -> tokenizer "HOMOLOGENE"
    | Prefix "Phenotype_" () -> tokenizer "PHENOTYPE"
    | Prefix "Pathway_" () -> tokenizer "PATHWAY"
    | Equals "has" () -> tokenizer "HAS" 
    | Equals "-has" () -> tokenizer "RHAS" 
    | Equals "is_homologous_to" () -> tokenizer "HOMOLOGTO"
    | Equals "-is_homologous_to" () -> tokenizer "RHOMOLOGTO"
    | Equals "interacts_with" () -> tokenizer "INTERACTS" 
    | Equals "belongs_to" () -> tokenizer "BELONGS" 
    | Equals "-belongs_to" () -> tokenizer "RBELONGS" 
    | Equals "codes_for" () -> tokenizer "CODESFOR"
    | Equals "-codes_for" () -> tokenizer "RCODESFOR"
    | Equals "refers_to" () -> tokenizer "REFERS"
    | Prefix "GO_" () -> tokenizer "GO"
    | Prefix "FamilyOrDomain_" () -> tokenizer "FAMDOM"
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
    let mutable idV = -2
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
            
        let getFId = getId f
        let getTId = getId t

        match getFId, getTId with
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

let getIntGraph file =
    let edges = getEdges file
    let allVs = edges |> Array.collect (fun (f,l,t) -> [|f * 1<positionInInput>; t * 1<positionInInput>|]) |> Set.ofArray |> Array.ofSeq
    let graph = new SimpleInputGraph<int<token>>(allVs, id)
    edges
    |> Array.collect (fun (f,l,t) -> 
        if getTokenFromTag (fun x -> GLL.BioCFG.stringToToken.[x]) l <> GLL.BioCFG.stringToToken.["OTHER"]
        then [|new ParserEdge<_>(f, t, getTokenFromTag (fun x -> GLL.BioCFG.stringToToken.[x]) l)|]
        else [||])
    |> graph.AddVerticesAndEdgeRange
    |> ignore

    graph, graph.EdgeCount

let getParseInputGraph file (ps : ParserSourceGLL) =
    let edges = getEdges file    
    let allGenes = edges |> Array.choose (fun (f,l,t) -> 
        if getTokenFromTag id l = "GENE"
        then Some(f * 1<positionInInput>)
        else None)

    let genes = allGenes |> Set.ofArray |> Array.ofSeq
    let firstGene = [|allGenes.[0]|]
     
    let edgeTagToInt x = getTokenFromTag (fun t -> t |> ps.StringToToken) x
    let graph = new SimpleInputGraph<_>(firstGene, edgeTagToInt)

    edges
    |> Array.collect (fun (f,l,t) -> [|new ParserEdge<_>(f, t, l)|])
    |> graph.AddVerticesAndEdgeRange
    |> ignore

    graph, graph.EdgeCount

let getParseInputGraphVert file (ps : ParserSourceGLL) =
    let edges = getEdgesVert file
    let allGenes = edges |> Array.choose (fun (f,l,t) -> 
        if getTokenFromTag id f = "GENE"
        then Some(f)
        else None)
    let firstGene = [|allGenes.[0]|]
    let genes = allGenes |> Set.ofArray |> Array.ofSeq 
    let fewGenes = genes 
                    |> Array.indexed 
                    |> Array.choose (fun (i, x) -> 
                            if i % 99 = 0
                            then Some(x)
                            else None)
    let edgeTagToInt x = getTokenFromTag (fun t -> t |> ps.StringToToken |> int) x
    let graph = new GraphLabelledVertex<string>(firstGene, firstGene, edgeTagToInt)

    edges 
    |> Array.collect (fun (f,l,t) -> [|new TaggedEdge<_,_>(f,t,l)|])
    |> graph.AddEdges
    |> ignore

    graph, graph.EdgeCount
        
let processFile inputFile grammarFile =
    let ps = PrepareGrammarFromFile grammarFile
    let g1, edges = 
        getParseInputGraphVert inputFile ps
    printfn "\nNumber of edges: %i" edges

    let start = System.DateTime.Now
    let _,sppf,_ = parse ps g1 true
    let subgraph = SPPFToSubgraph sppf ps
    let time1 = (System.DateTime.Now - start).TotalMilliseconds

//    PrintToDotVert g1 "inputGraph.dot" id
    let subgraphVert = SubgraphToGraphVert subgraph g1 
//    PrintToDotVert subgraphVert "subgraph.dot" id
    printfn "Number of edges in subgraph: %i" subgraphVert.EdgeCount
    printfn "Time: %f\n" time1

let outputDir = __SOURCE_DIRECTORY__ + @"\..\data\BioData\result\"
let dataDir = (__SOURCE_DIRECTORY__ + @"\..\data\BioData\")
let grammarsDir = __SOURCE_DIRECTORY__ + @"\"

let performTests() =
    let BioCFGAllDatabases = grammarsDir + "BioCFG_AllDatabases.yrd"
    
    let files = System.IO.Directory.GetFiles(outputDir)
    for f in files do
        processFile f BioCFGAllDatabases
    
    printfn "finished"