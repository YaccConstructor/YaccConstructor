module YC.GLL.Abstarct.Tests.RDFPerformance

open VDS.RDF
open VDS.RDF.Parsing

open QuickGraph
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParserWithoutTree
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

let getEdges (g:Graph) =
    let vMap = new System.Collections.Generic.Dictionary<_,_>()
    let mutable id = -1
    [|   
        for t in g.Triples ->
            let from = t.Object
            let _to = t.Subject
            let lbl = t.Predicate
            let getId v = if vMap.ContainsKey v then vMap.[v] else (id <- id + 1; vMap.Add(v, id); id)
            
            getId from
            ,(lbl :?> UriNode).Uri.Fragment
            , getId _to
    |]

let loadFromFile (file:string) =
    let g = new Graph()
    if (System.IO.Path.GetExtension file).ToLower() = "ttl"
    then        
        let ttlparser = new TurtleParser()
        ttlparser.Load(g, file)
    else
        FileLoader.Load(g, file)       
    g

let getParseInputGraph tokenizer file =    
    let g = loadFromFile file
    let triples = g.Triples.Count
    let edgs = getEdges g
    let edg f t (l: string) = 
        match l.ToLower() with
        | "#type" -> 
            [|
                new TaggedEdge<_,_>(f, t, tokenizer "T") 
                new TaggedEdge<_,_>(t, f, tokenizer "TR")
            |]

        | "#subclassof" -> 
            [|
                new TaggedEdge<_,_>(f, t, tokenizer "SCO") 
                new TaggedEdge<_,_>(t, f, tokenizer "SCOR")
            |]

        | _ -> [| new TaggedEdge<_,_>(f, t, tokenizer "OTHER")|]
        
    let allVs = edgs |> Array.collect (fun (f,l,t) -> [|f * 1<positionInInput>; t * 1<positionInInput>|]) |> Set.ofArray |> Array.ofSeq
    let eofV = allVs.Length
        
    let g = new SimpleGraphInput<_>(allVs, id)
    
    [|for (f,l,t) in edgs -> edg f t l |]
    |> Array.concat
    |> g.AddVerticesAndEdgeRange
    |> ignore
    
    g, triples
        
let processFile file =
    let cnt = 1
    let g1, triples1 = 
        getParseInputGraph (GLL.GPPerf1.stringToNumber >> ((*) 1<token>)) file
//    let g2, triples1 = 
//        getParseInputGraph (GLL.GPPerf2.stringToNumber >> ((*) 1<token>)) file
//        
    let start = System.DateTime.Now
    let root1 =
        [for i in 0..cnt-1 ->
            Yard.Generators.GLL.AbstractParserWithoutTree.getAllRangesForStartState GLL.GPPerf1.parserSource g1
            |> Seq.length]
    
    let time1 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    
    let start = System.DateTime.Now
    let root2 =
        [for i in 0..cnt-1 ->
            Yard.Generators.GLL.AbstractParserWithoutTree.getAllRangesForStartState GLL.GPPerf2.parserSource g1
            [-1]
            |> Seq.length]
    let time2 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)

    System.IO.Path.GetFileNameWithoutExtension file, triples1, time1, root1.[0], time2, root2.[0]

let performTests () =
    let basePath = @"..\..\..\data\RDF"
    let files = System.IO.Directory.GetFiles basePath    
    files 
    |> Array.map processFile
    |> Array.sortBy (fun (_,_,x,_,_,_) -> x)
    |> Array.iter (printfn "%A")    