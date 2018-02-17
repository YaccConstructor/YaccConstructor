module YC.GLL.Abstarct.Tests.RDFPerformance

open VDS.RDF
open VDS.RDF.Parsing

open QuickGraph
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

open Yard.Generators.GLL
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta

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
                new ParserEdge<_>(f, t, tokenizer "T") 
                new ParserEdge<_>(t, f, tokenizer "TR")
            |]

        | "#subclassof" -> 
            [|
                new ParserEdge<_>(f, t, tokenizer "SCO") 
                new ParserEdge<_>(t, f, tokenizer "SCOR")
            |]

        | _ -> [| new ParserEdge<_>(f, t, tokenizer "OTHER")|]
        
    let allVs = edgs |> Array.collect (fun (f,l,t) -> [|f * 1<positionInInput>; t * 1<positionInInput>|]) |> Set.ofArray |> Array.ofSeq
    let eofV = allVs.Length
        
    let g = new SimpleInputGraph<_>(allVs, id)
    
    [|for (f,l,t) in edgs -> edg f t l |]
    |> Array.concat
    |> g.AddVerticesAndEdgeRange
    |> ignore
    
    g, triples
        
let processFile file =
    let cnt = 1
    let g1, triples1 = 
        getParseInputGraph (fun x -> GLL.GPPerf1.stringToToken.[x] |> int) file
    let g2, triples1 = 
        getParseInputGraph (fun x -> GLL.GPPerf2.stringToToken.[x] |> int) file
        
    let start = System.DateTime.Now
    let root1 =
        [for i in 0..cnt-1 ->
            Yard.Generators.GLL.AbstractParser.getAllRangesForStartState GLL.GPPerf1.parserSource g1
            |> Set.ofSeq
            |> Seq.length]
    
    let time1 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    
    let start = System.DateTime.Now
    let root2 =
        [for i in 0..cnt-1 ->
            Yard.Generators.GLL.AbstractParser.getAllRangesForStartState GLL.GPPerf2.parserSource g2
            |> Set.ofSeq
            |> Seq.length]
    let time2 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)

    System.IO.Path.GetFileNameWithoutExtension file, triples1, time1, root1.[0], time2, root2.[0]

let performTests basePath =
    //let basePath = @"..\..\..\data\RDF"
    let files = System.IO.Directory.GetFiles basePath
    printfn "filename, triples, Q1 time, Q1 result, Q2 time, Q2 result"
    files 
    |> Array.map processFile
    |> Array.sortBy (fun (_,_,x,_,_,_) -> x)
    |> Array.iter (fun x -> (sprintf "%A" x).Trim [|'('; ')'|] |> printfn "%s")


let doSmth () =
    let fe = new YardFrontend()
    let gen = new GLL()
    let meta = new ExpandMeta()
    let conv = seq{yield new ExpandMeta()}

    let parser = YC.API.generate (@"E:\bio_brackets.yrd")
                        fe gen
                        None
                        conv
                        [|""|]
                        [] :?> ParserSourceGLL
    let tokenizer ch =
        string (if ch = 'T' then 'U' else ch)  |> parser.StringToToken
        
    let data =
        System.IO.File.ReadLines(@"E:\complete_genome\ATCC_35405.txt")
        |> Seq.concat
        |> Seq.map tokenizer
        |> Seq.take 10000
        |> Seq.toArray

    let input = LinearInput([|0<positionInInput>|],data)
    let start = System.DateTime.Now
    let res = Yard.Generators.GLL.AbstractParser.getGSS parser input

    printfn "Full time = %A" (System.DateTime.Now-start)

