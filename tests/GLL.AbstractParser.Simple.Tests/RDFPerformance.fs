module YC.GLL.Abstarct.Tests.RDFPerformance

open VDS.RDF
open VDS.RDF.Parsing

open QuickGraph
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

let getEdges (g:Graph) =
    let vMap = new System.Collections.Generic.Dictionary<_,_>()
    let mutable id = -1
    [|   
        for t in g.Triples ->
            let from = t.Object //:?> UriNode
            let _to = t.Subject //:?> UriNode
            let lbl = t.Predicate //:?> UriNode
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

let getParseInputGraph file =    
    let g = loadFromFile file
    let edgs = getEdges g
    let edg f t (l: string) = 
        match l.ToLower() with
        | "#type" -> 
            [
                new ParserEdge<_>(f, t, GLL.GPPerf1.T 0) 
                new ParserEdge<_>(t, f, GLL.GPPerf1.TR 0)
            ]

        | "#subclassof" -> 
            [
                new ParserEdge<_>(f, t, GLL.GPPerf1.SCO 1) 
                new ParserEdge<_>(t, f, GLL.GPPerf1.SCOR 1)
            ]

        | _ -> [ new ParserEdge<_>(f, t, GLL.GPPerf1.OTHER 0)]
        
    let allVs = edgs |> Array.collect (fun (f,l,t) -> [|f;t|]) |> Set.ofArray |> Array.ofSeq
    let eofV = Array.max allVs + 1
    let edges = 
        [for (f,l,t) in edgs do yield! edg f t l]
        @ [for i in allVs -> new ParserEdge<_>(i, eofV, GLL.GPPerf1.RNGLR_EOF 2)]
    let g = new ParserInputGraph<_>(allVs, [|eofV|])
    g.AddVerticesAndEdgeRange edges |> ignore
    g
     
let parse file =
    let cnt = 3
    let g = getParseInputGraph file
    let start = System.DateTime.Now
    for i in 0..cnt-1 do
        let res = GLL.GPPerf1.buildAbstractAst g
        match res with
        | Success t -> 
            printfn "Success with: %A" t.CountCounters
            t.AstToDot GLL.GPPerf1.numToString  GLL.GPPerf1.tokenToNumber GLL.GPPerf1.tokenData "outt.dot"
        | _ -> printfn "res: %A" res
        ()
    let time = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    printfn "time: %A" time