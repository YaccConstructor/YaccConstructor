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

let getParseInputGraph t' tr' sco' scor' other' eof' file =    
    let g = loadFromFile file
    let triples = g.Triples.Count
    let edgs = getEdges g
    let edg f t (l: string) = 
        match l.ToLower() with
        | "#type" -> 
            [|
                new ParserEdge<_>(f, t, t' 0) 
                new ParserEdge<_>(t, f, tr' 0)
            |]

        | "#subclassof" -> 
            [|
                new ParserEdge<_>(f, t, sco' 1) 
                new ParserEdge<_>(t, f, scor' 1)
            |]

        | _ -> //[| new ParserEdge<_>(f, t, other' 0)|]
               [||]
        
    let allVs = edgs |> Array.collect (fun (f,l,t) -> [|f;t|]) |> Set.ofArray |> Array.ofSeq
    let eofV = allVs.Length //Array.max allVs + 1
        
    let g = new ParserInputGraph<_>(allVs, [|eofV|])
    
    
    [|for (f,l,t) in edgs -> edg f t l |]
    |> Array.concat
    |> g.AddVerticesAndEdgeRange
    |> ignore
    allVs |> Array.Parallel.map (fun i -> new ParserEdge<_>(i, eofV, eof' 2))
    |> g.AddVerticesAndEdgeRange
    |> ignore
    
    //g.PrintToDot "input1.dot" (fun s -> (((string s).Split '.' |> Array.rev).[0].Split '+' |> Array.rev).[0])
    g, triples

let  getTestGraph () =
    let qGraph = new ParserInputGraph<_>([|0;2|], [|4|])
    let edg f t l = new ParserEdge<_>(f, t, l) 
    qGraph.AddVerticesAndEdgeRange
        [edg 0 1 (GLL.GPPerf1.T 6)
         edg 1 0 (GLL.GPPerf1.TR 1)
         edg 2 3 (GLL.GPPerf1.T 2)
         edg 3 2 (GLL.GPPerf1.TR 3)
         edg 3 1 (GLL.GPPerf1.T 4)
         edg 1 3 (GLL.GPPerf1.TR 5)
         edg 0 4 (GLL.GPPerf1.RNGLR_EOF 0)
         edg 1 4 (GLL.GPPerf1.RNGLR_EOF 0)
         edg 2 4 (GLL.GPPerf1.RNGLR_EOF 0)
         edg 3 4 (GLL.GPPerf1.RNGLR_EOF 0)
        ] |> ignore
    qGraph

         
let processFile file =
    let cnt = 1
    let g1, triples1 = 
        getParseInputGraph GLL.GPPerf1.T GLL.GPPerf1.TR GLL.GPPerf1.SCO GLL.GPPerf1.SCOR GLL.GPPerf1.OTHER GLL.GPPerf1.RNGLR_EOF file
//    let g2, triples1 = 
//        getParseInputGraph GLL.GPPerf2.T GLL.GPPerf2.TR GLL.GPPerf2.SCO GLL.GPPerf2.SCOR GLL.GPPerf2.OTHER GLL.GPPerf2.RNGLR_EOF file
//        //getTestGraph ()
    //g.PrintToDot "input2.dot" (fun s -> (((string s).Split '.' |> Array.rev).[0].Split '+' |> Array.rev).[0])
    let start = System.DateTime.Now
    for i in 0..cnt-1 do
        let res = GLL.GPPerf1.buildAbstractAst g1
        match res with
        | Success t -> 
            printfn "Success"
            //t.AstToDot GLL.GPPerf1.numToString  GLL.GPPerf1.tokenToNumber GLL.GPPerf1.tokenData "outt.dot"
        | _ -> printfn "res: %A" res
        ()
    let time1 = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let root1 = !Yard.Generators.GLL.AbstractParser.rootCount

//    let start = System.DateTime.Now
//    for i in 0..cnt-1 do
//        let res = GLL.GPPerf2.buildAbstractAst g2
//        match res with
//        | Success t -> 
//            printfn "Success"
//            //t.AstToDot GLL.GPPerf1.numToString  GLL.GPPerf1.tokenToNumber GLL.GPPerf1.tokenData "outt.dot"
//        | _ -> printfn "res: %A" res
//        ()
    let time2 = 0.0//(System.DateTime.Now - start).TotalMilliseconds / (float cnt)
    let root2 = 0 //!Yard.Generators.GLL.AbstractParser.rootCount

    System.IO.Path.GetFileNameWithoutExtension file, triples1, time1, root1, time2, root2

let performTests () =
    let basePath = @"..\..\..\data\RDF\"
    let files = System.IO.Directory.GetFiles basePath    
    files 
    |> Array.map processFile
    |> Array.sortBy (fun (_,_,x,_,_,_) -> x)
    |> Array.iter (printfn "%A")    