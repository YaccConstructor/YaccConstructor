module YC.FST.AbstractLexing.Interpreter

open QuickGraph.FST.GraphBasedFst
open AbstractAnalysis.Common
open System.Collections.Generic
open Microsoft.FSharp.Collections
open QuickGraph
open QuickGraph.FSA.GraphBasedFsa

//let printSmbString (x:char*Position<_>) = 
//        (fst x).ToString() + "_br: " + (snd x).back_ref + "(" + (snd x).start_offset.ToString() + "," + (snd x).end_offset.ToString() + ")"

[<Struct>]
type GraphAction<'br when 'br:equality> =
    val startAct: int
    val endActs: HashSet<int>
    val graph: FSA<'br>
    new (sa, ea, gr) = {startAct = sa; endActs = ea; graph = gr}   

let Interpret (inputFstLexer: FST<_,_>) (actions: array<FSA<_> -> _>) eofToken tokenToNumber =   
    let maxV = inputFstLexer.Vertices |> Seq.max |> ref
    let edgesParserGraph = new ResizeArray<_>()
         
    let actionV = [|for v in inputFstLexer.InitState do yield v ; for edge in inputFstLexer.Edges do if (snd edge.Tag) <> Eps then yield edge.Source |] |> Set.ofArray
    let tokens = new ResizeArray<_>()

    let FstInverse = new FST<_,_>()   
    for edge in inputFstLexer.Edges do
        new EdgeFST<_,_>(edge.Target, edge.Source, edge.Tag) |>  FstInverse.AddVerticesAndEdge |> ignore

     
    let actionVInv = [|for v in inputFstLexer.FinalState do yield v ; for edge in inputFstLexer.Edges do if (snd edge.Tag) <> Eps then yield edge.Source |] |> Set.ofArray |> Array.ofSeq
    let actionVInvBool = ResizeArray.init (!maxV + 1) (fun _ -> false) 
    
    for v in actionVInv do
        actionVInvBool.[v] <- true       
            
    let tokensInv = new ResizeArray<_>() 
    let visited = new HashSet<_>()

    let bfs vertex (graphFst: FST<_,_>) =
        let targetAct = new HashSet<_>()
        let edgesToks = new ResizeArray<_>()            
        let queueV = new Queue<_>()
        queueV.Enqueue(vertex)               

        let isStartV v = ResizeArray.exists ((=) v) inputFstLexer.InitState
        while queueV.Count > 0 do
            let topV = queueV.Dequeue()
            if not <| visited.Contains(topV) 
            then
                visited.Add(topV) |> ignore
                for v in graphFst.OutEdges(topV) do
                    if (snd v.Tag) = Eps
                    then 
                        if (v.Source <> vertex || (isStartV v.Source))
                        then 
                            new EdgeFSA<_>(v.Source, v.Target, fst v.Tag) |> edgesToks.Add |> ignore
                            if v.Target = vertex then targetAct.Add vertex |> ignore
                            queueV.Enqueue v.Target
                    else 
                        if v.Source = vertex 
                        then 
                            new EdgeFSA<_>(v.Source, v.Target, fst v.Tag) |> edgesToks.Add |> ignore
                            if v.Target = graphFst.FinalState.[0]
                            then targetAct.Add v.Target |> ignore
                            else queueV.Enqueue v.Target
                        else 
                            targetAct.Add v.Source |> ignore   
        visited.Clear()                                   
        let gr = new FSA<_>() 
        gr.AddVerticesAndEdgeRange edgesToks |> ignore
        new GraphAction<_>(vertex, targetAct, gr)      
                
    for act in actionV do 
        bfs act inputFstLexer |> tokens.Add |> ignore

    let bfsInv vertex (graphFst: FST<_,_>) =        
        let targetAct = new HashSet<_>()
        let edgesToks = new ResizeArray<_>()            
        let queueV = new Queue<_>()
        queueV.Enqueue(vertex)               

        let isEps v = inputFstLexer.OutEdges(v) |> Seq.exists(fun x -> (snd x.Tag) = Eps) 

        while queueV.Count > 0 do
            let topV = queueV.Dequeue()
            if not <| visited.Contains(topV)
            then
                visited.Add(topV) |> ignore
                for v in graphFst.OutEdges(topV) do
                    new EdgeFSA<_>(v.Target, v.Source, fst v.Tag) |> edgesToks.Add |> ignore
                    if actionVInvBool.[v.Target]
                    then if (isEps v.Target) then queueV.Enqueue(v.Target)
                    else queueV.Enqueue(v.Target)  
        visited.Clear()                                   
        let gr = new FSA<_>() 
        gr.AddVerticesAndEdgeRange edgesToks |> ignore
        new GraphAction<_>(vertex, targetAct, gr) 

    for act in actionVInv do
        bfsInv act FstInverse |> tokensInv.Add |> ignore     //if from vertex exist and act-edge and eps-edge, then continue add edges.

    let EqualEdges (edg1:EdgeFSA<_>) (edg2:EdgeFSA<_>) = 
       (edg1.Source = edg2.Source) && (edg1.Target = edg2.Target) && (edg1.Tag = edg2.Tag)
    
    let CommonEdges (str1:GraphAction<_>) (str2:GraphAction<_>) = 
        let edges = new ResizeArray<_>()
        for edge1 in str1.graph.Edges do
            for edge2 in str2.graph.Edges do
                if EqualEdges edge1 edge2
                then edges.Add(edge1) 
        edges

    let isEqAc elSearch (elem:GraphAction<_>) = elSearch = elem.startAct
    let FindElAct el = ResizeArray.find (isEqAc el) tokensInv

    let idF = ref 0

    let idFunction v = 
        for edge in inputFstLexer.OutEdges(v) do
            if (snd edge.Tag) <> Eps
            then 
                idF := match (snd edge.Tag) with |Smbl y -> y | _ -> failwith "Unexpected :(" 
        !idF    

    for t in tokens do
        for ea in t.endActs do
            if ea <> inputFstLexer.FinalState.[0]
            then 
                let edgesToken = CommonEdges t (FindElAct ea)            
                let grToken = new FSA<_>()
                grToken.AddVerticesAndEdgeRange edgesToken |> ignore
                grToken.InitState <- ResizeArray.singleton t.startAct
                grToken.FinalState <- ResizeArray.singleton ea
                let tok = actions.[(idFunction ea)] grToken
                new ParserEdge<_>(t.startAct, ea, tok) |> edgesParserGraph.Add |> ignore

    let final = new ResizeArray<_>()
    for edge in inputFstLexer.Edges do
        if  edge.Target = inputFstLexer.FinalState.[0]
        then final.Add edge.Source
        
    for v in final do         
        new ParserEdge<_>(v, inputFstLexer.FinalState.[0], Some eofToken) |> edgesParserGraph.Add |> ignore

    let unwrapOption x = 
        match x with
        | Some a -> a |> tokenToNumber
        | None -> -1

    let res = new SimpleInputGraph<_>(inputFstLexer.InitState.[0], inputFstLexer.FinalState.[0], unwrapOption)
    res.AddVerticesAndEdgeRange edgesParserGraph |> ignore  
    res

let Tokenize (fstLexer : FST<_, _>) (actions : array<FSA<_> -> _>) (alphabet: HashSet<_>) eofToken (inputFst : FST<_, _>) tagToToken =    
    let inputFstLexer = FST<_, _>.Compose(inputFst, fstLexer, alphabet) 
    let epsRes = 
        match inputFstLexer with
        | Success fst -> 
            //fst.PrintToDOT (@"../../../src/AbstractLexer.Interpreter.Tests/Tests/CalcTestLexerCompos.dot", printSmbString)
            let SimpleInputGraph = Interpret fst actions eofToken tagToToken
            Success (EpsClosure.NfaToDfa SimpleInputGraph tagToToken)
        | Error errors -> Error errors
    
    epsRes 
    
