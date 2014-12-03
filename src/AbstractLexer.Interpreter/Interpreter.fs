module YC.FST.AbstractLexing.Interpreter

open YC.FST.GraphBasedFst
open AbstractAnalysis.Common
open System.Collections.Generic
open Microsoft.FSharp.Collections
open QuickGraph
open YC.FST.FstApproximation

//let printSmbString (x:char*Position<_>) = 
//        (fst x).ToString() + "_br: " + (snd x).back_ref + "(" + (snd x).start_offset.ToString() + "," + (snd x).end_offset.ToString() + ")"

type TokenEdge<'br>(s,e,t)=
    inherit TaggedEdge<int, char*Position<'br>>(s,e,t)
        
    member this.BackRef = (snd t).back_ref
    member this.StartPos = (snd t).start_offset
    member this.EndPos = (snd t).end_offset
    member this.Label = fst t

type GraphTokenValue<'br>() =
    inherit AdjacencyGraph<int,TokenEdge<'br>>()

    member this.AddEdgeForsed (e:TokenEdge<_>) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

[<Struct>]
type GraphAction<'br> =
    val startAct: int
    val endActs: HashSet<int>
    val graph: GraphTokenValue<'br>
    new (sa, ea, gr) = {startAct = sa; endActs = ea; graph = gr}   


let Interpret (inputFstLexer: FST<_,_>) (actions: array<GraphTokenValue<_> -> _>) eofToken =   
    let maxV = inputFstLexer.Vertices |> Seq.max |> ref
    let edgesParserGraph = new ResizeArray<_>()
         
    let actionV = [|for v in inputFstLexer.InitState do yield v ; for edge in inputFstLexer.Edges do if edge.Tag.OutSymb <> Eps then yield edge.Source |] |> Set.ofArray
    let tokens = new ResizeArray<_>()

    let FstInverse = new FST<_,_>()   
    for edge in inputFstLexer.Edges do
        new TaggedEdge<_,_>(edge.Target, edge.Source, edge.Tag) |>  FstInverse.AddVerticesAndEdge |> ignore

     
    let actionVInv = [|for v in inputFstLexer.FinalState do yield v ; for edge in inputFstLexer.Edges do if edge.Tag.OutSymb <> Eps then yield edge.Source |] |> Set.ofArray |> Array.ofSeq
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
                    if v.Tag.OutSymb = Eps
                    then 
                        if (v.Source <> vertex || (isStartV v.Source))
                        then 
                            new TokenEdge<_>(v.Source, v.Target, match v.Tag.InSymb with |Smbl y -> y | _ -> failwith "Unexpected!!!" ) |> edgesToks.Add |> ignore
                            if v.Target = vertex then targetAct.Add vertex |> ignore
                            queueV.Enqueue v.Target
                    else 
                        if v.Source = vertex 
                        then 
                            new TokenEdge<_>(v.Source, v.Target, match v.Tag.InSymb with |Smbl y -> y | _ -> failwith "Unexpected!!!" ) |> edgesToks.Add |> ignore
                            if v.Target = graphFst.FinalState.[0]
                            then targetAct.Add v.Target |> ignore
                            else queueV.Enqueue v.Target
                        else 
                            targetAct.Add v.Source |> ignore   
        visited.Clear()                                   
        let gr = new GraphTokenValue<_>() 
        gr.AddVerticesAndEdgeRange edgesToks |> ignore
        new GraphAction<_>(vertex, targetAct, gr)      
                
    for act in actionV do 
        bfs act inputFstLexer |> tokens.Add |> ignore

    let bfsInv vertex (graphFst: FST<_,_>) =        
        let targetAct = new HashSet<_>()
        let edgesToks = new ResizeArray<_>()            
        let queueV = new Queue<_>()
        queueV.Enqueue(vertex)               

        let isEps v = inputFstLexer.OutEdges(v) |> Seq.exists(fun x -> x.Tag.OutSymb = Eps) 

        while queueV.Count > 0 do
            let topV = queueV.Dequeue()
            if not <| visited.Contains(topV)
            then
                visited.Add(topV) |> ignore
                for v in graphFst.OutEdges(topV) do
                    new TokenEdge<_>(v.Target, v.Source, match v.Tag.InSymb with |Smbl y -> y | _ -> failwith "Unexpected!!!" ) |> edgesToks.Add |> ignore
                    if actionVInvBool.[v.Target]
                    then if (isEps v.Target) then queueV.Enqueue(v.Target)
                    else queueV.Enqueue(v.Target)  
        visited.Clear()                                   
        let gr = new GraphTokenValue<_>() 
        gr.AddVerticesAndEdgeRange edgesToks |> ignore
        new GraphAction<_>(vertex, targetAct, gr) 

    for act in actionVInv do
        bfsInv act FstInverse |> tokensInv.Add |> ignore     //if from vertex exist and act-edge and eps-edge, then continue add edges.

    let EqualEdges (edg1:TokenEdge<_>) (edg2:TokenEdge<_>) = 
        (edg1.Source = edg2.Source) && (edg1.Target = edg2.Target) && (edg1.Label = edg2.Label) && (edg1.BackRef = edg2.BackRef) //smth else?
    
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
            if edge.Tag.OutSymb <> Eps
            then 
                idF := match edge.Tag.OutSymb with |Smbl y -> y | _ -> failwith "Unexpected :(" 
        !idF    

    for t in tokens do
        for ea in t.endActs do
            if ea <> inputFstLexer.FinalState.[0]
            then 
                let edgesToken = CommonEdges t (FindElAct ea)            
                let grToken = new GraphTokenValue<_>()
                grToken.AddVerticesAndEdgeRange edgesToken |> ignore
                let tok = actions.[(idFunction ea)] grToken
                new ParserEdge<_>(t.startAct, ea, tok) |> edgesParserGraph.Add |> ignore

    let final = new ResizeArray<_>()
    for edge in inputFstLexer.Edges do
        if  edge.Target = inputFstLexer.FinalState.[0]
        then final.Add edge.Source
        
    for v in final do         
        new ParserEdge<_>(v, inputFstLexer.FinalState.[0], Some eofToken) |> edgesParserGraph.Add |> ignore

    let res = new ParserInputGraph<_>(inputFstLexer.InitState.[0], inputFstLexer.FinalState.[0])
    res.AddVerticesAndEdgeRange edgesParserGraph |> ignore  
    res

let Tokenize (fstLexer : FST<_,_>) (actions : array<GraphTokenValue<_> -> _>) (alphabet: HashSet<_>) eofToken (inputGraph : Appr<_>) =    
    let inputFst = inputGraph.ToFST() 
    let inputFstLexer = FST<_,_>.Compos(inputFst, fstLexer, alphabet) 
    let epsRes = 
        match inputFstLexer with
        | Success fst -> 
            //fst.PrintToDOT (@"../../../src/AbstractLexer.Interpreter.Tests/Tests/CalcTestLexerk.dot", printSmbString)
            let parserInputGraph = Interpret fst actions eofToken
            Success (EpsClosure.NfaToDfa parserInputGraph)
        | Error errors -> Error errors
    
    epsRes 

let ToDot (parserInputGraph : ParserInputGraph<_>) filePrintPath toStr =
    let rank s l =
        "{ rank=" + s + "; " + (l |> string) + " }\n"
    let s = 
        "digraph G {\n" 
        + "rankdir = LR\n"
        + "node [shape = circle]\n"
        + sprintf "%i[style=filled, fillcolor=green]\n" parserInputGraph.InitState 
        + sprintf "%i[shape = doublecircle, style=filled, fillcolor=red]\n" parserInputGraph.FinalState
        + rank "same" parserInputGraph.InitState
        + rank "min" parserInputGraph.InitState  
        + rank "same" parserInputGraph.FinalState 
        + rank "max" parserInputGraph.FinalState
    
    let strs =
            parserInputGraph.Edges
            |> Seq.map (fun edge ->
                sprintf "%i -> %i [label=\"%s\"]; \n" edge.Source edge.Target  (toStr edge.Tag)) 
                                      
    System.IO.File.WriteAllText(filePrintPath, s + (String.concat "" strs) + "\n}")
    ()
    