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
         
    let actionV = [|for v in inputFstLexer.InitState do yield v ; for edge in inputFstLexer.Edges (*GetEdges()*) do if edge.Tag.OutSymb <> Eps then yield edge.Source |] |> Set.ofArray
    let tokens = new ResizeArray<_>()

    let FstInverse = new FST<_,_>()   
    for edge in inputFstLexer.Edges do
        new TaggedEdge<_,_>(edge.Target, edge.Source, edge.Tag) |> FstInverse.AddVerticesAndEdge |> ignore

     
    let actionVInv = [|for v in inputFstLexer.FinalState do yield v ; for edge in inputFstLexer.Edges(*GetEdges()*) do if edge.Tag.OutSymb <> Eps then yield edge.Source |] |> Set.ofArray |> Array.ofSeq
    let actionVInvBool = ResizeArray.init (!maxV + 1) (fun _ -> false) 
    
    for v in actionVInv do
        actionVInvBool.[v] <- true       
            
    let tokensInv = new Dictionary<_,_>()
    //let visited = new HashSet<_>()

    let bfs vertex (graphFst: FST<_,_>) =
        let targetAct = new HashSet<_>()
        let edgesToks = new ResizeArray<_>()            
        let queueV = new Queue<_>()
        let visited = new HashSet<_>()
        queueV.Enqueue(vertex)               

        let isStartV v = ResizeArray.exists ((=) v) inputFstLexer.InitState
        while queueV.Count > 0 do
            let topV = queueV.Dequeue()
            if not <| visited.Contains(topV) 
            then
                visited.Add(topV) |> ignore
                for v in graphFst.(*Get*)OutEdges topV do
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
        //visited.Clear()                                   
        let gr = new GraphTokenValue<_>() 
        //gr.AddVerticesAndEdgeRange edgesToks |> ignore
        //new GraphAction<_>(vertex, targetAct, gr)      
        (vertex, targetAct, edgesToks)
                
    for act in actionV do 
        bfs act inputFstLexer |> tokens.Add |> ignore

    let isEps = 
        let mem = Dictionary<_,_>(1000)

        fun v -> 
            if mem.ContainsKey v
            then mem.[v]
            else 
                let res = inputFstLexer.(*Get*)OutEdges(v) |> Seq.exists(fun x -> x.Tag.OutSymb = Eps) 
                mem.Add(v,res)
                res

    let bfsInv vertex (graphFst: FST<_,_>) =  
        let visited = new HashSet<_>()      
        let targetAct = new HashSet<_>()
        let edgesToks = 
            let d = new Dictionary<_,ResizeArray<_>>()
            //for v in graphFst.Vertices do d.Add(v,new ResizeArray<_>())
            d

        let queueV = new Queue<_>()
        queueV.Enqueue(vertex)                       

        while queueV.Count > 0 do
            let topV = queueV.Dequeue()
            if not <| visited.Contains(topV)
            then
                visited.Add(topV) |> ignore
                for v in graphFst.(*Get*)OutEdges(topV) do
                    let addE e =
                        if edgesToks.ContainsKey v.Target
                        then edgesToks.[v.Target].Add e
                        else edgesToks.Add(v.Target,ResizeArray.singleton e)
                    new TokenEdge<_>(v.Target, v.Source, match v.Tag.InSymb with |Smbl y -> y | _ -> failwith "Unexpected!!!" ) |> addE |> ignore
                    if actionVInvBool.[v.Target]
                    then if (isEps v.Target) then queueV.Enqueue(v.Target)
                    else queueV.Enqueue(v.Target)  
        //visited.Clear()
        let gr = new GraphTokenValue<_>() 
        //gr.AddVerticesAndEdgeRange edgesToks |> ignore
        //new GraphAction<_>(vertex, targetAct, gr) 
        vertex, targetAct, edgesToks

    for act in actionVInv do
        bfsInv act FstInverse |> fun (vertex, targetAct, edgesToks) -> tokensInv.Add(vertex, (vertex, targetAct, edgesToks)) |> ignore     //if from vertex exist and act-edge and eps-edge, then continue add edges.

    let inline EqualEdges (edg1:TokenEdge<_>) (edg2:TokenEdge<_>) = 
        (*(edg1.Source = edg2.Source) &&*) (edg1.Target = edg2.Target) && (edg1.Label = edg2.Label) && (edg1.BackRef = edg2.BackRef) //smth else?
    
    let inline CommonEdges str1 (str2:Dictionary<_,_>) = 
        //let s1,s2 = if Array.length str1 > Array.length str2 then str2,str1 else str1,str2
        str1 |> Array.filter (fun (edg:TokenEdge<_>) -> str2.ContainsKey edg.Source && str2.[edg.Source] |> ResizeArray.exists (EqualEdges edg))    

    let idFunction v = 
        let idF = ref 0
        for edge in inputFstLexer.(*Get*)OutEdges(v) do
            if edge.Tag.OutSymb <> Eps
            then idF := match edge.Tag.OutSymb with |Smbl y -> y | _ -> failwith "Unexpected :(" 
        !idF    

    for (vertex, targetAct, edgesToks) in tokens do
        let tsArr = edgesToks.ToArray()
        for ea in targetAct do
            if ea <> inputFstLexer.FinalState.[0]
            then 
                let edgesToken = CommonEdges tsArr (let _,_,e = tokensInv.[ea] in e)
                let grToken = new GraphTokenValue<_>()
                grToken.AddVerticesAndEdgeRange edgesToken |> ignore
                let tok = actions.[idFunction ea] grToken
                new ParserEdge<_>(vertex, ea, tok) |> edgesParserGraph.Add |> ignore

    let final = new ResizeArray<_>()
    for edge in inputFstLexer.Edges(*GetEdges()*) do
        if  edge.Target = inputFstLexer.FinalState.[0]
        then final.Add edge.Source
        
    for v in final do         
        new ParserEdge<_>(v, inputFstLexer.FinalState.[0], Some eofToken) |> edgesParserGraph.Add |> ignore

    let res = new ParserInputGraph<_>(inputFstLexer.InitState.[0], inputFstLexer.FinalState.[0])
    res.AddVerticesAndEdgeRange edgesParserGraph |> ignore  
    res

let Tokenize (fstLexer : FST<_,_>) (actions : array<GraphTokenValue<_> -> _>) (alphabet: HashSet<_>) eofToken (inputGraph : Appr<_>) =    
    //let oldMode = System.Runtime.GCSettings.LatencyMode
    //System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
    let inputFst = inputGraph.ToFST() 
    let inline f x y = x = y
    let inputFstLexer = FST<_,_>.Compos(inputFst, fstLexer, alphabet, f) 
    //System.Runtime.GCSettings.LatencyMode <- oldMode
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
            parserInputGraph.Edges(*GetEdges()*)
            |> Seq.map (fun edge ->
                sprintf "%i -> %i [label=\"%s\"]; \n" edge.Source edge.Target  (toStr edge.Tag)) 
                                      
    System.IO.File.WriteAllText(filePrintPath, s + (String.concat "" strs) + "\n}")
    ()
    