module Yard.Generators.OldGLL.AbstractParserWithoutTree

open System 
open Microsoft.FSharp.Collections
open JetBrains.dotMemoryUnit
open FSharpx.Collections.Experimental

open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.OldGLL.ParserCommon
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns

type SysDict<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>
type CompressedArray<'t> = Yard.Generators.GLL.ParserCommon.CompressedArray<'t>

[<Struct>]
type M =
    val pos : int64
    val lbl : int<positionInGrammar>
    new (p,l) = {pos = p; lbl = l}
        
[<Struct>]
type ResultStruct =
    val le : int
    val lpos : int
    val re : int
    val rpos : int
    val nterm  : string
    val length : uint16
    new (l,l1, r, r1, n, len) = {le = l; lpos = l1; re = r; rpos = r1; nterm = n; length = len}


let buildAbstract<'token> (parser : ParserSourceGLL<'token>) (input : IParserInput) condNonTerm = 
    //let shift = input.Shift
//    if input.EdgeCount = 0 then
//      Error ("This grammar does not accept empty input.")     
    let input = 
        (input :?> YC.Bio.GraphLoader.EdgeCompressedGraphInput).Edges
        |> Seq.map(fun edge -> BioParserEdge(int edge.Source, int edge.Target, edge.Tag.length, edge.Tag.str))
        |> Array.ofSeq
        |> (fun x -> BioParserInputGraph(x))

    let outEdges = 
        let r = Array.init<List<int>> input.VertexCount (fun _ -> List<int>.Empty)
        for i in 0..input.EdgeCount - 1 do
            r.[input.Edges.[i].Start] <- i :: r.[input.Edges.[i].Start]
        r
    //let parser = parser
    let edgeCount = ref 0
    let vertexCount = ref 0
    let sppfNodes = ref 0
    let descr = ref 0

    let reused = ref 0
    let slots = parser.Slots
    let condNonTermRules = Seq.toArray <| seq{for i in 0..parser.LeftSide.Length - 1 do if parser.LeftSide.[i] = condNonTerm then yield i}
    let setU = new CompressedArray<SysDict<int, SysDict<int64, _(*ResizeArray<int64<extension>>*)>>>(input.ChainLength, (fun _ -> null )) 
    let setP = new SysDict<int64, Yard.Generators.Common.DataStructures.ResizableUsualOne<int64<extension>*uint16>>(500)
    let currentRule = parser.StartRule
    let currentLabel = ref <| (CommonFuns.packLabelNew currentRule 0)
    let tempCount = ref 0
    let currentLength = ref 0us
    let r = new System.Collections.Generic.HashSet<_>()
    let currentIndex = ref 0<positionInInput>
    let currentExtension = ref 0L<extension>
    let dummyGSSNode = new Vertex(!currentIndex, -1<positionInGrammar>)
    
    let edges = Array.init parser.NonTermCount (fun _ -> new CompressedArray<SysDict<int<positionInGrammar>, SysDict<int64<extension>, SysDict<int<positionInGrammar>, (int<positionInInput> * uint16)[](*ResizeArray<int*uint16>*)>>>> (input.ChainLength, (fun _ -> null)))
    let vertices = new CompressedArray<ResizeArray<_>>(input.ChainLength, (fun _ -> null))
    let currentGSSNode = ref <| dummyGSSNode
     
    let startContexts = 
        input.InitialVertices
        |> Array.rev
        |> Array.Parallel.mapi(fun i e -> 
            let t = getEdge e
            let tt = getPosOnEdge e
            let ttt = packEdgePos t tt
            let ext = packExtension ttt ttt
            new Context(e, !currentLabel, !currentGSSNode, ext, !currentLength))

    let setR = new System.Collections.Generic.Stack<Context>(startContexts)  

    let currentContext = ref <| new Context(!currentIndex, !currentLabel, !currentGSSNode, !currentExtension, !currentLength)
        
    let slotIsEnd (label : int<positionInGrammar>) =
        (getPositionNew label) = Array.length (parser.rules.[getRuleNew label])
         
    let containsContext  inputIndex (label : int<positionInGrammar>) (vertex : Vertex) extension =
        let vertexKey = CommonFuns.pack vertex.Level vertex.NontermLabel
        if setU.[inputIndex] <> null
        then
            let cond, current = setU.[inputIndex].TryGetValue(int label) 
            if  cond
            then
                if current.ContainsKey vertexKey
                then
                    let trees = current.[vertexKey]
                    if not <| Array.contains extension !trees
                    then 
                        Array.Resize(trees, (!trees).Length + 1)
                        (!trees).[(!trees).Length - 1] <- extension
                        //trees := Array.reduce` extension
                        false
                    else
                        true
                else 
//                        let arr = new ResizeArray<int64<extension>>()
//                        arr.Add extension
                    current.Add(vertexKey, ref [|extension|])                    
                    false
            else 
                let dict = new SysDict<_,_>()
                setU.[inputIndex].Add(int label, dict)
                //let arr = new ResizeArray<_>()
                //arr.Add extension
                dict.Add(vertexKey, ref [|extension|]) 
                false
        else 
            let dict1 = new SysDict<_, _>()
            setU.[inputIndex] <- dict1
            let dict2 = new SysDict<_, _>()
            dict1.Add(int label, dict2)
//                let arr = new ResizeArray<_>()
//                arr.Add extension
            dict2.Add(vertexKey, ref [|extension|])
            false
            
    let addContext (inputVertex : int<positionInInput>) (label : int<positionInGrammar>) vertex extension len =
        if not <| containsContext inputVertex label vertex extension 
        then
            setR.Push(new Context(inputVertex, label, vertex, extension, len))
            incr descr
        else
            incr reused

    let containsEdge (b : Vertex) (e : Vertex) label extension (len : uint16) =
        let rt = getRuleNew label
        let pt = getPositionNew label
        let nontermName = b.NontermLabel
        let beginLevel = b.Level
        let endLevel = int e.Level
        let outgoingEdges = edges.[int nontermName].[beginLevel]
        let cond, dict = 
            if outgoingEdges <> null
            then
                let c1, dictExtensionKey = outgoingEdges.TryGetValue label
                if c1
                then
                    let c2, dictNontermKey = dictExtensionKey.TryGetValue extension
                    if c2
                    then
                        let c3, posLen = dictNontermKey.TryGetValue e.NontermLabel
                        if c3
                        then
                            let mutable contains = false
                            for pair in posLen do if (fst pair) = e.Level && (snd pair) = len then contains <- true
                            if contains then true, None
                            else
                                let a = ref posLen
                                Array.Resize(a, posLen.Length + 1)
                                (!a).[posLen.Length] <- (e.Level,len)
                                dictNontermKey.[e.NontermLabel] <- (!a)
                                false, None
                        else
                            //let arr = new ResizeArray<_>()
                            //arr.Add((e.Level,len))
                            let arr = [|e.Level,len|]
                            dictNontermKey.Add(e.NontermLabel, arr)
                            false, None
                    else
//                            let ra = new ResizeArray<_>()
//                            ra.Add((e.Level,len))
                        let arr = [|e.Level,len|]
                        let d = new SysDict<_, _>()
                        d.Add(e.NontermLabel, arr)
                        dictExtensionKey.Add(extension, d)
                        false, None    
                else
                    let d1 = new SysDict<int64<extension>, SysDict<_, _>>()
                    let d2 = new SysDict<_, _>()
//                        let ra = new ResizeArray<_>()
//                        ra.Add((e.Level,len))
                    let arr = [|e.Level,len|]
                    d2.Add(e.NontermLabel, arr)
                    d1.Add(extension, d2)
                    outgoingEdges.Add(label, d1)
                    false, None
            else
                incr vertexCount 
                let d0 = new SysDict<_, SysDict<int64<extension>, SysDict<_, _>>>()
                let d1 = new SysDict<int64<extension>, SysDict<_, _>>()
                let d2 = new SysDict<_, _>()
//                    let ra = new ResizeArray<_>()
//                    ra.Add((e.Level,len))
                let arr = [|e.Level,len|]
                d2.Add(e.NontermLabel, arr)
                d1.Add(extension, d2)
                d0.Add(label, d1)
                false, Some d0
        if dict.IsSome then edges.[int nontermName].[beginLevel] <- dict.Value
        if not cond then incr edgeCount
        cond
        
    let create (cE : int) (cP : int) (label : int<positionInGrammar>) (vertex : Vertex) curSymbol (len : uint16) =   
        let nonTermName = curSymbol
        let i = packEdgePos cE cP
        ///let ttttt = getPosOnEdge <| getLeftExtension !currentExtension
        let extension = packExtension (getLeftExtension !currentExtension) i 
        let v = new Vertex(i, nonTermName)
        if edges.[int nonTermName].[i] <> null
        then
            let vertexKey = pack i nonTermName
            let temp = containsEdge v vertex label extension len
            if not <| temp 
            then
                if setP.ContainsKey(vertexKey)
                then
                    let arr = setP.[vertexKey]
                          
                    arr.DoForAll (fun ext   ->
                        let ex = fst ext
                        let l = snd ext 
                        let e = packExtension (getLeftExtension extension) (getRightExtension ex)
                        let iiii = packEdgePos (getEdge (1<positionInInput> * getRightExtension ex)) ((getPosOnEdge <| 1<positionInInput> * getRightExtension ex))
                        addContext iiii label vertex e (len + l))
        else
            ignore <| containsEdge v vertex label extension len
            let chainLen = input.ChainLength.[cE]
            let addCntxtForNonTerm e pos index = 
                let curToken = input.Edges.[e].Tokens.[pos]
                let key = int(( int curSymbol  <<< 16) ||| (int curToken - parser.NonTermCount))
                let flg,rules = parser.Table.TryGetValue key
                if flg then
                    for rule in rules do
                        let newLabel = packLabelNew rule 0
                        addContext !currentIndex newLabel v (packExtension index index) 0us  
            if cP < chainLen - 1
            then
                addCntxtForNonTerm cE cP i   
            else
                for oE in outEdges.[input.Edges.[cE].End] do
                    addCntxtForNonTerm oE 0 (packEdgePos oE 0)      
            
                
    let pop (u : Vertex) (i : int<positionInInput>) (extension : int64<extension>) len =
        if u <> dummyGSSNode
        then
                
            let vertexKey = pack u.Level (int u.NontermLabel)
            if setP.ContainsKey vertexKey
            then
                setP.[vertexKey].Add (extension, len)
            else
                let newList = new ResizableUsualOne<_>((extension, len))
                setP.Add(vertexKey, newList)
            let outEdges = edges.[int u.NontermLabel].[u.Level]
            for eLabel in outEdges do
                let labelOnEdge = eLabel.Key  
                for edge in eLabel.Value do
                    let extOnEdge = edge.Key
                    for slotLevels in edge.Value do
                            let slot = slotLevels.Key
                            for level in slotLevels.Value do
                                let l = (getLeftExtension extOnEdge)
                                //let tt = getPosOnEdge l
                                    
                                let r = (getRightExtension extension)
                                //let ttt = getPosOnEdge r
                                let ext = packExtension l r
                                let newVertex = new Vertex(fst level, slot)
                                addContext i (labelOnEdge) newVertex ext (len + snd level)

    let table = parser.Table
    let condition = ref true 
    let stop = ref false
    let rec dispatcher () =
        let rec get () =
            let c = setR.Pop()
            c
        if setR.Count <> 0
        then
            currentContext :=  get () 
            currentIndex := currentContext.Value.Index
            let t = getEdge !currentIndex
            let tt = getPosOnEdge !currentIndex
            currentGSSNode := currentContext.Value.Vertex
            currentLabel := currentContext.Value.Label
            currentExtension := currentContext.Value.Extension
            currentLength := currentContext.Value.SLength
            //let ttt = getPosOnEdge <| getLeftExtension !currentExtension
            //let aa = getPosOnEdge <| getRightExtension !currentExtension
            condition := false
        else 
            stop := true  
                              
    and processing () =  
        condition := true
        let rule = getRuleNew !currentLabel
        let position = getPositionNew !currentLabel
        let eatTerm () =
            let pos = (1 + getPosOnEdge !currentIndex)
            currentIndex := packEdgePos (getEdge !currentIndex) pos
            currentLength := !currentLength + 1us
            //let ttt = getPosOnEdge (getLeftExtension !currentExtension)
            let le = (getLeftExtension !currentExtension)
            let re = packEdgePos (getEdge !currentIndex) ((getPosOnEdge !currentIndex))
            currentExtension := packExtension le re
            currentLabel := packLabelNew rule (position + 1)
            condition := false
        if Array.length parser.rules.[rule] = 0 
        then
            if Array.exists (fun e -> e = rule) condNonTermRules
            then
                r.Add(new ResultStruct((getEdge <| 1<positionInInput> * getLeftExtension !currentExtension), (getPosOnEdge <| 1<positionInInput> * getLeftExtension !currentExtension), (getEdge <| 1<positionInInput> * getRightExtension !currentExtension), (getPosOnEdge <| 1<positionInInput> * getRightExtension !currentExtension), parser.NumToString <| parser.LeftSide.[rule], !currentLength))
                |> ignore
                
            pop !currentGSSNode !currentIndex !currentExtension !currentLength
        else
            //setR.Count                
            if Array.length parser.rules.[rule] <> position
            then
                let curSymbol = parser.rules.[rule].[position]
                let cE = CommonFuns.getEdge !currentIndex
                let cP = CommonFuns.getPosOnEdge !currentIndex
                let chainLen = input.ChainLength.[cE]
                if parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol
                then                                                
                    if cP < chainLen - 1 
                    then    
                        let curToken = input.Edges.[cE].Tokens.[cP] 
                        if int curToken = curSymbol
                        then eatTerm ()
                    else   
                        let curEdge = 
                            let oEdges = outEdges.[input.Edges.[cE].End]
                            let mutable res = None
                            for oe in oEdges do
                                if curSymbol = int input.Edges.[oe].Tokens.[0] then
                                    res <- Some oe  
                            res
                                
                        match curEdge with
                        | Some edge ->
                            currentIndex := (packEdgePos curEdge.Value 0)
                            eatTerm ()
                        | None _ -> ()
                else               
                    if cP < chainLen - 1  then         
                        let curToken = input.Edges.[cE].Tokens.[cP]
                          
                        if curSymbol = condNonTerm then currentLength := 0us
                        let key = int(( curSymbol  <<< 16) ||| (int curToken - parser.NonTermCount))
                        if parser.Table.ContainsKey key
                        then
                            create cE cP (packLabelNew rule (position + 1)) !currentGSSNode (1<positionInGrammar> * curSymbol) !currentLength
                    else
                        let oEdges = outEdges.[input.Edges.[cE].End]
                        for oe in oEdges do 
                            let curToken = input.Edges.[oe].Tokens.[0]
                            if curSymbol = condNonTerm then currentLength := 0us
                            let key = int(( curSymbol  <<< 16) ||| (int curToken - parser.NonTermCount))
                            if parser.Table.ContainsKey key
                            then
                                create oe 0 (packLabelNew rule (position + 1)) !currentGSSNode (1<positionInGrammar> * curSymbol) !currentLength
                            
                                                        
                    
            else
                if Array.exists (fun e -> e = rule) condNonTermRules
                then
                    r.Add(new ResultStruct((getEdge <| 1<positionInInput> * getLeftExtension !currentExtension), (getPosOnEdge <| 1<positionInInput> * getLeftExtension !currentExtension), (getEdge <| 1<positionInInput> * getRightExtension !currentExtension), (getPosOnEdge <| 1<positionInInput> * getRightExtension !currentExtension), parser.NumToString <| parser.LeftSide.[rule], !currentLength))
                    |> ignore
                      
                pop !currentGSSNode !currentIndex !currentExtension !currentLength
                    
                    
    let control () =
            while not !stop do
            if !condition then dispatcher() else processing()
    let oldMode = System.Runtime.GCSettings.LatencyMode
    let currentProcess = System.Diagnostics.Process.GetCurrentProcess()
    let starting = dotMemory.Check()
    let totalBytesOfMemoryUsed = ref (int64 0)
    GC.Collect()
    
    System.Runtime.CompilerServices.RuntimeHelpers.PrepareConstrainedRegions()
    try
        System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
        control()
        // Generation 2 garbage collection is now
        // deferred, except in extremely low-memory situations
        dotMemory.Check(fun memory ->
          let newObjects = memory.GetDifference(starting).GetNewObjects()
          totalBytesOfMemoryUsed := newObjects.SizeInBytes) |> ignore
        //totalBytesOfMemoryUsed := currentProcess.WorkingSet64 - !totalBytesOfMemoryUsed
        if !totalBytesOfMemoryUsed < (int64 0) then printfn "wtf memory less then 0"
    finally
        // ALWAYS set the latency mode back
        System.Runtime.GCSettings.LatencyMode <- oldMode
   
    
    (*Array.ofSeq r,*) 
    !edgeCount, !vertexCount, !sppfNodes, !totalBytesOfMemoryUsed, !descr
                                  