module Yard.Generators.GLL.AbstractParserWithoutTree

open System 
open Microsoft.FSharp.Collections

open FSharpx.Collections.Experimental

open Yard.Generators.GLL 
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns

type SysDict<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>
type CompressedArray<'t> = Yard.Generators.GLL.ParserCommon.CompressedArray<'t>

[<Struct>]
type M =
    val pos : int64
    val lbl : int<labelMeasure>
    new (p,l) = {pos = p; lbl = l}
        
[<Struct>]
type ResultStruct =
    val le : int
    val lpos : int
    val re : int
    val rpos : int
    val nterm : string
    val length   : byte
    new (l,l1, r, r1, n, len) = {le = l; lpos = l1; re = r; rpos = r1; nterm = n; length = len}

let buildAbstract<'TokenType> (parser : ParserSourceGLL<'TokenType>) (input : BioParserInputGraph) condNonTerm = 
    let shift = input.Shift
    if input.EdgeCount = 0 then
      Error ("This grammar does not accept empty input.")     
    else
        let outEdges = 
            let r = Array.init<List<int>> input.VertexCount (fun _ -> List<int>.Empty)
            for i in 0..input.EdgeCount - 1 do
                r.[input.Edges.[i].Start] <- i :: r.[input.Edges.[i].Start]
            r
        let parser = parser
        let reused = ref 0
        let descriptorNumber = ref 0
        let slots = parser.Slots
        let condNonTermRules = Seq.toArray <| seq{for i in 0..parser.LeftSide.Length - 1 do if parser.LeftSide.[i] = condNonTerm then yield i}
        let setU = new CompressedArray<SysDict<int, SysDict<int64, ResizeArray<int64<extension>>>>>(input.ChainLength, (fun _ -> null ),shift) 
        let setP = new SysDict<int64, Yard.Generators.Common.DataStructures.ResizableUsualOne<int64<extension>>>(500)
        let setR = new System.Collections.Generic.Stack<Context2>(100)  
        let currentRule = parser.StartRule
        let currentLabel = ref <| (CommonFuns.packLabelNew currentRule 0) * 1<labelMeasure>
        let tempCount = ref 0
        let currentLength = ref 0uy
        let r = new System.Collections.Generic.HashSet<_>()
        let currentIndex = ref 0 
        let currentrule = parser.StartRule
        let currentExtension = ref 0L<extension>
        let dummyGSSNode = new Vertex(!currentIndex, -1)
        let input = input           
        let edges = Array.init parser.NonTermCount (fun _ -> new CompressedArray<SysDict<int, SysDict<int64<extension>, SysDict<int, ResizeArray<int>>>>> (input.ChainLength, (fun _ -> null),shift))          
        let vertices = new CompressedArray<ResizeArray<_>>(input.ChainLength, (fun _ -> null),shift)
        let currentGSSNode = ref <| dummyGSSNode
     
        for i = input.InitialVertices.Length - 1 downto 0 do
            let e = input.InitialVertices.[i]
            let t = getEdge e
            let tt = getPosOnEdge e + shift
            let ttt = packEdgePos t tt
            let ext = packExtension ttt ttt
            setR.Push(new Context2(e, !currentLabel, !currentGSSNode, ext, !currentLength))

        let currentContext = ref <| new Context2(!currentIndex, !currentLabel, !currentGSSNode, !currentExtension, !currentLength)
        
        let slotIsEnd (label : int<labelMeasure>) =
            (getPositionNew label) = Array.length (parser.rules.[getRuleNew label])
         
        let containsContext  inputIndex (label : int<labelMeasure>) (vertex : Vertex) extension =
            let vertexKey = CommonFuns.pack vertex.Level vertex.NontermLabel
            if setU.[inputIndex] <> null
            then
                let cond, current = setU.[inputIndex].TryGetValue(int label) 
                if  cond
                then
                    if current.ContainsKey vertexKey
                    then
                        let trees = current.[vertexKey]
                        if not <| trees.Contains extension
                        then 
                            trees.Add extension
                            false
                        else
                            true
                    else 
                        let arr = new ResizeArray<int64<extension>>()
                        arr.Add extension
                        current.Add(vertexKey, arr)                    
                        false
                else 
                    let dict = new SysDict<_, ResizeArray<_>>()
                    setU.[inputIndex].Add(int label, dict)
                    let arr = new ResizeArray<_>()
                    arr.Add extension
                    dict.Add(vertexKey, arr) 
                    false
            else 
                let dict1 = new SysDict<_, _>()
                setU.[inputIndex] <- dict1
                let dict2 = new SysDict<_, ResizeArray<_>>()
                dict1.Add(int label, dict2)
                let arr = new ResizeArray<_>()
                arr.Add extension
                dict2.Add(vertexKey, arr)
                false
            
        let addContext (inputVertex : int) (label : int<labelMeasure>) vertex extension len =
            if not <| containsContext inputVertex label vertex extension 
            then
                setR.Push(new Context2(inputVertex, label, vertex, extension, len))
                incr descriptorNumber
            else
              incr reused

        let containsEdge (b : Vertex) (e : Vertex) label extension =
            let rt = getRuleNew label
            let pt = getPositionNew label
            let nontermName = b.NontermLabel
            let beginLevel = int b.Level
            let endLevel = int e.Level
            let dictLabelKey = edges.[nontermName].[beginLevel]
            let cond, dict = 
                if dictLabelKey <> null
                then
                    let c1, dictExtensionKey = dictLabelKey.TryGetValue label
                    if c1
                    then
                        let c2, dictNontermKey = dictExtensionKey.TryGetValue extension
                        if c2
                        then
                            let c3, ra = dictNontermKey.TryGetValue e.NontermLabel
                            if c3
                            then
                                if ra.Contains(e.Level) then true, None
                                else
                                    ra.Add(e.Level)
                                    false, None
                            else
                                let arr = new ResizeArray<int>()
                                arr.Add(e.Level) 
                                dictNontermKey.Add(e.NontermLabel, arr)
                                false, None
                        else
                            let ra = new ResizeArray<int>()
                            ra.Add(e.Level)
                            let d = new SysDict<int, ResizeArray<int>>()
                            d.Add(e.NontermLabel, ra)
                            dictExtensionKey.Add(extension, d)
                            false, None    
                    else
                        let d1 = new SysDict<int64<extension>, SysDict<int, ResizeArray<int>>>()
                        let d2 = new SysDict<int, ResizeArray<int>>()
                        let ra = new ResizeArray<int>()
                        ra.Add(e.Level)
                        d2.Add(e.NontermLabel, ra)
                        d1.Add(extension, d2)
                        dictLabelKey.Add(label, d1)
                        false, None
                else 
                    let d0 = new SysDict<int, SysDict<int64<extension>, SysDict<int, ResizeArray<int>>>>()
                    let d1 = new SysDict<int64<extension>, SysDict<int, ResizeArray<int>>>()
                    let d2 = new SysDict<int, ResizeArray<int>>()
                    let ra = new ResizeArray<int>()
                    ra.Add(e.Level)
                    d2.Add(e.NontermLabel, ra)
                    d1.Add(extension, d2)
                    d0.Add(label, d1)
                    false, Some d0
            if dict.IsSome then edges.[nontermName].[beginLevel] <- dict.Value
            cond
        
        let create (cE : int) (cP : int) (label : int<labelMeasure>) (vertex : Vertex) curSymbol len =   
            let nonTermName = curSymbol
            let i = packEdgePos cE cP
            let ttttt = getPosOnEdge <| getLeftExtension !currentExtension
            let extension = packExtension (getLeftExtension !currentExtension) i 
            let v = new Vertex(i, nonTermName)
            if edges.[nonTermName].[i] <> null
            then
                let vertexKey = pack i nonTermName
                let temp = containsEdge v vertex (int label) extension 
                if not <| temp 
                then
                    if setP.ContainsKey(vertexKey)
                    then
                        let arr = setP.[vertexKey]  
                        arr.DoForAll (fun ext  ->
                            let e = packExtension (getLeftExtension extension) (getRightExtension ext)
                            let iiii = packEdgePos (getEdge (getRightExtension ext)) ((getPosOnEdge <| getRightExtension ext) - shift)
                            addContext iiii label vertex e len)
            else
                ignore <| containsEdge v vertex (int label) extension
                let chainLen = input.ChainLength.[cE]
                let addCntxtForNonTerm e pos index = 
                    let curToken = input.Edges.[e].Tokens.[pos]
                    let key = int(( curSymbol  <<< 16) ||| (curToken - parser.NonTermCount))
                    let flg,rules = parser.Table.TryGetValue key
                    if flg then
                        for rule in rules do
                            let newLabel = 1<labelMeasure> * packLabelNew rule 0
                            addContext !currentIndex newLabel v (packExtension index index) len  
                if cP < chainLen - 1
                then
                    addCntxtForNonTerm cE cP i   
                else
                    for oE in outEdges.[input.Edges.[cE].End] do
                        addCntxtForNonTerm oE shift (packEdgePos oE 0)      
            
                
        let pop (u : Vertex) (i : int) (extension : int64<extension>) len =
            if u <> dummyGSSNode
            then
                let vertexKey = pack u.Level (int u.NontermLabel)
                if setP.ContainsKey vertexKey
                then
                    setP.[vertexKey].Add extension
                else
                    let newList = new ResizableUsualOne<_>(extension)
                    setP.Add(vertexKey, newList)
                let outEdges = edges.[u.NontermLabel].[u.Level]
                for eLabel in outEdges do
                    let labelOnEdge = eLabel.Key  
                    for edge in eLabel.Value do
                        let extOnEdge = edge.Key
                        for slotLevels in edge.Value do
                                let slot = slotLevels.Key
                                for level in slotLevels.Value do
                                    let l = (getLeftExtension extOnEdge)
                                    let tt = getPosOnEdge l
                                    
                                    let r = (getRightExtension extension)
                                    let ttt = getPosOnEdge r
                                    let ext = packExtension l r
                                    let newVertex = new Vertex(level, slot)
                                    addContext i (labelOnEdge*1<labelMeasure>) newVertex ext len

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
                let ttt = getPosOnEdge <| getLeftExtension !currentExtension
                let aa = getPosOnEdge <| getRightExtension !currentExtension
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
                currentLength := !currentLength + 1uy
                let ttt = getPosOnEdge (getLeftExtension !currentExtension)
                let le = (getLeftExtension !currentExtension)
                let re = packEdgePos (getEdge !currentIndex) ((getPosOnEdge !currentIndex) + shift)
                currentExtension := packExtension le re
                currentLabel := 1<labelMeasure> * packLabelNew rule (position + 1)
                condition := false
            if Array.length parser.rules.[rule] = 0 
            then
                if Array.exists (fun e -> e = rule) condNonTermRules
                then
                    r.Add(new ResultStruct((getEdge <| getLeftExtension !currentExtension), (getPosOnEdge <| getLeftExtension !currentExtension), (getEdge <| getRightExtension !currentExtension), (getPosOnEdge <| getRightExtension !currentExtension), parser.NumToString <| parser.LeftSide.[rule], !currentLength))
                    |> ignore
                pop !currentGSSNode !currentIndex !currentExtension !currentLength
            else
                setR.Count                
                if Array.length parser.rules.[rule] <> position
                then
                    let curSymbol = parser.rules.[rule].[position]
                    let cE = CommonFuns.getEdge !currentIndex
                    let cP = CommonFuns.getPosOnEdge !currentIndex + shift
                    let chainLen = input.ChainLength.[cE]
                    if parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol
                    then                                                
                        if cP < chainLen - 1 
                        then    
                            let curToken = input.Edges.[cE].Tokens.[cP] 
                            if curToken = curSymbol
                            then
                                eatTerm ()
                        else   
                            let curEdge = 
                                let oEdges = outEdges.[input.Edges.[cE].End]
                                let mutable res = None
                                for oe in oEdges do
                                    if curSymbol = input.Edges.[oe].Tokens.[shift] then
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
                            let key = int(( curSymbol  <<< 16) ||| (curToken - parser.NonTermCount))
                            if parser.Table.ContainsKey key
                            then
                                create cE cP (1<labelMeasure>  * packLabelNew rule (position + 1)) !currentGSSNode curSymbol !currentLength
                        else
                            let oEdges = outEdges.[input.Edges.[cE].End]
                            for oe in oEdges do 
                                let curToken = input.Edges.[oe].Tokens.[shift]
                                let key = int(( curSymbol  <<< 16) ||| (curToken - parser.NonTermCount))
                                if parser.Table.ContainsKey key
                                then
                                    create oe shift (1<labelMeasure>  * packLabelNew rule (position + 1)) !currentGSSNode curSymbol !currentLength
                            
                                                        
                    
                else
                    if Array.exists (fun e -> e = rule) condNonTermRules
                    then
                        r.Add(new ResultStruct((getEdge <| getLeftExtension !currentExtension), (getPosOnEdge <| getLeftExtension !currentExtension), (getEdge <| getRightExtension !currentExtension), (getPosOnEdge <| getRightExtension !currentExtension), parser.NumToString <| parser.LeftSide.[rule], !currentLength))
                        |> ignore
                        
                    pop !currentGSSNode !currentIndex !currentExtension !currentLength
                    
                    
        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match r.Count with
            | 0 -> 
                Error ("String was not parsed")
            | _ -> 
                //printfn "Reused descriptors %d" !reused
                //printfn "All descriptors %d" !descriptorNumber
                Success1 (Array.ofSeq r) 
                                  