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
    new (l,l1, r, r1, n) = {le = l; lpos = l1; re = r; rpos = r1; nterm = n}

let buildAbstract<'TokenType> (parser : ParserSourceGLL<'TokenType>) (input : BioParserInputGraph) maxLen condNonTerm = 
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
        let setR = new System.Collections.Generic.Queue<Context2>(100)  
        let currentRule = parser.StartRule
        let currentLabel = ref <| (CommonFuns.packLabelNew currentRule 0) * 1<labelMeasure>
        let tempCount = ref 0
        let r = ref None  
        let currentIndex = ref 0 
        let currentrule = parser.StartRule
        let currentExtension = ref 0L<extension>
        let dummyGSSNode = new Vertex(!currentIndex, int !currentLabel)
        let input = input           
        let edges = Array.init slots.Count (fun _ -> new CompressedArray<SysDict<int64<extension>, SysDict<int, ResizeArray<int>>>> (input.ChainLength, (fun _ -> null),shift))          
        let currentGSSNode = ref <| dummyGSSNode
        
//        for v in input.InitialVertices do
//            for e in outEdges.[v] do 
//                let ext = packExtension e 0
//                let index = pack2to32 e (-shift)
//                setR.Enqueue(new Context2(index, !currentLabel, !currentGSSNode, ext)) 

        for e in input.InitialVertices do
            let ext = packExtension e e
            setR.Enqueue(new Context2(e, !currentLabel, !currentGSSNode, ext))

        let currentContext = ref <| new Context2(!currentIndex, !currentLabel, !currentGSSNode, !currentExtension)
        
        let slotIsEnd (label : int<labelMeasure>) =
            (getPositionNew label) = Array.length (parser.rules.[getRuleNew label])
         
        let containsContext  inputIndex (label : int<labelMeasure>) (vertex : Vertex) extension =
  
            let vertexKey = CommonFuns.pack vertex.Level vertex.NontermLabel
            if setU.[inputIndex] <> Unchecked.defaultof<_>
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
            
        let addContext (inputVertex : int) (label : int<labelMeasure>) vertex extension =
            if not <| containsContext inputVertex label vertex extension 
            then
                setR.Enqueue(new Context2(inputVertex, label, vertex, extension))
                incr descriptorNumber
            else
              incr reused

        let containsEdge (b : Vertex) (e : Vertex) extension =
            let labelN = slots.[int b.NontermLabel]
            let beginLevel = int b.Level
            let endLevel = int e.Level
            let dict1 = edges.[labelN].[beginLevel]
            let cond, dict = 
                if dict1 <> Unchecked.defaultof<_>
                then
                    if dict1.ContainsKey(extension)
                    then
                        let dict2 = dict1.[extension]
                        if dict2.ContainsKey(e.NontermLabel)
                        then
                            let t = dict2.[e.NontermLabel]
                            if t.Contains(e.Level) 
                            then true, None
                            else 
                                t.Add(e.Level) 
                                false, None 
                        else
                            let arr = new ResizeArray<int>()
                            arr.Add(e.Level) 
                            dict2.Add(e.NontermLabel, arr)
                            false, None
                    else
                        let d = new SysDict<int, ResizeArray<int>>()
                        dict1.Add(extension, d)
                        let l = new ResizeArray<int>()
                        l.Add(e.Level)
                        d.Add(e.NontermLabel, l)
                        false, None
                else
                    let newDict1 = new SysDict<int64<extension>, SysDict<int, ResizeArray<int>>>()
                    let newDict2 = new SysDict<int, ResizeArray<int>>()
                    let newArr = new ResizeArray<int>()
                    newArr.Add(e.Level)
                    newDict2.Add(e.NontermLabel, newArr)
                    newDict1.Add(extension, newDict2)
                    false, Some newDict1  
            if dict.IsSome then edges.[labelN].[beginLevel] <- dict.Value
            cond
        
        let create (inputVertex : int) (label : int<labelMeasure>) (vertex : Vertex) extension = 
            let v = new Vertex(inputVertex, int label)
            let vertexKey = pack inputVertex (int label)
            let temp = containsEdge v vertex extension
            if not <| temp 
            then
                if setP.ContainsKey(vertexKey)
                then
                    let arr = setP.[vertexKey]  
                    arr.DoForAll (fun ext  ->
                        let e = packExtension (getLeftExtension extension) (getRightExtension ext)
                        addContext (getRightExtension ext) label vertex e)
            v
                
        let pop (u : Vertex) (i : int) (extension : int64<extension>) =
            if u <> dummyGSSNode
            then
                let vertexKey = pack u.Level (int u.NontermLabel)
                if setP.ContainsKey vertexKey
                then
                    setP.[vertexKey].Add extension
                else
                    let newList = new ResizableUsualOne<_>(extension)
                    setP.Add(vertexKey, newList)
                let outEdges = edges.[slots.[int u.NontermLabel]].[u.Level]
                for edge in outEdges do
                    let extOnEdge = edge.Key
                    for slotLevels in edge.Value do
                         let slot = slotLevels.Key
                         for level in slotLevels.Value do
                            let tt = getLeftExtension extOnEdge
                            let ttt = (getRightExtension extension)
                            let ext = packExtension (getLeftExtension extOnEdge) (getRightExtension extension)
                            let newVertex = new Vertex(level, slot)
                            addContext i (u.NontermLabel*1<labelMeasure>) newVertex ext

        let table = parser.Table
        let condition = ref true 
        let stop = ref false
        let rec dispatcher () =
            let rec get () =
                let c = setR.Dequeue()
                c
            if setR.Count <> 0
            then
                currentContext :=  get ()
                
                currentIndex := currentContext.Value.Index
                
                currentGSSNode := currentContext.Value.Vertex
                currentLabel := currentContext.Value.Label
                currentExtension := currentContext.Value.Extension
                condition := false
            else 
                stop := true  
                              
        and processing () =  
            condition := true
            let rule = getRuleNew !currentLabel
            let position = getPositionNew !currentLabel
            if Array.length parser.rules.[rule] = 0 
            then
              pop !currentGSSNode !currentIndex !currentExtension
            else
                if Array.length parser.rules.[rule] <> position
                then
                    let curSymbol = parser.rules.[rule].[position]
                    if parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol
                    then
                        let cE = CommonFuns.getEdge !currentIndex
                        let cP = CommonFuns.getPosOnEdge !currentIndex + shift
                        let chainLen = input.ChainLength.[cE]
                        if cP < chainLen - 1 
                        then    
                            let curToken = input.Edges.[cE].Tokens.[cP] 
                            if curToken = curSymbol
                            then
                                currentIndex := packEdgePos (getEdge !currentIndex) (1 + getPosOnEdge !currentIndex)
                                currentExtension := packExtension (getLeftExtension !currentExtension) !currentIndex
                                currentLabel := 1<labelMeasure> * packLabelNew rule (position + 1)
                                condition := false
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
                                currentIndex := packEdgePos (getEdge !currentIndex) (1 + getPosOnEdge !currentIndex)
                                currentExtension := packExtension (getLeftExtension !currentExtension) !currentIndex
                                currentLabel := 1<labelMeasure> * packLabelNew rule (position + 1)
                                condition := false
                            | None _ -> ()
                    else 
                        let getIndex nTerm term = 
                            let mutable index = nTerm
                            index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                            index <- index + term - parser.NonTermCount
                            index
                        currentGSSNode := create !currentIndex (1<labelMeasure>  * packLabelNew rule (position + 1)) !currentGSSNode !currentExtension
                        let cE = CommonFuns.getEdge !currentIndex
                        let cP = CommonFuns.getPosOnEdge !currentIndex + shift
                        let chainLen = input.ChainLength.[cE]
                        if cP < chainLen - 1
                        then
                            let curToken = input.Edges.[cE].Tokens.[cP]
                            //let index = getIndex curSymbol curToken
                            let key = int(( curSymbol  <<< 16) ||| (curToken - parser.NonTermCount))
                            //pack2to32 curSymbol (curToken - parser.NonTermCount)
                            let flg,rules = table.TryGetValue key
                            if flg then
                                for rule in rules do
                                    let newLabel = 1<labelMeasure> * packLabelNew rule 0
                                    addContext !currentIndex newLabel !currentGSSNode (packExtension !currentIndex !currentIndex)
                        else
                            for oE in outEdges.[input.Edges.[cE].End] do
                                let curToken = input.Edges.[oE].Tokens.[shift]
                                //let index = getIndex curSymbol curToken
                                let key = int((curSymbol  <<< 16) ||| (curToken - parser.NonTermCount))
                                //pack2to32 curSymbol (curToken - parser.NonTermCount)
                                let flg,rules = table.TryGetValue key
                                if flg then
                                    let t = packEdgePos oE 0
                                    for rule in rules do
                                        let newLabel = 1<labelMeasure> * packLabelNew rule 0
                                        addContext t newLabel !currentGSSNode (packExtension !currentIndex !currentIndex)                              
                else
                    if Array.exists (fun e -> e = rule) condNonTermRules
                    then
                        match !r with
                            | None ->  
                                let t = new ResizeArray<_>()
                                let t1 = new ResultStruct((getEdge <| getLeftExtension !currentExtension), (getPosOnEdge <| getLeftExtension !currentExtension), (getEdge <| getRightExtension !currentExtension), (getPosOnEdge <| getRightExtension !currentExtension), parser.NumToString <| parser.LeftSide.[rule])
                                t.Add t1
                                r := Some t 
                            | Some a -> a.Add(new ResultStruct((getEdge <| getLeftExtension !currentExtension), (getPosOnEdge <| getLeftExtension !currentExtension), (getEdge <| getRightExtension !currentExtension), (getPosOnEdge <| getRightExtension !currentExtension), parser.NumToString <| parser.LeftSide.[rule]))
                        
                    pop !currentGSSNode !currentIndex !currentExtension
                    
                    
        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match !r with
            | None -> 
                Error ("String was not parsed")
            | Some res -> 
                printfn "Reused descriptors %d" !reused
                printfn "All descriptors %d" !descriptorNumber
                Success1 (res) 
                                  