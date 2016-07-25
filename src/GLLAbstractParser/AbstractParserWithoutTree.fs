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
    val nterm  : string
    val length : uint16
    new (l,l1, r, r1, n, len) = {le = l; lpos = l1; re = r; rpos = r1; nterm = n; length = len}
    override this.ToString () = "Start:edge:" + (this.le.ToString()) + ";pos:" + (this.lpos.ToString()) + "--" +
                                "Final:edge:" + (this.re.ToString()) + ";pos:" + (this.rpos.ToString())

let buildAbstract<'TokenType> (parser : ParserSourceGLL<'TokenType>) (input : BioParserInputGraph) (condNonTerm: int) = 
    //let condNonTerm = condNonTermInt*1<labelMeasure>
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
        /// Start rule productions for wich we should output the result
        let condNonTermRules = Seq.toArray <| seq{for i in 0..parser.LeftSide.Length - 1 do if parser.LeftSide.[i] = condNonTerm then yield i}
        let condNonTerm = condNonTerm*1<labelMeasure>
        /// Descriptors
        let setU = new CompressedArray<SysDict<int, SysDict<int64, _(*ResizeArray<int64<extension>>*)>>>(input.ChainLength, (fun _ -> null ),shift)
        /// Poped elements
        let setP = new SysDict<int64, Yard.Generators.Common.DataStructures.ResizableUsualOne<int64<extension>*uint16>>(500)
        let currentRule = parser.StartRule
        let currentLabel = ref <| CommonFuns.packLabelNew currentRule 0
        let currentLength = ref 0us
        let result = new System.Collections.Generic.HashSet<_>()
        let currentIndex = ref (-1<positionInInput>)
        let currentExtension = ref 0L<extension>
        let dummyGSSNode = new Vertex(!currentIndex, -1<labelMeasure>)
        let input = input
        // Edges of GSS
        let edges = Array.init parser.NonTermCount (fun _ -> new CompressedArray<SysDict<_, SysDict<int64<extension>, SysDict<_, _>>>> (input.ChainLength, (fun _ -> null),shift))
        //let vertices = new CompressedArray<ResizeArray<_>>(input.ChainLength, (fun _ -> null),shift)
        let currentGSSNode = ref <| dummyGSSNode
     
        let startContexts = 
            input.InitialVertices
            |> Array.rev
            |> Array.Parallel.mapi(fun i e -> 
                let pos: int<positionInInput> = LanguagePrimitives.Int32WithMeasure e
                let t = getEdge pos
                let tt = getPosOnEdge pos + shift
                let ttt = packEdgePos t tt
                let ext = packExtension ttt ttt
                new Context2(pos, !currentLabel, !currentGSSNode, ext, !currentLength))
        
        //Stack of contexts
        let setR = new System.Collections.Generic.Stack<Context2>(startContexts)  

        let currentContext = ref <| new Context2(!currentIndex, !currentLabel, !currentGSSNode, !currentExtension, !currentLength)
        
        let slotIsEnd (label : int<labelMeasure>) =
            (getPositionNew label) = Array.length (parser.rules.[getRuleNew label])
        
        let getKey (left:int<labelMeasure>) right = 
            int( (int left <<< 16) ||| (right - parser.NonTermCount) )
         
        // Checks for existing of context in SetU. Adds it to SetU.
        let containsContext inputIndex (label : int<labelMeasure>) (vertex : Vertex) extension =
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
        
        /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
        let addContext (inputVertex : int<positionInInput>) (label : int<labelMeasure>) vertex extension len =
            if not <| containsContext inputVertex label vertex extension 
            then
                setR.Push(new Context2(inputVertex, label, vertex, extension, len))
                incr descriptorNumber
            else
                incr reused

        let containsEdge (b : Vertex) (e : Vertex) label extension (len : uint16) =
            let rt = getRuleNew label
            let pt = getPositionNew label
            let nontermName = b.NontermLabel
            let beginLevel = b.Level
            let endLevel = e.Level
            let dictLabelKey = edges.[int nontermName].[beginLevel]
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
                                if Array.contains (e.Level,len) ra
                                then
                                    true, None
                                else
                                    let newSlotlevel = Array.append ra [|e.Level,len|]
                                    dictNontermKey.[e.NontermLabel] <- newSlotlevel
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
                        dictLabelKey.Add(label, d1)
                        false, None
                else 
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
            cond
        
        /// Creates new contexts and descriptors for it.
        let create (cE : int) (cP : int) (label : int<labelMeasure>) (vertex : Vertex) nonTermName (len : uint16) =
            let edgePos = packEdgePos cE cP
            ///let ttttt = getPosOnEdge <| getLeftExtension !currentExtension
            let extension = packExtension (getLeftExtension !currentExtension) edgePos 
            let v = new Vertex(edgePos, nonTermName)
            if edges.[int nonTermName].[edgePos] <> null
            then
                let vertexKey = pack edgePos nonTermName
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
                            let iiii = packEdgePos (getEdge <| (getRightExtension ex)*1<positionInInput>) ((getPosOnEdge <| (getRightExtension ex)*1<positionInInput>) - shift)
                            addContext iiii label vertex e (len + l))
            else
                ignore <| containsEdge v vertex label extension len
                let chainLen = input.ChainLength.[cE]
                let addCntxtForNonTerm e pos index = 
                    let curToken = input.Edges.[e].Tokens.[pos]
                    let key = getKey nonTermName curToken
                    let flg,rules = parser.Table.TryGetValue key
                    if flg then
                        for rule in rules do
                            let newLabel = packLabelNew rule 0
                            addContext !currentIndex newLabel v (packExtension index index) 0us  
                if cP < chainLen
                then
                    addCntxtForNonTerm cE cP edgePos   
                else
                    for oE in outEdges.[input.Edges.[cE].End] do
                        addCntxtForNonTerm oE shift (packEdgePos oE 0)      
            
        /// Adds vertex to setP. For each slot level adds new context.
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
                //foreach label, extension, slot, level
                for labelEdge in outEdges do
                    let labelOnEdge = labelEdge.Key  
                    for edge in labelEdge.Value do
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
                                    addContext i labelOnEdge newVertex ext (len + snd level)

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
                //let t = getEdge !currentIndex
                //let tt = getPosOnEdge !currentIndex
                currentGSSNode := currentContext.Value.Vertex
                currentLabel := currentContext.Value.Label
                currentExtension := currentContext.Value.Extension
                currentLength := currentContext.Value.Length
                //let ttt = getPosOnEdge <| getLeftExtension !currentExtension
                //let aa = getPosOnEdge <| getRightExtension !currentExtension
                condition := false
            else 
                stop := true  
                              
        and processing () =  
            condition := true
            let rule = getRuleNew !currentLabel
            let position = getPositionNew !currentLabel
            /// Moves positions in input and grammar by 1.
            let eatTerm () =
                let pos = (1 + getPosOnEdge !currentIndex)
                currentIndex := packEdgePos (getEdge !currentIndex) pos
                currentLength := !currentLength + 1us
                //let ttt = getPosOnEdge (getLeftExtension !currentExtension)
                let le = (getLeftExtension !currentExtension)
                let re = packEdgePos (getEdge !currentIndex) ((getPosOnEdge !currentIndex) + shift)
                currentExtension := packExtension le re
                currentLabel := packLabelNew rule (position + 1)
                condition := false
            if Array.length parser.rules.[rule] = 0 
            then//current rule is epsilon
                if Array.exists (fun e -> e = rule) condNonTermRules
                then
                    result.Add(new ResultStruct(getEdge <| (getLeftExtension !currentExtension)*1<positionInInput>,
                                                getPosOnEdge <| (getLeftExtension !currentExtension)*1<positionInInput>,
                                                getEdge <| (getRightExtension !currentExtension)*1<positionInInput>,
                                                getPosOnEdge <| (getRightExtension !currentExtension)*1<positionInInput>,
                                                parser.NumToString <| int parser.LeftSide.[rule],
                                                !currentLength))
                    |> ignore
                
                pop !currentGSSNode !currentIndex !currentExtension !currentLength
            else//current rule is not epsilon     
                if Array.length parser.rules.[rule] <> position
                then//rule isn't finished(current position is in current rule)
                    ///current grammar symbol
                    let curSymbol = parser.rules.[rule].[position]*1<labelMeasure>
                    ///current edge in input
                    let cE = CommonFuns.getEdge !currentIndex
                    ///current position on edge in input
                    let cP = CommonFuns.getPosOnEdge !currentIndex + shift
                    let chainLen = input.ChainLength.[cE]
                    if parser.NumIsTerminal (int curSymbol) || parser.NumIsLiteral (int curSymbol)
                    then//current grammar symbol is terminal
                        if cP < chainLen
                        then//edge isn't finished(cur token on edgee is on current edge)
                            let curToken = input.Edges.[cE].Tokens.[cP] 
                            if curToken = int curSymbol
                            then eatTerm ()
                        else//edge is finished(cur token on edgee is not on current edge)
                            let curEdge = 
                                let oEdges = outEdges.[input.Edges.[cE].End]
                                //result will be only one, bacause input doesn't contain vetices with out edges with equal first tokens
                                oEdges
                                |> List.tryFind (fun outEdge -> int curSymbol = input.Edges.[outEdge].Tokens.[shift])
                                
                            match curEdge with
                            | Some edge ->
                                // One of out edges starts with token similar with current terminal.
                                // Move positions.
                                currentIndex := (packEdgePos edge 0)
                                eatTerm ()
                            | None _ -> ()
                    else//current grammar symbol is nonterminal
                        if cP < chainLen
                        then//edge isn't finished(cur token on edgee is on current edge)
                            let curToken = input.Edges.[cE].Tokens.[cP]
                            
                            //Initialise counter for length of result
                            if curSymbol = condNonTerm then currentLength := 0us

                            let key = getKey curSymbol curToken
                            if parser.Table.ContainsKey key
                            then// Found nonterninal in rule, and it have derivation starting with current token.
                                // Create new descriptors.
                                create cE cP (packLabelNew rule (position + 1)) !currentGSSNode curSymbol !currentLength
                        else//edge is finished(cur token on edgee is not on current edge)
                            let oEdges = outEdges.[input.Edges.[cE].End]
                            for oe in oEdges do 
                                let curToken = input.Edges.[oe].Tokens.[shift]
                                if curSymbol = condNonTerm then currentLength := 0us
                                let key = getKey curSymbol curToken
                                if parser.Table.ContainsKey key
                                then
                                    // Found nonterninal in rule, and it have derivation
                                    //     starting with first token of one of the out edges.
                                    // Create new descriptors for this token.
                                    create oe shift (packLabelNew rule (position + 1)) !currentGSSNode curSymbol !currentLength    
                else//rule finished(current position isn't in current rule)
                    if Array.exists (fun e -> e = rule) condNonTermRules
                    then
                        result.Add(new ResultStruct(getEdge <| (getLeftExtension !currentExtension)*1<positionInInput>,
                                                    getPosOnEdge <| (getLeftExtension !currentExtension)*1<positionInInput>,
                                                    getEdge <| (getRightExtension !currentExtension)*1<positionInInput>,
                                                    getPosOnEdge <| (getRightExtension !currentExtension)*1<positionInInput>,
                                                    parser.NumToString <| int parser.LeftSide.[rule],
                                                    !currentLength))
                        |> ignore
                      
                    pop !currentGSSNode !currentIndex !currentExtension !currentLength
                    
                    
        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match result.Count with
            | 0 -> 
                Error ("String was not parsed")
            | _ -> 
                //printfn "Reused descriptors %d" !reused
                //printfn "All descriptors %d" !descriptorNumber
                Success1 (Array.ofSeq result) 
                                  