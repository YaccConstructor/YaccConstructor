module Yard.Generators.GLL.AbstractParser 

open System 
open Microsoft.FSharp.Collections

open FSharpx.Collections.Experimental

open Yard.Generators.GLL 
open YC.TreeProcessor
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
        

let buildAbstractAst<'TokenType> (parser : ParserSourceGLL<'TokenType>) (input : BioParserInputGraph<'TokenType>) maxLen : ParserCommon.ParseResult<_> = 
    if input.EdgeCount = 0 then
      //  if parser.AcceptEmptyInput then
      //      let eps = new Nonnte
            //Success (new Tree<_>(null, getEpsilon startNonTerm, null))
     //   else
            Error ("This grammar does not accept empty input.")     
    else
        let outEdges = 
            let r = Array.init<List<int>> input.VertexCount (fun _ -> List<int>.Empty)
            for i in 0..input.EdgeCount - 1 do
                r.[input.Edges.[i].Start] <- i :: r.[input.Edges.[i].Start]
            r
        let parser = parser
        let slots = parser.Slots
        let errors = new SysDict<int64, SysDict<int<nodeMeasure>, Vertex*int>>()   
        let setU = new CompressedArray<SysDict<int, SysDict<int64, ResizeArray<int<nodeMeasure>>>>>(input.ChainLength, (fun _ -> null ))
        let structures = new ParserStructures<'TokenType>(parser.StartRule)
        let setR = structures.SetR
        let epsilonNode = structures.EpsilonNode
        let setP = structures.SetP
        let tempCount = ref 0

        let currentIndex = ref 0
        let mutable currentProb = 1.0        
        //o.Start()
        
        
        //let currentPath = ref <| List.empty<ParserEdge<'TokenType*ref<bool>>>
        let currentrule = parser.StartRule

        //let finalPaths = new ResizeArray<list<ParserEdge<'TokenType*ref<bool>>>>()
        //let errorPaths = new ResizeArray<list<ParserEdge<'TokenType*ref<bool>>>>()

         //packLabel without int
        let dummyGSSNode = new Vertex(!currentIndex, int !structures.CurrentLabel)
        let sppfNodes = structures.SppfNodes
        
        let tokens = new BlockResizeArray<'TokenType>()  
        let input = input           
        let packedNodes = new SysDict<int, SysDict<M, int<nodeMeasure>>>()

        let isEnd = ref false
        let treeProc = new TreeProcessor<_>(parser, tokens)
        let o = treeProc.printerAgent (fun x -> 
            printfn "Ranges = %A" x
            isEnd := true)

        let nonTerminalNodes = new SysDict<int64,int<nodeMeasure>>()        
        let intermidiateNodes = new CompressedArray<_>(input.ChainLength, (fun _ -> new CompressedArray<_>(input.ChainLength, fun _ -> Unchecked.defaultof<SysDict<int<labelMeasure>, int<nodeMeasure>>>)))
        //let edges = new CompressedArray<_>(input.ChainLength, (fun _ -> Array.zeroCreate<SysDict<int<nodeMeasure>, SysDict<int, ResizeArray<int>>>> slots.Count))
        let edges = Array.init slots.Count (fun _ -> new CompressedArray<_> (input.ChainLength, (fun _ -> null)))
        let terminalNodes = new CompressedArray<_>(input.ChainLength, (fun _ -> 0 * 1<nodeMeasure> ))
        let currentGSSNode = ref <| dummyGSSNode
        for v in input.InitialVertices do
            let oEdges = outEdges.[v]
            for e in oEdges do
                setR.Enqueue(new Context(pack2to32 e 0, !structures.CurrentLabel, !currentGSSNode, structures.Dummy)) 
        let currentContext = ref <| new Context(!currentIndex, !structures.CurrentLabel, !currentGSSNode, structures.Dummy) //without *1<labelMeasure>
        
        let finalExtensions =
            let len = input.InitialVertices.Length
            let arr = Array.zeroCreate<int64<extension>> len
            for i = 0 to len - 1 do
                let oE = outEdges.[input.InitialVertices.[i]]
                for e in oE do
                    arr.[i] <- packExtension (pack2to32 e 0) (pack2to32 (input.Edges.Length - 1) 1)          
            arr

        let slotIsEnd (label : int<labelMeasure>) =
            (getRight32 label) = Array.length (parser.rules.[getRule label])

        let findSppfNode (label : int<labelMeasure>) lExt rExt : int<nodeMeasure> =
            let isEnd = slotIsEnd <| label
            let lExt = int lExt
            let rExt = int rExt
            let nTerm = parser.LeftSide.[getRule label]
            
            if isEnd
            then
                if not <| nonTerminalNodes.ContainsKey(pack3 nTerm lExt rExt)
                then
                    let newNode = new NonTerminalNode(nTerm, (packExtension lExt rExt), 0)
                    sppfNodes.Add(newNode)
                    let num = sppfNodes.Length - 1
                    nonTerminalNodes.Add((pack3 nTerm lExt rExt), num*1<nodeMeasure>)
                    if parser.NumToString newNode.Name = "folded" then o.Post(NodeToProcess(newNode))
                    num*1<nodeMeasure>
                else
                    nonTerminalNodes.[pack3 nTerm lExt rExt]
            else
                if intermidiateNodes.[lExt].[rExt] = Unchecked.defaultof<SysDict<int<labelMeasure>, int<nodeMeasure>>>
                then
                    let d = new SysDict<int<labelMeasure>, int<nodeMeasure>>(2)
                    let newNode = new IntermidiateNode(int label, (packExtension lExt rExt), 0)
                    sppfNodes.Add(newNode)
                    let num = (sppfNodes.Length - 1)*1<nodeMeasure>
                    d.Add(label, num)
                    intermidiateNodes.[lExt].[rExt] <- d 
                    num
                else
                    let dict = intermidiateNodes.[lExt].[rExt] 
                    if dict.ContainsKey label
                    then
                        dict.[label]
                    else
                        let newNode = new IntermidiateNode(int label, (packExtension lExt rExt), 0)
                        sppfNodes.Add(newNode)
                        let num = (sppfNodes.Length - 1)*1<nodeMeasure>
                        dict.Add(label, num)
                        num

        let findSppfPackedNode (symbolNode : int<nodeMeasure>) (label : int<labelMeasure>) lExt (rExt : int64<extension>) (left : INode) (right : INode) : int<nodeMeasure> = 
            let i = getLeftExtension lExt
            let rule = getRule label      
            let length = left.getLength() + right.getLength()   
            let c, v = packedNodes.TryGetValue(i)
            if c 
            then    
                let key = new M (int64 rExt, label)
                let flg,res = v.TryGetValue(key)            
                if flg
                then res
                else
                    let newNode = new PackedNode(rule, left, right, length)
                    sppfNodes.Add(newNode)
                    let num = (sppfNodes.Length - 1 )*1<nodeMeasure>
                    v.Add(key, num)
                    match sppfNodes.Item (int symbolNode) with
                    | :? NonTerminalNode as n ->
                        n.AddChild newNode
                        if n.Length <> 0 && n.Length > length then n.SetLength length
                    | :? IntermidiateNode as i ->
                        i.AddChild newNode
                        if i.Length <> 0 && i.Length > length then i.SetLength length
                    | _ -> ()
                    num
            else
                let d = new SysDict<_, _>()
                let key = new M (int64 rExt, label)
                let newNode = new PackedNode(rule, left, right, length)
                sppfNodes.Add(newNode)
                let num = (sppfNodes.Length - 1 )*1<nodeMeasure>
                d.Add(key, num)
                packedNodes.Add(i, d)
                match sppfNodes.Item (int symbolNode) with
                | :? NonTerminalNode as n ->
                    n.AddChild newNode
                    if n.Length <> 0 && n.Length > length then n.SetLength length
                | :? IntermidiateNode as i ->
                    i.AddChild newNode
                    if i.Length <> 0 && i.Length > length then i.SetLength length
                | _ -> ()
                num
                
                  
        let getNodeT (index) =
            if terminalNodes.[index] <> Unchecked.defaultof<int<nodeMeasure>>
            then
                terminalNodes.[index]
            else
                let ext = packExtension index (pack2to32 (getLeft32 index) (1 + getRight32 index))
                let t = new TerminalNode(index, ext, 1)
                sppfNodes.Add t
                let res = (sppfNodes.Length - 1) * 1<nodeMeasure>
                terminalNodes.[index] <- res
                res
            
                     
        let containsEdge (b : Vertex) (e : Vertex) ast =
            let labelN = slots.[int b.NontermLabel]
            let beginLevel = int b.Level
            let endLevel = int e.Level
            let dict1 = edges.[labelN].[beginLevel]
            let cond, dict = structures.ContainsEdge dict1 ast e
            if dict.IsSome then edges.[labelN].[beginLevel] <- dict.Value
            cond
        
        
        let create (inputVertex : int) (label : int<labelMeasure>) (vertex : Vertex) (ast : int<nodeMeasure>) (*prob sLength*) = 
            let v = new Vertex(inputVertex, int label)
            let vertexKey = pack inputVertex (int label)
            let temp = containsEdge v vertex ast
            if not <| temp //containsEdge v vertex ast
            then
                if setP.ContainsKey(vertexKey)
                then
                    let arr = setP.[vertexKey]
                    arr.DoForAll (fun tree  ->
                        let y = structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy label ast tree
//                        if slotIsEnd label 
//                        then 
//                            //let name = parser.NumToString <| parser.LeftSide.[getRule label]
//                            o.Post(NodeToProcess(sppfNodes.[int y]))
                        let index = getRightExtension <| structures.GetTreeExtension y 
                        structures.AddContext setU index label vertex y maxLen (*!currentPath*))
            v
                
        let pop (u : Vertex) (i : int) (z : int<nodeMeasure>) (*prob sLength*) =
            if u <> dummyGSSNode
            then
                let vertexKey = pack u.Level (int u.NontermLabel)
                //let f,v = setP.TryGetValue vert
                if setP.ContainsKey vertexKey
                then
                    setP.[vertexKey].Add z
                else
                    let newList = new ResizableUsualOne<_>(z)
                    setP.Add(vertexKey, newList)
                let outEdges = edges.[slots.[int u.NontermLabel]].[u.Level]
                for edge in outEdges do
                    let sppfNodeOnEdge = edge.Key
                    for slotLevels in edge.Value do
                         let slot = slotLevels.Key
                         for level in slotLevels.Value do
                            let resTree = structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy (u.NontermLabel*1<labelMeasure>) sppfNodeOnEdge z 
//                            if slotIsEnd (u.NontermLabel*1<labelMeasure>) 
//                            then 
                                //let name = parser.NumToString <| parser.LeftSide.[getRule (u.NontermLabel*1<labelMeasure>)]
                                //o.Post(NodeToProcess(sppfNodes.[int resTree]))
                            let newVertex = new Vertex(level, slot)
                            structures.AddContext setU i (u.NontermLabel*1<labelMeasure>) newVertex resTree maxLen //!currentPath

        let table = parser.Table
        
        let condition = ref true 
        let stop = ref false

        (*let containsError index label vertex ast currentPath = 
            let key = pack index label 
            let c, d = errors.TryGetValue(key)
            if c then
                let c1, d1 = d.TryGetValue(ast)
                if not c1 
                then 
                    errorPaths.Add currentPath
                    d.Add(ast, (vertex, (errorPaths.Count - 1)))
                
            else
                let d = new SysDict<int<nodeMeasure>, Vertex*int>()
                errorPaths.Add currentPath
                d.Add(ast, (vertex, (errorPaths.Count - 1)))
                errors.Add(key, d)*)
            

        let rec dispatcher () =
            let rec get () =
                let c = setR.Dequeue()
                if currentContext.contents.SLength > 100
                then
                  get()   //currentContext.contents.Probability < (0.1e-200) then get()
                else c
            if setR.Count <> 0
            then
                currentContext :=  get ()
                currentIndex := currentContext.Value.Index
                let t = CommonFuns.getLeft32 !currentIndex
                let t2 = CommonFuns.getRight32 !currentIndex
                currentGSSNode := currentContext.Value.Vertex
                structures.CurrentLabel := currentContext.Value.Label
                structures.CurrentN := currentContext.Value.Ast 
                structures.CurrentR := structures.Dummy
                //currentProb <- currentContext.contents.Probability
                //currentPath := currentContext.Value.Path
                condition := false
            else 
                stop := true  
                              
        and processing () =  
            condition := true
            let rule = getRule !structures.CurrentLabel
            let position = getRight32 !structures.CurrentLabel
            if Array.length parser.rules.[rule] = 0 
            then
              let t = new TerminalNode(-1, packExtension !currentIndex !currentIndex, 0)
              sppfNodes.Add t
              let res = sppfNodes.Length - 1
              structures.CurrentR := res * 1<nodeMeasure>
              structures.CurrentN := structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy !structures.CurrentLabel !structures.CurrentN !structures.CurrentR  
              pop !currentGSSNode !currentIndex !structures.CurrentN //prob sLength
            else
                if Array.length parser.rules.[rule] <> position
                then
                    let curSymbol = parser.rules.[rule].[position]
                   // if !currentVertexInInput <> input.FinalState
                   // then
                    if parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol
                    then
                        let isEq (sym : int) (elem : ParserEdge<'TokenType>) = sym = parser.TokenToNumber (elem.Tag)
                        let cE = CommonFuns.getLeft32 !currentIndex
                        let cP = CommonFuns.getRight32 !currentIndex
                        let chainLen = input.ChainLength.[cE]
                        if cP < chainLen - 1 
                        then    
                            let curToken = input.Edges.[cE].Tokens.[cP] 
                            if curToken = curSymbol
                            then
                                if !structures.CurrentN = structures.Dummy
                                then structures.CurrentN := getNodeT !currentIndex
                                else structures.CurrentR := getNodeT !currentIndex
                                currentIndex := pack2to32 (getLeft32 !currentIndex) (1 + getRight32 !currentIndex)
                                structures.CurrentLabel := 1<labelMeasure> * pack2to32 rule (position + 1)
                                if !structures.CurrentR <> structures.Dummy
                                then 
                                    structures.CurrentN := structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy !structures.CurrentLabel !structures.CurrentN !structures.CurrentR
                                condition := false
                                
                        else   
                            let curEdge = 
                                let oEdges = outEdges.[input.Edges.[cE].End]
                                let mutable res = None
                                for oe in oEdges do
                                    if curSymbol = input.Edges.[oe].Tokens.[0] then
                                        res <- Some oe  
                                res
                            
                            match curEdge with
                            | Some edge ->
                                currentIndex := (pack2to32 curEdge.Value 0)
                                let curToken = input.Edges.[edge].Tokens.[0]
                                if !structures.CurrentN = structures.Dummy
                                then structures.CurrentN := getNodeT !currentIndex
                                else structures.CurrentR := getNodeT !currentIndex
                                currentIndex := pack2to32 (getLeft32 !currentIndex) (1 + getRight32 !currentIndex)
                                structures.CurrentLabel := 1<labelMeasure> * pack2to32 rule (position + 1)
                                if !structures.CurrentR <> structures.Dummy
                                then 
                                    structures.CurrentN := structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy !structures.CurrentLabel !structures.CurrentN !structures.CurrentR
//                                    if slotIsEnd !structures.CurrentLabel
//                                    then 
//                                        //let name = parser.NumToString <| parser.LeftSide.[getRule !structures.CurrentLabel]
//                                        let node = sppfNodes.[int !structures.CurrentN]
//                                        o.Post(NodeToProcess(node))
                                condition := false
                            | 
                                None _ -> ()
                                    //containsError !currentVertexInInput !structures.CurrentLabel !currentGSSNode !structures.CurrentN !currentPath
                                
                    else 
                        let getIndex nTerm term = 
                            let mutable index = nTerm
                            index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                            index <- index + term - parser.NonTermCount
                            index
                        currentGSSNode := create !currentIndex (1<labelMeasure>  * pack2to32 rule (position + 1)) !currentGSSNode  !structures.CurrentN //prob sLength
                        let cE = CommonFuns.getLeft32 !currentIndex
                        let cP = CommonFuns.getRight32 !currentIndex
                        let chainLen = input.ChainLength.[cE]
                        if cP < chainLen - 1
                        then
                            let curToken = input.Edges.[cE].Tokens.[cP]
                            let index = getIndex curSymbol curToken
                            let key =  int((int32 curSymbol <<< 16) ||| int32 (curToken - parser.NonTermCount))    
                            let flg,rules = table.TryGetValue key
                            if flg then
                                for rule in rules do
                                    let newLabel = 1<labelMeasure> * pack2to32 rule 0
                                    structures.AddContext setU !currentIndex newLabel !currentGSSNode structures.Dummy maxLen 
                        else
                            for oE in outEdges.[input.Edges.[cE].End] do
                                let curToken = input.Edges.[oE].Tokens.[0]
                                let index = getIndex curSymbol curToken
                                let key =  int((int32 curSymbol <<< 16) ||| int32 (curToken - parser.NonTermCount))    
                                let flg,rules = table.TryGetValue key
                                if flg then
                                    let t = pack2to32 oE 0
                                    for rule in rules do
                                        let newLabel = 1<labelMeasure> * pack2to32 rule 0
                                        structures.AddContext setU t newLabel !currentGSSNode structures.Dummy maxLen 
                            
                                    
                       
                                            
                else
                    let curRight =  sppfNodes.Item (int !structures.CurrentN) 
                    let r = curRight.getExtension ()
                    //if Array.exists ((=) r) finalExtensions then finalPaths.Add !currentPath
                    structures.FinalMatching
                        curRight 
                        parser.LeftSide.[parser.StartRule]
                        finalExtensions
                        findSppfNode
                        findSppfPackedNode
                        currentGSSNode
                        currentIndex
                        pop
                  
                    
        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match !structures.ResultAST with
            | None -> 
                if errors.Count <> 0 then
                    for e in errors do
                        printfn "Position %d rule %d" (getLeft e.Key) (getRight e.Key >>> 16)
                
                Error ("String was not parsed")
            | Some res -> 
                    let r1 = new Tree<_> (input, res, parser.rules)
                    //r1.AstToDot parser.NumToString parser.TokenToNumber parser.TokenData "AST123456.dot"
                                            (*let isSubpath l1 l2 =
                        List.length l1 <= List.length l2 
                        && Seq.forall2 (=) l1 (Seq.take (List.length l1) l2)*)
                    //setU |> Seq.iter(fun x -> x |> Seq.iter (fun x -> printf "%A; " x.Value.Count))
                    //r1.AstToDot parser.NumToString parser.TokenToNumber parser.TokenData "AST123456.dot"
                    (*for e in errors do
                        for p in e.Value do
                            let path = List.rev errorPaths.[snd p.Value]                            
                                
                            if finalPaths |> ResizeArray.exists (fun fp -> isSubpath path (List.rev fp)) |> not
                            then printfn "Position %d rule %d" (getLeft e.Key) (getRight e.Key >>> 16)      *)                      
                    o.Post End     
                    while not !isEnd do()
                    Success (r1)   
                     
                        