module Yard.Generators.GLL.AbstractParser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.DataStructures
open Microsoft.FSharp.Collections
open AbstractAnalysis.Common
open FSharpx.Collections.Experimental
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns

[<Struct>]
type M =
    val pos : int64
    val lbl : int<labelMeasure>
    new (p,l) = {pos = p; lbl = l}

let buildAbstractAst<'TorenType> (parser : ParserSourceGLL<'TorenType>) (input : ParserInputGraph) : ParserCommon.ParseResult<int> = 
    
    if input.EdgeCount = 0 then
      //  if parser.AcceptEmptyInput then
      //      let eps = new Nonnte
            //Success (new Tree<_>(null, getEpsilon startNonTerm, null))
     //   else
            Error ("This grammar does not accept empty input.")     
    else
        let slots = parser.Slots
           
        let setU = Array.zeroCreate<Dictionary<_, Dictionary<_, ResizeArray<_>>>> input.VertexCount
        let structures = new ParserStructures<_>(parser.StartRule)
        let setR = structures.SetR
        let epsilonNode = structures.EpsilonNode
        let setP = structures.SetP
        let tempCount = ref 0
        let currentVertexInInput = ref 0
        let currentrule = parser.StartRule
         //packLabel without int
        let dummyGSSNode = new Vertex(!currentVertexInInput, int !structures.CurrentLabel)
        let sppfNodes = structures.SppfNodes
        
        let tokens = new BlockResizeArray<'TokenType>()        
        let packedNodes = new Dictionary<M, int<nodeMeasure>>()
        
        let nonTerminalNodes = new Dictionary<int64,int<nodeMeasure>>()        
        let intermidiateNodes = Array2D.zeroCreate<Dictionary<int<labelMeasure>, int<nodeMeasure>>> (input.VertexCount) (input.VertexCount) //убрала +1
        let edges = Array2D.zeroCreate<Dictionary<int<nodeMeasure>, Dictionary<int, ResizeArray<int>>>> slots.Count (input.VertexCount )
        let terminalNodes = Array3D.zeroCreate<int<nodeMeasure>> input.VertexCount input.VertexCount parser.TermCount  
        let currentGSSNode = ref <| dummyGSSNode
        let currentContext = ref <| new Context(!currentVertexInInput, !structures.CurrentLabel, !currentGSSNode, structures.Dummy) //without *1<labelMeasure>
        
        let finalExtensions =
            let len = input.FinalStates.Length
            let arr = Array.zeroCreate<int64<extension>> len
            for i = 0 to len - 1 do
                arr.[i] <- packExtension input.InitStates.[0] input.FinalStates.[i]
            arr

        let slotIsEnd (label : int<labelMeasure>) =
            (getPositionNew label) = Array.length (parser.rules.[getRule label])

        let findSppfNode (label : int<labelMeasure>) lExt rExt : int<nodeMeasure> =
            let isEnd = slotIsEnd <| label
            let lExt = int lExt
            let rExt = int rExt
            let nTerm = parser.LeftSide.[getRule label]
            
            if isEnd
            then
                if not <| nonTerminalNodes.ContainsKey(pack3 nTerm lExt rExt)
                then
                    let newNode = new NonTerminalNode(nTerm, (packExtension lExt rExt))
                    sppfNodes.Add(newNode)
                    let num = sppfNodes.Length - 1
                    nonTerminalNodes.Add((pack3 nTerm lExt rExt), num*1<nodeMeasure>)
                    num*1<nodeMeasure>
                else
                    nonTerminalNodes.[pack3 nTerm lExt rExt]
            else
                if intermidiateNodes.[lExt, rExt] = Unchecked.defaultof<Dictionary<int<labelMeasure>, int<nodeMeasure>>>
                then
                    let d = new Dictionary<int<labelMeasure>, int<nodeMeasure>>(2)
                    let newNode = new IntermidiateNode(int label, (packExtension lExt rExt))
                    sppfNodes.Add(newNode)
                    let num = (sppfNodes.Length - 1)*1<nodeMeasure>
                    d.Add(label, num)
                    intermidiateNodes.[lExt, rExt] <- d 
                    num
                else
                    let dict = intermidiateNodes.[lExt, rExt] 
                    if dict.ContainsKey label
                    then
                        dict.[label]
                    else
                        let newNode = new IntermidiateNode(int label, (packExtension lExt rExt))
                        sppfNodes.Add(newNode)
                        let num = (sppfNodes.Length - 1)*1<nodeMeasure>
                        dict.Add(label, num)
                        num

        let findSppfPackedNode (symbolNode : int<nodeMeasure>) (label : int<labelMeasure>) lExt rExt (left : INode) (right : INode) : int<nodeMeasure> = 
            let i = getLeftExtension lExt
            let j = getRightExtension lExt
            let k = getRightExtension rExt
            let rule = getRule label            
            let key = new M (pack3 i j k, label)
            let flg,res = packedNodes.TryGetValue(key)            
            if flg
            then res
            else
                let newNode = new PackedNode(rule, left, right)
                sppfNodes.Add(newNode)
                let num = (sppfNodes.Length - 1 )*1<nodeMeasure>
                packedNodes.Add(key, num)
                match sppfNodes.Item (int symbolNode) with
                | :? NonTerminalNode as n ->
                    n.AddChild newNode
                | :? IntermidiateNode as i ->
                    i.AddChild newNode
                | _ -> ()
                num
                  
        let getNodeT (edge : ParserEdge<int>) =
            let beginVertix = edge.Source
            let endVertix = edge.Target
            let tag = edge.Tag
            let i = tag - parser.NonTermCount
            if terminalNodes.[beginVertix, endVertix, i] <> Unchecked.defaultof<int<nodeMeasure>>
            then
                terminalNodes.[beginVertix, endVertix, i]
            else
                tokens.Add tag
                let t = new TerminalNode(tokens.Length - 1, packExtension beginVertix endVertix)
                sppfNodes.Add t
                let res = sppfNodes.Length - 1
                terminalNodes.[beginVertix, endVertix, i] <- ((sppfNodes.Length - 1)*1<nodeMeasure>)
                res * 1<nodeMeasure>
            
                     
        let containsEdge (b : Vertex) (e : Vertex) ast =
            let labelN = slots.[int b.NontermLabel]
            let beginLevel = int b.Level
            let endLevel = int e.Level
            let dict1 = edges.[labelN, beginLevel]
            let cond, dict = structures.ContainsEdge dict1 ast e
            if dict.IsSome then edges.[labelN, beginLevel] <- dict.Value
            cond
        
        
        let create (inputVertex : int) (label : int<labelMeasure>) (vertex : Vertex) (ast : int<nodeMeasure>) = 
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
                        let index = getRightExtension <| structures.GetTreeExtension y 
                        structures.AddContext setU index label vertex y )
            v
                
        let pop (u : Vertex) (i : int) (z : int<nodeMeasure>) =
            if u <> dummyGSSNode
            then
                let vertexKey = pack u.Level (int u.NontermLabel)
                if setP.ContainsKey vertexKey
                then
                    setP.[vertexKey].Add z
                else
                    let newList = new ResizableUsualOne<_>(z) //new ResizeArray<int<nodeMeasure>>([|z|])
                    //newList.Add(z)
                    setP.Add(vertexKey, newList)
                let outEdges = edges.[slots.[int u.NontermLabel], u.Level]
                for edge in outEdges do
                    let sppfNodeOnEdge = edge.Key
                    for slotLevels in edge.Value do   
                         let slot = slotLevels.Key
                         for level in slotLevels.Value do
                            let resTree = structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy (u.NontermLabel*1<labelMeasure>) sppfNodeOnEdge z 
                            let newVertex = new Vertex(level, slot)
                            structures.AddContext setU i (u.NontermLabel*1<labelMeasure>) newVertex resTree

        let table = parser.Table
        
        let condition = ref false 
        let stop = ref false

        let rec dispatcher () =
            if setR.Count <> 0
            then
                currentContext := setR.Dequeue()
                currentVertexInInput := currentContext.Value.Index
                currentGSSNode := currentContext.Value.Vertex
                structures.CurrentLabel := currentContext.Value.Label
                structures.CurrentN := currentContext.Value.Ast 
                structures.CurrentR := structures.Dummy
                condition := false
            else 
                stop := true  
                              
        and processing () =  
            condition := true
            let rule = getRule !structures.CurrentLabel
            let position = getPositionNew !structures.CurrentLabel
            if Array.length parser.rules.[rule] = 0 
            then
              let t = new TerminalNode(-1, packExtension !currentVertexInInput !currentVertexInInput)
              sppfNodes.Add t
              let res = sppfNodes.Length - 1
              structures.CurrentR := res * 1<nodeMeasure>
              structures.CurrentN := structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy !structures.CurrentLabel !structures.CurrentN !structures.CurrentR  
              pop !currentGSSNode !currentVertexInInput !structures.CurrentN 
            else
                if Array.length parser.rules.[rule] <> position
                then
                    let curSymbol = parser.rules.[rule].[position]
                   // if !currentVertexInInput <> input.FinalState
                   // then
                    if parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol
                    then
                        let isEq (sym : int) (elem : ParserEdge<int>) = sym = elem.Tag
                        let curEdge = Seq.tryFind (isEq curSymbol) (input.OutEdges !currentVertexInInput)
                        match curEdge with
                        | Some edge ->
                            let curToken = edge.Tag
                            if curSymbol = curToken 
                            then
                                if !structures.CurrentN = structures.Dummy
                                then 
                                    structures.CurrentN := getNodeT edge
                                    
                                else 
                                    structures.CurrentR := getNodeT edge
                                currentVertexInInput := edge.Target
                                structures.CurrentLabel := 1<labelMeasure> * (packLabelNew rule (position + 1))
                                if !structures.CurrentR <> structures.Dummy
                                then 
                                    structures.CurrentN := structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy !structures.CurrentLabel !structures.CurrentN !structures.CurrentR
                                condition := false
                        | None _ -> ()
                    else 
                        let getIndex nTerm term = 
                            let mutable index = nTerm
                            index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                            index <- index + term - parser.NonTermCount
                            index
                        currentGSSNode := create !currentVertexInInput (1<labelMeasure> *(packLabelNew rule (position + 1))) !currentGSSNode  !structures.CurrentN
                        for edge in input.OutEdges !currentVertexInInput do
                            let curToken = edge.Tag
                            let index = getIndex curSymbol curToken
                            let key =  int((int32 curSymbol <<< 16) ||| int32 (curToken - parser.NonTermCount  ))    
                            if table.ContainsKey key
                            then
                                for rule in table.[key] do
                                 
                                    let newLabel = 1<labelMeasure> * (packLabelNew rule 0)
                                    structures.AddContext setU !currentVertexInInput newLabel !currentGSSNode structures.Dummy 
                                    
                else
                    let curRight =  sppfNodes.Item (int !structures.CurrentN) 
                    structures.FinalMatching
                        curRight 
                        parser.LeftSide.[parser.StartRule]
                        finalExtensions
                        findSppfNode
                        findSppfPackedNode
                        currentGSSNode
                        currentVertexInInput
                        pop


        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()

        let rec checkConj (ast : obj) : bool = 
            match ast with
            | :? NonTerminalNode as node -> let buff = ref <| checkConj node.First
                                            if node.Others = Unchecked.defaultof<_> then //others empty
                                                !buff && not <| (parser.NumToString node.Name).StartsWith "yard_conjunction"
                                            else 
                                                node.Others.ForEach(fun n -> buff := !buff && checkConj n)
                                                !buff 
            | :? TerminalNode as node -> true
            | :? PackedNode as node -> checkConj node.Left && checkConj node.Right
            | :? IntermidiateNode as node -> let buff = ref <| checkConj node.First
                                             if node.Others = Unchecked.defaultof<_> then //others empty
                                                !buff
                                             else
                                                node.Others.ForEach(fun n -> buff := !buff && checkConj n)
                                                !buff
            | _ -> false
                 
        match !structures.ResultAST with
            | None -> Error ("String was not parsed")
            | Some res -> 
                    if checkConj res then        
                            let r1 = new Tree (tokens.ToArray(), res, parser.rules)
                            printf "%A" r1
                            //setU |> Seq.iter(fun x -> x |> Seq.iter (fun x -> printf "%A; " x.Value.Count))
                            //r1.AstToDot parser.NumToString "AST123456.dot"
                            let t = r1.GetPath 1
                    //printfn "%d" !tempCount
                            ParseResult<'TokenType>.Success(r1)   
                    else Error ("String was not parsed")