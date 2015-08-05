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


//[<Measure>] type vertexMeasure
[<Measure>] type nodeMeasure
[<Measure>] type labelMeasure

let inline packExtension left right : int64<extension> =  LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)
 
let inline packLabel rule position = ((int rule <<< 16) ||| int position)*1<labelMeasure>
let inline getRule (packedValue : int<labelMeasure>)  = int packedValue >>> 16
let inline getPosition (packedValue : int<labelMeasure>) = int (int packedValue &&& 0xffff)

[<Struct>]
type Vertex =
    val Level            : int
    val Label            : int<labelMeasure>
    new (level, label) = {Level = level; Label = label}

[<Struct>]
type Context =
    val InputVertex   : int
    val Label         : int<labelMeasure>
    val Vertex        : Vertex
    val Ast           : int<nodeMeasure>
    new (index, label, vertex, ast) = {InputVertex = index; Label = label; Vertex = vertex; Ast = ast}

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of string


let buildAbstractAst<'TokenType> (parser : ParserSourceGLL<'TokenType>) (input : ParserInputGraph<'TokenType>) : ParseResult<_> = 
    
    if input.EdgeCount = 0 then
      //  if parser.AcceptEmptyInput then
      //      let eps = new Nonnte
            //Success (new Tree<_>(null, getEpsilon startNonTerm, null))
     //   else
            Error ("This grammar does not accept empty input.")     
    else
        let slots = parser.Slots
        let setR = new Queue<Context>()   
        let setP = new Dictionary<(int*int<labelMeasure>), ResizeArray<int<nodeMeasure>>> ()
        let setU = Array.zeroCreate<Dictionary<int<labelMeasure>, Dictionary<(int*int<labelMeasure>), ResizeArray<int<nodeMeasure>>>>> (input.VertexCount )///1
        let tempCount = ref 0
        let currentVertexInInput = ref 0
        let currentrule = parser.StartRule
        let currentLabel = ref <| packLabel currentrule 0
        let dummyGSSNode = new Vertex(!currentVertexInInput, !currentLabel)
        let dummyAST = new TerminalNode(-1, packExtension -1 -1)
        let dummy = 0<nodeMeasure>
        let currentN = ref <| dummy
        let currentR = ref <| dummy
        let tokens = new BlockResizeArray<'TokenType>()
        let resultAST = ref None
        let packedNodes = Array3D.zeroCreate<Dictionary<int<labelMeasure>, int<nodeMeasure>>> (input.VertexCount) (input.VertexCount) (input.VertexCount)
        let nonTerminalNodes = Array3D.zeroCreate<int<nodeMeasure>> parser.NonTermCount (input.VertexCount) (input.VertexCount)
        let intermidiateNodes = Array2D.zeroCreate<Dictionary<int<labelMeasure>, int<nodeMeasure>>> (input.VertexCount) (input.VertexCount) //убрала +1
        let edges = Array2D.zeroCreate<Dictionary<int<nodeMeasure>, Dictionary<int<labelMeasure>, ResizeArray<int>>>> slots.Count (input.VertexCount )
        let terminalNodes = Array3D.zeroCreate<int<nodeMeasure>> input.VertexCount input.VertexCount parser.TermCount  
        let epsilonNode = new TerminalNode(-1, packExtension 0 0)
        let sppfNodes = new BlockResizeArray<INode>()
        sppfNodes.Add(dummyAST)
        sppfNodes.Add(epsilonNode)


        let currentGSSNode = ref <| dummyGSSNode
        let currentContext = ref <| new Context(!currentVertexInInput, !currentLabel, !currentGSSNode, dummy)
        
        let finalExtensions =
            let len = input.FinalStates.Length
            let arr = Array.zeroCreate<int64<extension>> len
            for i = 0 to len - 1 do
                arr.[i] <- packExtension input.InitState input.FinalStates.[i]
            arr

        let containsContext (inputVertex : int) (label : int<labelMeasure>) (vertex : Vertex) (ast : int<nodeMeasure>) =
            let index = (int) inputVertex
            //if inputVertex <> input.FinalState 
            //then
            let vertexKey = (vertex.Level, vertex.Label)
            if setU.[index] <> Unchecked.defaultof<Dictionary<int<labelMeasure>, Dictionary<(int*int<labelMeasure>), ResizeArray<int<nodeMeasure>>>>>
            then
                let cond, current = setU.[index].TryGetValue(label) 
                if  cond then
                    if current.ContainsKey(vertexKey) then
                        let trees = current.[vertexKey]
                        if not <| trees.Contains(ast)
                        then 
                            trees.Add(ast)
                            false
                        else
                            true
                    else 
                        let arr = new ResizeArray<int<nodeMeasure>>()
                        arr.Add(ast)
                        current.Add(vertexKey, arr)                    
                        false
                else 
                    let dict = new Dictionary<(int*int<labelMeasure>), ResizeArray<int<nodeMeasure>>>()
                    setU.[index].Add(label, dict)
                    let arr = new ResizeArray<int<nodeMeasure>>()
                    arr.Add(ast)
                    dict.Add(vertexKey, arr) 
                    false
            else 
                let dict1 =  new Dictionary<int<labelMeasure>, Dictionary<(int*int<labelMeasure>), ResizeArray<int<nodeMeasure>>>>()
                setU.[index] <- dict1
                let dict2 = new Dictionary<(int*int<labelMeasure>), ResizeArray<int<nodeMeasure>>>()
                dict1.Add(label, dict2)
                let arr = new ResizeArray<int<nodeMeasure>>()
                arr.Add(ast)
                dict2.Add(vertexKey, arr)
                false
            //else true
                      

        let addContext (inputVertex : int) (label : int<labelMeasure>) vertex ast =
            if not <| containsContext inputVertex label vertex ast
            then
                setR.Enqueue(new Context(inputVertex, label, vertex, ast))
                tempCount := !tempCount + 1

        let slotIsEnd (label : int<labelMeasure>) =
            (getPosition label) = Array.length (parser.rules.[getRule label])

        let findSppfNode (label : int<labelMeasure>) lExt rExt : int<nodeMeasure> =
            let isEnd = slotIsEnd label
            let lExt = int lExt
            let rExt = int rExt
            let nTerm = parser.LeftSide.[getRule label]
            
            if isEnd
            then
                if nonTerminalNodes.[nTerm, lExt, rExt] = Unchecked.defaultof<int<nodeMeasure>>
                then
                    let newNode = new NonTerminalNode(nTerm, (packExtension lExt rExt))
                    sppfNodes.Add(newNode)
                    let num = sppfNodes.Length - 1
                    nonTerminalNodes.[nTerm, lExt, rExt] <- num*1<nodeMeasure>
                    num*1<nodeMeasure>
                else
                    nonTerminalNodes.[nTerm, lExt, rExt]
            else
                if intermidiateNodes.[lExt, rExt] = Unchecked.defaultof<Dictionary<int<labelMeasure>, int<nodeMeasure>>>
                then
                    let d = new Dictionary<int<labelMeasure>, int<nodeMeasure>>()
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
            let d = 
                if packedNodes.[i, j, k] <> null then
                    packedNodes.[i, j, k]
                else 
                    let t = new Dictionary<int<labelMeasure>, int<nodeMeasure>>()
                    packedNodes.[i, j, k] <- t
                    t
            if d.ContainsKey label
            then
                d.[label] 
            else 
                let newNode = new PackedNode(rule, left, right)
                sppfNodes.Add(newNode)
                let num = (sppfNodes.Length - 1 )*1<nodeMeasure>
                d.Add(label, num)
                match (sppfNodes.Item (int symbolNode)) with
                | :? NonTerminalNode as n ->
                    n.AddChild newNode
                | :? IntermidiateNode as i ->
                    i.AddChild newNode
                | _ -> ()
                num
                
        let getNodeP (label : int<labelMeasure>) (left : int<nodeMeasure>) (right : int<nodeMeasure>) : int<nodeMeasure> =
            let currentRight = sppfNodes.Item (int right)
            let rightExt = 
                match currentRight with                    
                    | :? NonTerminalNode as nonTerm ->
                        nonTerm.Extension
                    | :? IntermidiateNode as interm ->
                        interm.Extension
                    | :? TerminalNode as term ->
                        term.Extension   
                    | _ -> failwith "Smth strange, Nastya"             
            if left <> dummy
            then
                let currentLeft = sppfNodes.Item (int left)
                let leftExt =
                    match currentLeft with                    
                    | :? NonTerminalNode as nonTerm ->
                        nonTerm.Extension
                    | :? IntermidiateNode as interm ->
                        interm.Extension
                    | :? TerminalNode as term ->
                        term.Extension 
                    | _ -> failwith "Smth strange, Nastya" 
                    
                let y = findSppfNode label (getLeftExtension leftExt) (getRightExtension rightExt)
                ignore <| findSppfPackedNode y label leftExt rightExt currentLeft currentRight
                y
            else
                let y = findSppfNode label (getLeftExtension rightExt) (getRightExtension rightExt)
                ignore <| findSppfPackedNode y label rightExt rightExt dummyAST currentRight 
                y
            
                
            
        let getNodeT (edge : ParserEdge<'TokenType>) =
            let beginVertix = edge.Source
            let endVertix = edge.Target
            let tag = edge.Tag
            let i = (parser.TokenToNumber tag) - parser.NonTermCount
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
            let labelN = slots.[int b.Label]
            let beginLevel = int b.Level
            let endLevel = int e.Level
            let dict1 = edges.[labelN, beginLevel]
            if dict1 <> Unchecked.defaultof<Dictionary<int<nodeMeasure>, Dictionary<int<labelMeasure>, ResizeArray<int>>>>
            then
                if dict1.ContainsKey(ast)
                then
                    let dict2 = dict1.[ast]
                    if dict2.ContainsKey(e.Label)
                    then
                        let t = dict2.[e.Label]
                        if t.Contains(e.Level) then true
                        else 
                            t.Add(e.Level) 
                            false
                    else
                        let arr = new ResizeArray<int>()
                        arr.Add(e.Level) 
                        dict2.Add(e.Label, arr)
                        false
                else
                    let d = new Dictionary<int<labelMeasure>, ResizeArray<int>>()
                    dict1.Add(ast, d)
                    let l = new ResizeArray<int>()
                    l.Add(e.Level)
                    d.Add(e.Label, l)
                    false
            else
                let newDict1 = new Dictionary<int<nodeMeasure>, Dictionary<int<labelMeasure>, ResizeArray<int>>>()
                let newDict2 = new Dictionary<int<labelMeasure>, ResizeArray<int>>()
                let newArr = new ResizeArray<int>()
                newArr.Add(e.Level)
                newDict2.Add(e.Label, newArr)
                newDict1.Add(ast, newDict2)
                edges.[labelN, beginLevel] <- newDict1
                false                    
        
        let getTreeExtension (node : int<nodeMeasure>) =
            match sppfNodes.Item (int node) with
                | :? TerminalNode as t ->
                    t.Extension
                | :? IntermidiateNode as i ->
                    i.Extension
                | :? NonTerminalNode as n ->
                    n.Extension
                | _ -> failwith "Bad type for tree node"

        
        let create (inputVertex : int) (label : int<labelMeasure>) (vertex : Vertex) (ast : int<nodeMeasure>) = 
            let v = new Vertex(inputVertex, label)
            let vertexKey = (inputVertex, label)
            let temp = containsEdge v vertex ast
            if not <| temp //containsEdge v vertex ast
            then
                if setP.ContainsKey(vertexKey)
                then
                    let arr = setP.[vertexKey]
                    for tree in arr do
                        let y = getNodeP label ast tree
                        let index = getRightExtension <| getTreeExtension y 
                        addContext index label vertex y 
            v

        let pop (u : Vertex) (i : int) (z : int<nodeMeasure>) =
            if u <> dummyGSSNode
            then
                let vertexKey = (u.Level, u.Label)
                if setP.ContainsKey vertexKey
                then
                    setP.[vertexKey].Add(z)
                else
                    let newList = new ResizeArray<int<nodeMeasure>>()
                    newList.Add(z)
                    setP.Add(vertexKey, newList)
                let outEdges = edges.[slots.[int u.Label], u.Level]
                for edge in outEdges do
                    let sppfNodeOnEdge = edge.Key
                    for slotLevels in edge.Value do   
                         let slot = slotLevels.Key
                         for level in slotLevels.Value do
                            let resTree = getNodeP u.Label sppfNodeOnEdge z 
                            let newVertex = new Vertex(level, slot)
                            addContext i u.Label newVertex resTree

        let table = parser.Table
        
        let condition = ref false 
        let stop = ref false

        let rec dispatcher () =
            if setR.Count <> 0
            then
                currentContext := setR.Dequeue()
                
                currentVertexInInput := currentContext.Value.InputVertex
                currentGSSNode := currentContext.Value.Vertex
                currentLabel := currentContext.Value.Label
                let tempRule = getRule !currentLabel
                let tempPos = getPosition !currentLabel
                currentN := currentContext.Value.Ast 
                currentR := dummy
                condition := false
            else 
                stop := true  
                              
        and processing () =  
            condition := true
            let rule = getRule !currentLabel
            let position = getPosition !currentLabel
            if Array.length parser.rules.[rule] = 0 
            then
              let t = new TerminalNode(-1, packExtension !currentVertexInInput !currentVertexInInput)
              sppfNodes.Add t
              let res = sppfNodes.Length - 1
              currentR := res * 1<nodeMeasure>
              currentN := getNodeP !currentLabel !currentN !currentR  
              pop !currentGSSNode !currentVertexInInput !currentN 
            else
                if Array.length parser.rules.[rule] <> position
                then
                    let curSymbol = parser.rules.[rule].[position]
                   // if !currentVertexInInput <> input.FinalState
                   // then
                    if (parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol)
                    then
                        let isEq (sym : int) (elem : ParserEdge<'TokenType>) = sym = parser.TokenToNumber elem.Tag
                        let curEdge = Seq.tryFind (isEq curSymbol) (input.OutEdges !currentVertexInInput)
                        match curEdge with
                        | Some edge ->
                            let curToken = parser.TokenToNumber edge.Tag
                            if curSymbol = curToken 
                            then
                                if !currentN = dummy
                                then currentN := getNodeT edge
                                else currentR := getNodeT edge
                                currentVertexInInput := edge.Target
                                currentLabel := packLabel (rule) ((position) + 1)
                                if !currentR <> dummy
                                then 
                                    currentN := getNodeP !currentLabel !currentN !currentR
                                condition := false
                        | None _ -> ()
                    else 
                        let getIndex nTerm term = 
                            let mutable index = nTerm
                            index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                            index <- index + term - parser.NonTermCount
                            index
                        currentGSSNode := create !currentVertexInInput (packLabel (rule) (position + 1)) !currentGSSNode  !currentN
                        for edge in input.OutEdges !currentVertexInInput do
                            let curToken = parser.TokenToNumber edge.Tag

                            let index = getIndex curSymbol curToken
                                
                            if Array.length table.[index] <> 0 
                            then
                                let a rule = 
                                    let newLabel = packLabel rule 0
                                    addContext !currentVertexInInput newLabel !currentGSSNode dummy 
                                table.[index] |>  Array.iter a
//                    else
//                        if Seq.length <| input.OutEdges !currentVertexInInput = 0
//                        then
//                            if parser.CanInferEpsilon.[curSymbol]
//                            then
//                                let curToken = parser.IndexEOF
//                                let getIndex nTerm term = 
//                                    let mutable index = nTerm
//                                    index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
//                                    index <- index + term - parser.NonTermCount
//                                    index
//                                let index = getIndex curSymbol curToken
//                                currentGSSNode := create !currentVertexInInput (packLabel (rule) (position + 1)) !currentGSSNode  !currentN
//                                if Array.length table.[index] <> 0 
//                                then
//                                    let a rule = 
//                                        let newLabel = packLabel rule 0
//                                        addContext !currentVertexInInput newLabel !currentGSSNode dummy 
//                                    table.[index] |>  Array.iter a
//                            condition := true
                                    
                else
                    let curRight =  sppfNodes.Item (int !currentN) 
                    match curRight with
                        | :? TerminalNode as t ->
                            currentN := getNodeP !currentLabel !currentR !currentN
                            let r = (sppfNodes.Item (int !currentN)) :?> NonTerminalNode 
                            pop !currentGSSNode !currentVertexInInput !currentN
                        | :? NonTerminalNode as r ->
                            if (r.Name = parser.LeftSide.[parser.StartRule]) && (Array.exists (fun a -> a = r.Extension) finalExtensions)
                            then 
                                match !resultAST with
                                    | None -> resultAST := Some r
                                    | Some a -> a.AddChild r.First
                            pop !currentGSSNode !currentVertexInInput !currentN

        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match !resultAST with
            | None -> Error ("String was not parsed")
            | Some res -> 
                    let r1 = new Tree<_> (tokens.ToArray(), res, parser.rules)
                    r1.AstToDot parser.NumToString parser.TokenToNumber parser.TokenData "AST123456.dot"
                    printfn "%d" !tempCount
                    Success (r1)   
                     
                        