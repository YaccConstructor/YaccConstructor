module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.Common.AST3
open Yard.Generators.Common.DataStructures
open Microsoft.FSharp.Collections



[<Measure>] type vertexMeasure
[<Measure>] type nodeMeasure
[<Measure>] type labelMeasure

let inline packExtension left right : int64<extension> =  LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)

let inline packVertex level label: int64<vertexMeasure> =  LanguagePrimitives.Int64WithMeasure ((int64 level <<< 32) ||| int64 label)
let inline getIndex1Vertex (long : int64<vertexMeasure>)       = int <| ((int64 long) &&& 0xffffffffL)
let inline getIndex2Vertex (long : int64<vertexMeasure>)       = int <| ((int64 long) >>> 32)
 

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
    val Index         : int
    val Label         : int<labelMeasure>
    val Vertex        : Vertex
    val Ast           : int<nodeMeasure>
    new (index, label, vertex, ast) = {Index = index; Label = label; Vertex = vertex; Ast = ast}

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of string

let buildAst<'TokenType> (parser : ParserSource2<'TokenType>) (tokens : seq<'TokenType>) : ParseResult<_> = 
    let tokens = Seq.toArray tokens
    let inputLength = Seq.length tokens
    let nonTermsCountLimit = 1 + (Array.max parser.LeftSide)
    let getEpsilon =

         let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i - 1))
         fun i -> epsilons.[i]
    if inputLength = 0 || parser.IndexEOF = parser.TokenToNumber tokens.[0] then
//        if parser.AcceptEmptyInput then
//            Success (new Tree<_>(null, getEpsilon startNonTerm, null))
//        else
        Error ("This grammar does not accept empty input.")     
    else
        let slots = parser.Slots
        let setR = new Queue<Context>()   
        let setP = new Dictionary<Vertex, ResizeArray<int<nodeMeasure>>> ()
        let setU = Array.zeroCreate<Dictionary<int<labelMeasure>, Dictionary<Vertex, ResizeArray<int<nodeMeasure>>>>> (inputLength + 1)

        let currentIndex = ref 0
        let currentrule = parser.StartRule
        let currentLabel = ref <| packLabel currentrule 0
        let dummyGSSNode = new Vertex(!currentIndex, !currentLabel)
        let dummyAST = new TerminalNode(-1, packExtension -1 -1)
        let dummy = 0<nodeMeasure>
        let currentN = ref <| dummy
        let currentR = ref <| dummy

        let resultAST = ref None
        let packedNodes = Array3D.zeroCreate<Dictionary<int<labelMeasure>, int<nodeMeasure>>> (inputLength + 1) (inputLength + 1) (inputLength + 1)
        let symbolNodes = Array3D.zeroCreate<int<nodeMeasure>> parser.NonTermCount (inputLength + 1) (inputLength + 1) 
        let edges = Array2D.zeroCreate<Dictionary<int<nodeMeasure>, Dictionary<int<labelMeasure>, ResizeArray<int>>>> slots.Count inputLength
        let terminalNodes = new BlockResizeArray<int<nodeMeasure>>() // чем по умолчанию заполнен такой массив 
        
        let sppfNodes = new BlockResizeArray<INode>()
        sppfNodes.Add(dummyAST)

        let currentGSSNode = ref <| dummyGSSNode
        let currentContext = ref <| new Context(!currentIndex, !currentLabel, !currentGSSNode, dummy)
        
        let finalExtension = packExtension 0 inputLength

        let containsContext index (label : int<labelMeasure>) vertex (ast : int<nodeMeasure>) =
            if index <= inputLength
            then
                if setU.[index] <> Unchecked.defaultof<Dictionary<int<labelMeasure>, Dictionary<Vertex, ResizeArray<int<nodeMeasure>>>>>
                then 
                    let cond, current = setU.[index].TryGetValue(label) 
                    if  cond then
                       if current.ContainsKey(vertex) then
                            let trees = current.[vertex]
                            if not <| trees.Contains(ast)
                            then 
                                trees.Add(ast)
                                false
                            else
                                true
                        else 
                            let arr = new ResizeArray<int<nodeMeasure>>()
                            arr.Add(ast)
                            current.Add(vertex, arr)                    
                            false
                    else 
                        let dict = new Dictionary<Vertex, ResizeArray<int<nodeMeasure>>>()
                        setU.[index].Add(label, dict)
                        let arr = new ResizeArray<int<nodeMeasure>>()
                        arr.Add(ast)
                        dict.Add(vertex, arr) 
                        false
                else 
                    let dict1 =  new Dictionary<int<labelMeasure>, Dictionary<Vertex, ResizeArray<int<nodeMeasure>>>>()
                    setU.[index] <- dict1
                    let dict2 = new Dictionary<Vertex, ResizeArray<int<nodeMeasure>>>()
                    dict1.Add(label, dict2)
                    let arr = new ResizeArray<int<nodeMeasure>>()
                    arr.Add(ast)
                    dict2.Add(vertex, arr)
                    false
            else true
                      

        let addContext index (label : int<labelMeasure>) vertex ast =
            if not <| containsContext index label vertex ast
            then
                setR.Enqueue(new Context(index, label, vertex, ast))

        let slotIsEnd (label : int<labelMeasure>) =
            (getPosition label) = Array.length (parser.rules.[getRule label])

        let findSppfNode label lExt rExt leftChild rightChild : int<nodeMeasure> =
            let isEnd = slotIsEnd label
            let nTerm = parser.LeftSide.[getRule label]
            if symbolNodes.[nTerm, lExt, rExt] = Unchecked.defaultof<int<nodeMeasure>>
            then
                if isEnd then
                    let newNode = new NonTerminalNode(nTerm, (packExtension lExt rExt))
                    sppfNodes.Add(newNode)
                else
                    let newNode = new IntermidiateNode(int label, (packExtension lExt rExt))
                    sppfNodes.Add(newNode)
                let num = sppfNodes.Count - 1
                symbolNodes.[nTerm, lExt, rExt] <- num*1<nodeMeasure>
                num*1<nodeMeasure>
            else 
                symbolNodes.[nTerm, lExt, rExt]

        let isDummyAST (node : INode) = 
            node.Equals dummyAST

        let findSppfPackedNode symbolNode label leftExtension rightExtension (left : INode) (right : INode) : int<nodeMeasure> = 
            if isDummyAST left 
            then 
                let i = getLeftExtension leftExtension
                let j = getRightExtension leftExtension
                let k = getRightExtension rightExtension
                let rule = getRule label
                if packedNodes.[i, j, k] <> null then
                    let d = packedNodes.[i, j, k]
                    if d.ContainsKey label
                    then
                        d.[label] 
                    else 
                        let newNode = new PackedNode(rule, left, right)
                        sppfNodes.Add(newNode)
                        let num = (sppfNodes.Count - 1 )*1<nodeMeasure>
                        d.Add(label, num)
                        match (sppfNodes.Item (int symbolNode)) with
                        | :? NonTerminalNode as n ->
                            n.AddChild newNode
                        | :? IntermidiateNode as i ->
                            i.AddChild newNode
                        | _ -> ()
                        num
                else
                    let d = new Dictionary<int<labelMeasure>, int<nodeMeasure>>()
                    let newNode = new PackedNode(rule, left, right)
                    sppfNodes.Add(newNode)
                    let num = (sppfNodes.Count - 1 )*1<nodeMeasure>
                    d.Add(label, num)
                    match (sppfNodes.Item (int symbolNode)) with
                    | :? NonTerminalNode as n ->
                        n.AddChild newNode
                    | :? IntermidiateNode as i ->
                        i.AddChild newNode
                    | _ -> ()
                    num                   
            else
                let i = getLeftExtension leftExtension
                let j = getRightExtension leftExtension
                let k = getRightExtension rightExtension
                let rule = getRule label
                if packedNodes.[i, j, k] <> null then
                    let d = packedNodes.[i, j, k]
                    if d.ContainsKey label
                    then
                        d.[label] 
                    else 
                        let newNode = new PackedNode(rule, left, right)
                        sppfNodes.Add(newNode)
                        let num = (sppfNodes.Count - 1 )*1<nodeMeasure>
                        d.Add(label, num)
                        match (sppfNodes.Item (int symbolNode)) with
                        | :? NonTerminalNode as n ->
                            n.AddChild newNode
                        | :? IntermidiateNode as i ->
                            i.AddChild newNode
                        | _ -> ()
                        num
                else
                    let d = new Dictionary<int<labelMeasure>, int<nodeMeasure>>()
                    let newNode = new PackedNode(rule, left, right)
                    sppfNodes.Add(newNode)
                    let num = (sppfNodes.Count - 1 )*1<nodeMeasure>
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
                //y := findSPPFSymbolNode(t, k, i)
                //findSPPFPackNode(y, s, k, (), w)
                    
                let y = findSppfNode label (getLeftExtension leftExt) (getRightExtension rightExt) currentLeft currentRight
                ignore <| findSppfPackedNode y label leftExt rightExt currentLeft currentRight
                y
            else
                let y = findSppfNode label (getLeftExtension rightExt) (getRightExtension rightExt) dummyAST currentRight
                ignore <| findSppfPackedNode y label rightExt rightExt dummyAST currentRight 
                y
            
                
            
        let getNodeT index =
            if terminalNodes.Item index <> Unchecked.defaultof<int<nodeMeasure>>
            then
                terminalNodes.Item index
            else
                let t = new TerminalNode(parser.TokenToNumber tokens.[index], packExtension index (index + 1))
                sppfNodes.Add t
                let res = sppfNodes.Count - 1
                terminalNodes.Set index ((sppfNodes.Count - 1)*1<nodeMeasure>)
                res * 1<nodeMeasure>
                     
        let containsEdge (b : Vertex) (e : Vertex) ast =
            let labelN = slots.[int b.Label]
            let dict1 = edges.[labelN, b.Level]
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
                edges.[labelN, b.Level] <- newDict1
                false                    
        
        let getTreeExtension (node : int<nodeMeasure>) =
            node


        let create index label (vertex : Vertex) ast = 
            let v = new Vertex(index, label)
            let temp = containsEdge v vertex ast
            if not <| temp //containsEdge v vertex ast
            then
                if setP.ContainsKey(v)
                then
                    let arr = setP.[v]
                    for tree in arr do
                        let y = getNodeP label ast tree
                        let index = int <| getTreeExtension y 
                        addContext index label vertex y 
            v

        let pop (u : Vertex) (i : int) (z : int<nodeMeasure>) extension =
            if u <> dummyGSSNode
            then
                if setP.ContainsKey u
                then
                    setP.[u].Add(z)
                else
                    let newList = new ResizeArray<int<nodeMeasure>>()
                    newList.Add(z)
                    setP.Add(u, newList)
                let outEdges = edges.[slots.[ int u.Label], u.Level]
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
                currentIndex := currentContext.Value.Index
                currentGSSNode := currentContext.Value.Vertex
                currentLabel := currentContext.Value.Label
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
               // currentR := getNodeT !currentIndex
                //currentN := getNodeP !currentLabel !currentN !currentR  
                //pop !currentGSSNode !currentIndex !currentN
                printf "temp"
            else
                if Array.length parser.rules.[rule] <> position
                then
                    if !currentIndex < inputLength 
                    then
                        let curToken = parser.TokenToNumber tokens.[!currentIndex]
                        let curSymbol = parser.rules.[rule].[position]
                        if (parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol) //может, здесь нужен отдельный иф для проверки на совпадение текущего символа и токена
                        then
                            if curSymbol = curToken 
                            then
                                if !currentN = dummy
                                then currentN := getNodeT !currentIndex
                                else currentR := getNodeT !currentIndex
                                currentIndex := !currentIndex + 1
                                currentLabel := packLabel (rule) ((position) + 1)
                                if !currentR <> dummy
                                then 
                                    currentN := getNodeP !currentLabel !currentN !currentR
                                condition := false
                        else 
                            let getIndex nTerm term = 
                                let mutable index = nTerm
                                index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                                index <- index + term - parser.NonTermCount
                                index

                            let index = getIndex curSymbol curToken
                            currentGSSNode := create !currentIndex (packLabel (rule) (position + 1)) !currentGSSNode  !currentN
                            if Array.length table.[index] <> 0 
                            then
                                let a rule = 
                                    let newLabel = packLabel rule 0
                                    addContext !currentIndex newLabel !currentGSSNode dummy 
                                table.[index] |> Array.iter a
                    else 
                        condition := true
                                    
                else
                    let curRight =  sppfNodes.Item <| int !currentN 
                    let r =  curRight :?> NonTerminalNode
                    if (r.Name = parser.LeftSide.[parser.StartRule]) && r.Extension = finalExtension
                    then 
                        resultAST := Some r
                    
                    pop !currentGSSNode !currentIndex !currentN r.Extension

        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match !resultAST with
            | None -> Error ("String was not parsed")
            | Some res -> 
                    //drawDot parser tokens "gss.dot" gss
                    let r1 = new Tree<_> (tokens, res, parser.rules)
                    r1.AstToDot parser.NumToString parser.TokenToNumber  parser.LeftSide "ast1111111.dot"
                    Success (r1)   
                        