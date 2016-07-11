module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.DataStructures
open Microsoft.FSharp.Collections
open FSharpx.Collections.Experimental

[<Measure>] type vertexMeasure
[<Measure>] type nodeMeasure
[<Measure>] type labelMeasure

let inline pack left right : int64 =  ((int64 left <<< 32) ||| int64 right)
let inline getRight (long : int64) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeft (long : int64)  = int <| ((int64 long) >>> 32)

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

[<Struct>]
type LblNodePair =
    val lbl: int<labelMeasure>
    val node: int<nodeMeasure>
    new (l,n) = {lbl=l; node=n}

let buildAst<'TokenType> (parser : ParserSourceGLL<'TokenType>) (tokens : seq<'TokenType>) : ParseResult<_>  = 
    let tokens = seq {yield! tokens; yield parser.EOF}
    let tokens = Seq.toArray tokens
    let inputLength = Seq.length tokens
    let nonTermsCountLimit = 1 + (Array.max parser.LeftSide)

//    if inputLength = 0 || parser.IndexEOF = parser.TokenToNumber tokens.[0] then
//        if parser.AcceptEmptyInput then
//            let eps = new NonTerminalNode
//            Success (new Tree<_>(null, getEpsilon startNonTerm, null))
//        else
//            Error ("This grammar does not accept empty input.")     
//    else
    let slots = parser.Slots
    let setR = new Queue<Context>()   
    let setP = new Dictionary<int64, ResizeArray<int<nodeMeasure>>> ()
    //свернуть в 1 инт
    let setU = Array.zeroCreate<IntDictionary<Dictionary<int64<vertexMeasure>, ResizeArray<int<nodeMeasure>>>>> (inputLength + 1)

    let currentIndex = ref 0
    let currentrule = parser.StartRule
    let currentLabel = ref <| packLabel currentrule 0
    let dummyGSSNode = new Vertex(!currentIndex, !currentLabel)
    let dummyAST = new TerminalNode(-1, packExtension -1 -1)
    let dummy = 0<nodeMeasure>
    let currentN = ref <| dummy
    let currentR = ref <| dummy

    let resultAST = ref None
    let packedNodesReadCount = ref 0
    let packedNodesWriteCount = ref 0
    let packedNodesX = Array.zeroCreate<int> (inputLength + 1)
    let packedNodesY = Array.zeroCreate<int> (inputLength + 1)
    let packedNodesZ = Array.zeroCreate<int> (inputLength + 1)
    //let packedNodes = Array.zeroCreate<IntDictionary<IntDictionary<int>>> (inputLength + 1)
    let packedNodes = new Dictionary<int, int<nodeMeasure>>()
        //Array.zeroCreate<IntDictionary<IntDictionary<ResizableUsualOne<LblNodePair>>>> (inputLength + 1)
    let inline f x y z = x * (inputLength + 1) * (inputLength + 1) + y * (inputLength + 1) + z
    let inline f4 x y z w = x * parser.Slots.Count * (inputLength + 1) * (inputLength + 1) + y * (inputLength + 1) * (inputLength + 1) + z * (inputLength + 1) + w
    let nonTerminalNodesReadCount = ref 0
    let nonTerminalNodesWriteCount = ref 0 
    let nonterminalNodesX = Array.zeroCreate parser.NonTermCount
    let nonterminalNodesY = Array.zeroCreate (inputLength + 1)
    let nonterminalNodesZ = Array.zeroCreate (inputLength + 1)        
    let nonTerminalNodes = new Dictionary<int, int<nodeMeasure>>()
    //let nonTerminalNodes = Array3D.zeroCreate<int<nodeMeasure>> parser.NonTermCount (inputLength + 1) (inputLength + 1)


    let intermidiateNodesReadCount = ref 0
    let intermidiateNodesWriteCount = ref 0
    //we can use dictionary <extension, dict>
    let intermidiateNodes = new Dictionary<int, int<nodeMeasure>>()
 
//посчитать размерв коллекций
    let edgesReadCount = ref 0
    let edgesWriteCount = ref 0
    let edges = Array2D.zeroCreate<Dictionary<int64, ResizeArray<int>>> slots.Count (inputLength + 1)
        
    let terminalNodes = new BlockResizeArray<int<nodeMeasure>>()
    let epsilonNode = new TerminalNode(-1, packExtension 0 0)
    let sppfNodes = new BlockResizeArray<INode>()
    sppfNodes.Add(dummyAST)
    sppfNodes.Add(epsilonNode)


    let currentGSSNode = ref <| dummyGSSNode
    let currentContext = ref <| new Context(!currentIndex, !currentLabel, !currentGSSNode, dummy)
        
    let finalExtension = packExtension 0 (inputLength)

    let inline addA (a : ResizableUsualFive<_>[,]) i j x =
        if a.[i, j].first = Unchecked.defaultof<_> 
        then a.[i, j].first <- x
            elif a.[i, j].second = Unchecked.defaultof<_>
            then a.[i, j].second <- x
                elif a.[i, j].third = Unchecked.defaultof<_>
                then a.[i, j].third <- x
                    elif a.[i, j].fourth = Unchecked.defaultof<_>
                    then a.[i, j].fourth <- x
                        elif a.[i, j].fifth = Unchecked.defaultof<_>
                        then a.[i, j].fifth <- x
                        else a.[i, j].other <- x :: a.[i, j].other

    let inline addD (d:System.Collections.Generic.Dictionary<int,ResizableUsualFive<_>>) i x =
        let mutable a = d.[i]
        if a.first = Unchecked.defaultof<_> 
        then a.first <- x
            elif a.second = Unchecked.defaultof<_>
            then a.second <- x
                elif a.third = Unchecked.defaultof<_>
                then a.third <- x
                    elif a.fourth = Unchecked.defaultof<_>
                    then a.fourth <- x
                        elif a.fifth = Unchecked.defaultof<_>
                        then a.fifth <- x
                        else a.other <- x :: a.other
        d.[i] <- a

    let containsContext index (label : int<labelMeasure>) (vertex : Vertex) (ast : int<nodeMeasure>) =
        if index <= inputLength
        then
            let vertexKey = packVertex vertex.Level vertex.Label
            if setU.[index] <> Unchecked.defaultof<_>
            then
                let cond, current = setU.[index].TryGetValue(int label) 
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
                    let dict = new Dictionary<int64<vertexMeasure>, ResizeArray<int<nodeMeasure>>>()
                    setU.[index].Add(int label, dict)
                    let arr = new ResizeArray<int<nodeMeasure>>()
                    arr.Add(ast)
                    dict.Add(vertexKey, arr) 
                    false
            else 
                let dict1 =  new IntDictionary<_>()
                setU.[index] <- dict1
                let dict2 = new Dictionary<int64<vertexMeasure>, ResizeArray<int<nodeMeasure>>>()
                dict1.Add(int label, dict2)
                let arr = new ResizeArray<int<nodeMeasure>>()
                arr.Add(ast)
                dict2.Add(vertexKey, arr)
                false
        else true
                      

    let inline addContext index (label : int<labelMeasure>) vertex ast =
        if not <| containsContext index label vertex ast
        then
            setR.Enqueue(new Context(index, label, vertex, ast))

    let inline slotIsEnd (label : int<labelMeasure>) =
        (getPosition label) = Array.length (parser.rules.[getRule label])

    let findSppfNode label ext : int<nodeMeasure> =
        let isEnd = slotIsEnd label
        let nTerm = parser.LeftSide.[getRule label]
        let lExt = getLeftExtension ext
        let rExt = getRightExtension ext
        if isEnd
        then
            let key = f lExt rExt nTerm
            let contains, n = nonTerminalNodes.TryGetValue(key)
            if not contains
            then
                let newNode = new NonTerminalNode(nTerm, ext)
                sppfNodes.Add(newNode)
                let num = (sppfNodes.Length - 1)*1<nodeMeasure>
                nonTerminalNodes.Add(key, num)
                num
            else n
        else  
            let key = f lExt rExt (int label)
            let contains, n = intermidiateNodes.TryGetValue key
            if not contains
            then
                let newNode = new IntermidiateNode(int label, ext)
                sppfNodes.Add(newNode)
                let num = (sppfNodes.Length - 1)*1<nodeMeasure>
                intermidiateNodes.Add(key, num)
                num  
            else n

    let findSppfPackedNode symbolNode label leftExtension rightExtension (left : INode) (right : INode) : int<nodeMeasure> = 
        let rule = getRule label
        let createNode () =
            let newNode = new PackedNode(rule, left, right)
            sppfNodes.Add(newNode)
            let num = (sppfNodes.Length - 1 )*1<nodeMeasure>
            match (sppfNodes.Item (int symbolNode)) with
            | :? NonTerminalNode as n ->
                n.AddChild newNode
            | :? IntermidiateNode as i ->
                i.AddChild newNode
            | _ -> ()
            num

        let i = getLeftExtension leftExtension
        let j = getRightExtension leftExtension
        let k = getRightExtension rightExtension
        let key = f4 i j k (int label)
        //Array.zeroCreate<IntDictionary<IntDictionary<int>>> (inputLength + 1)            
        let contains, d1 = packedNodes.TryGetValue key
        if contains then d1
        else
            let newNode = createNode()
            packedNodes.Add(key, newNode)
            newNode
                
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
                    
            let y = findSppfNode label (packExtension (getLeftExtension leftExt) (getRightExtension rightExt))
            ignore <| findSppfPackedNode y label leftExt rightExt currentLeft currentRight
            y
        else
            let y = findSppfNode label (packExtension (getLeftExtension rightExt) (getRightExtension rightExt))
            ignore <| findSppfPackedNode y label rightExt rightExt dummyAST currentRight 
            y
            
                
            
    let getNodeT index =
        if terminalNodes.Item index <> Unchecked.defaultof<int<nodeMeasure>>
        then
            terminalNodes.Item index
        else
            let t = new TerminalNode(index, packExtension index (index + 1))
            sppfNodes.Add t
            let res = sppfNodes.Length - 1
            terminalNodes.[index] <- ((sppfNodes.Length - 1)*1<nodeMeasure>)
            res * 1<nodeMeasure>
            
                     
    let containsEdge (b : Vertex) (e : Vertex) (ast : int<nodeMeasure>) =
        let labelN = slots.[int b.Label]
        edgesReadCount := !edgesReadCount + 1
        let dict1 = edges.[labelN, b.Level]
        let key = pack ast (int e.Label)
        if dict1 <> Unchecked.defaultof<_>
        then
            let contains, t = dict1.TryGetValue(key)
            if contains
            then
                if t.Contains(e.Level) then true
                else 
                    t.Add(e.Level) 
                    false
            else
                let arr = new ResizeArray<int>()
                arr.Add(e.Level) 
                dict1.Add(key, arr)
                false
        else
            let newDict = new Dictionary<int64,ResizeArray<int>>()
            let newArr = new ResizeArray<int>()
            newArr.Add(e.Level)
            newDict.Add(key, newArr)
            edgesWriteCount := !edgesWriteCount + 1
            edges.[labelN, b.Level] <- newDict
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

        
    let create index label (vertex : Vertex) (ast : int<nodeMeasure>) = 
        let v = new Vertex(index, label)
        let vertexKey = pack index (int label)
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
            let vertexKey = pack u.Level (int u.Label)
            if setP.ContainsKey vertexKey
            then
                setP.[vertexKey].Add(z)
            else
                let newList = new ResizeArray<int<nodeMeasure>>()
                newList.Add(z)
                setP.Add(vertexKey, newList)
            let outEdges = edges.[slots.[ int u.Label], u.Level]
            edgesReadCount := !edgesReadCount + 1
            for edge in outEdges do
                let sppfNodeOnEdge = (getLeft edge.Key) * 1<nodeMeasure>
                let slot = (getRight edge.Key) * 1<labelMeasure>
                let arr = edge.Value
                for level in arr do
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
            let t = new TerminalNode(-1, packExtension !currentIndex !currentIndex)
            sppfNodes.Add t
            let res = sppfNodes.Length - 1
            currentR := res * 1<nodeMeasure>
            currentN := getNodeP !currentLabel !currentN !currentR  
            pop !currentGSSNode !currentIndex !currentN
        else
            if Array.length parser.rules.[rule] <> position
            then
                let curSymbol = parser.rules.[rule].[position]
                if !currentIndex < inputLength 
                then
                    let curToken = parser.TokenToNumber tokens.[!currentIndex]
                        
                    if (parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol)
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
                            table.[index] |>  Array.iter a
                else
                    if parser.CanInferEpsilon.[curSymbol]
                    then
                        let curToken = parser.IndexEOF
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
                            table.[index] |>  Array.iter a
                        condition := true
                    condition := true
                                    
            else
                let curRight =  sppfNodes.[int !currentN]
                match curRight with
                    | :? TerminalNode as t ->
                        currentN := getNodeP !currentLabel !currentR !currentN
                        let r = (sppfNodes.[int !currentN]) :?> NonTerminalNode 
                        pop !currentGSSNode !currentIndex !currentN
                    | :? NonTerminalNode as r ->
                        if (r.Name = parser.LeftSide.[parser.StartRule]) && r.Extension = finalExtension
                        then 
                            resultAST := Some r 
                        pop !currentGSSNode !currentIndex !currentN
                        | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()

    let control () =
            while not !stop do
            if !condition then dispatcher() else processing()
    control()

    match !resultAST with
        | None -> Error ("String was not parsed")
        | Some res -> 
            let r1 = new Tree<_> (tokens, res, parser.rules)
            //r1.AstToDot parser. parser. tokenToNum tokenData (outputDir + fileName)
            Success (r1)
                    
//                    for d in packedNodes do
//                        if d = Unchecked.defaultof<_> then printf "null; " else printf "1; "
//                    printfn "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
//                    printfn "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
//                    packedNodes |> Array.iter(fun d -> if d <> null then d |> Seq.iter (fun kvp -> if kvp.Value <> null then kvp.Value |> Seq.iter (fun kvp -> kvp.Value.other.Value.Length |> printf "%A; ")))
//                    let r1 = new Tree<_> (tokens, res, parser.rules)
//                    let path = @"../../../src/GLLApplication/out1000.txt"
//                    printfn "PACKED NODES"
//                    //packedNodes |> Array.iter(fun d -> d |> Seq.iter (fun kvp -> kvp.Value |> Seq.iter (fun kvp -> kvp.Value.other.Value.Length |> printf "%A; ")))
//                    printfn ""
//                    printfn "INTERMIDIATE NODES"
//                    intermidiateNodes  |> Seq.iter (fun kvp -> kvp.Value.Count |> printf "%A; ")
//                    printfn ""
//                    printfn "NONTERMINAL NODES"
//                    nonTerminalNodes |> Seq.iter (fun kvp -> kvp.Count |> printf "%A; ")
//                    out.Close()
                       
                            
