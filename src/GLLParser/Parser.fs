module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.DataStructures
open Microsoft.FSharp.Collections
open FSharpx.Collections.Experimental
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open AbstractAnalysis.Common


let inline packExtension left right : int64<extension> =  LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)



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
    let currentRule = parser.StartRule
    let structures = new ParserStructures<'TokenType>(currentRule)
    let setP = structures.SetP
    let epsilonNode = structures.EpsilonNode
    let setR = structures.SetR
    let sppfNodes = structures.SppfNodes
//    if inputLength = 0 || parser.IndexEOF = parser.TokenToNumber tokens.[0] then
//        if parser.AcceptEmptyInput then
//            let eps = new NonTerminalNode
//            Success (new Tree<_>(null, getEpsilon startNonTerm, null))
//        else
//            Error ("This grammar does not accept empty input.")     
//    else
    let slots = parser.Slots
    
    let printGrammar =
        for i = 0 to parser.rulesCount - 1 do
            let str = 
                let mutable res = ""
                for j = 0 to parser.rules.[i].Length - 1 do
                    res <- res + " " + parser.NumToString parser.rules.[i].[j]
                res
            printfn "%d : %A -> %A" i (parser.NumToString parser.LeftSide.[i]) str
    
    printGrammar 
       
    //свернуть в 1 инт
    let setU = Array.zeroCreate<Dictionary<int, Dictionary<int64, ResizeArray<int<nodeMeasure>>>>> (inputLength + 1)

    let currentIndex = ref 0
    
    
    let dummyGSSNode = new Vertex(-1<positionInInput>, currentRule*1<labelMeasure>)
  
    //let packedNodes = Array.zeroCreate<IntDictionary<IntDictionary<int>>> (inputLength + 1)
    let packedNodes = new Dictionary<int, int<nodeMeasure>>()
        //Array.zeroCreate<IntDictionary<IntDictionary<ResizableUsualOne<LblNodePair>>>> (inputLength + 1)
    let inline f x y z = x * (inputLength + 1) * (inputLength + 1) + y * (inputLength + 1) + z
    let inline f4 x y z w = x * parser.Slots.Count * (inputLength + 1) * (inputLength + 1) + y * (inputLength + 1) * (inputLength + 1) + z * (inputLength + 1) + w
    let nonTerminalNodes = new Dictionary<int, int<nodeMeasure>>()
    //let nonTerminalNodes = Array3D.zeroCreate<int<nodeMeasure>> parser.NonTermCount (inputLength + 1) (inputLength + 1)

    //we can use dictionary <extension, dict>
    let intermidiateNodes = new Dictionary<int, int<nodeMeasure>>()
 
    let edges = Array2D.zeroCreate<Dictionary<int64, Dictionary<int, ResizeArray<int>>>> parser.NonTermCount (inputLength + 1)
        
    let terminalNodes = new BlockResizeArray<int<nodeMeasure>>()
 


    let currentGSSNode = ref <| dummyGSSNode
    let currentContext = ref <| new Context(*<'TokenType>*)(!currentIndex, !structures.CurrentLabel, !currentGSSNode, structures.Dummy)
        
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

   
                      

    let inline slotIsEnd (label : int<labelMeasure>) =
        (getPositionNew label) = Array.length (parser.rules.[getRule label])

    let findSppfNode label lExt rExt : int<nodeMeasure> =
        let isEnd = slotIsEnd label
        let nTerm = parser.LeftSide.[getRule label]
     
        if isEnd
        then
            let key = f lExt rExt nTerm
            let contains, n = nonTerminalNodes.TryGetValue(key)
            if not contains
            then
                let newNode = new NonTerminalNode(nTerm, (packExtension lExt rExt))
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
                let newNode = new IntermidiateNode(int label, (packExtension lExt rExt))
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
            
                     
    let containsEdge (b : Vertex) (l : int<labelMeasure>) (e : Vertex) (ast : int<nodeMeasure>) =
        let tempRule = getRule l
        let tempPos = getPositionNew l
        let dict1 = edges.[int b.NontermLabel, int b.Level]
        let key = pack ast (int l)
        if dict1 <> Unchecked.defaultof<_>
        then
            let contains, t = dict1.TryGetValue(key)
            if contains
            then
                let c2, t2 = t.TryGetValue(int e.NontermLabel)
                if c2 
                then
                    if t2.Contains(int e.Level) then true
                    else 
                        t2.Add(int e.Level) 
                        false
                else
                    let arr = new ResizeArray<int>()
                    arr.Add(int e.Level) 
                    t.Add(int e.NontermLabel, arr)
                    false
            else
                let newDict = new Dictionary<int, ResizeArray<int>>()
                let arr = new ResizeArray<int>()
                arr.Add(int e.Level) 
                newDict.Add(int e.NontermLabel, arr)
                dict1.Add(key, newDict)
                false
        else
            let newDict = new Dictionary<int64, Dictionary<int, ResizeArray<int>>>()
            let newDict2 = new Dictionary<int, ResizeArray<int>>()
            let newArr = new ResizeArray<int>()
            newArr.Add(int e.Level)
            newDict2.Add(int e.NontermLabel, newArr)
            newDict.Add(key,newDict2)
            edges.[int b.NontermLabel, int b.Level] <- newDict
            false                    
        
    
    let create index (label : int<labelMeasure>) (vertex : Vertex) (ast : int<nodeMeasure>) = 
        let v = new Vertex(index, parser.LeftSide.[getRule label]*1<labelMeasure>)
        let vertexKey = pack index (int label)
        let temp = containsEdge v label vertex ast
        if not <| temp //containsEdge v vertex ast
        then
            if setP.ContainsKey(vertexKey)
            then
                let arr = setP.[vertexKey]
                arr.DoForAll (fun tree  ->
                    let y = structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy label ast tree
                    let index = getRightExtension <| structures.GetTreeExtension y 
                    
                    structures.AddContext setU index label vertex y (*List.empty<ParserEdge<'TokenType*ref<bool>>>*))
        v


    let pop (u : Vertex) (i : int) (z : int<nodeMeasure>) =
        if u <> dummyGSSNode
        then
            let vertexKey = pack u.Level (int u.NontermLabel)
            if setP.ContainsKey vertexKey
            then
                setP.[vertexKey].Add z
            else
                //let newList = new ResizeArray<int<nodeMeasure>>()
                //newList.Add(z)
                setP.Add(vertexKey, new ResizableUsualOne<_>(z))
            let outEdges = edges.[int u.NontermLabel, int u.Level]
            
            for kvp1 in outEdges do
                let sppfNodeOnEdge = (getLeft kvp1.Key) * 1<nodeMeasure>
                let slot = (getRight kvp1.Key) * 1<labelMeasure>
                for kvp2 in kvp1.Value do 
                    for level in kvp2.Value do
                        let resTree = structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy slot sppfNodeOnEdge z 
                        let newVertex = new Vertex(level*1<positionInInput>, kvp2.Key*1<labelMeasure>)
                        structures.AddContext setU i slot newVertex resTree

    let table = parser.Table
        
    let condition = ref false 
    let stop = ref false

    let rec dispatcher () =
        if setR.Count <> 0
        then
            currentContext := setR.Dequeue()
            currentIndex := currentContext.Value.Index
            currentGSSNode := currentContext.Value.Vertex
            structures.CurrentLabel := currentContext.Value.Label
            let tmpRule = getRule !structures.CurrentLabel
            let tmpPos = getPositionNew !structures.CurrentLabel
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
            let t = new TerminalNode(-1, packExtension !currentIndex !currentIndex)
            sppfNodes.Add t
            let res = sppfNodes.Length - 1
            structures.CurrentR := res * 1<nodeMeasure>
            structures.CurrentN := structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy !structures.CurrentLabel !structures.CurrentN !structures.CurrentR  
            pop !currentGSSNode !currentIndex !structures.CurrentN
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
                            if !structures.CurrentN = structures.Dummy
                            then structures.CurrentN := getNodeT !currentIndex
                            else structures.CurrentR := getNodeT !currentIndex
                            currentIndex := !currentIndex + 1
                            structures.CurrentLabel := packLabelNew (rule) ((position) + 1)
                            if !structures.CurrentR <> structures.Dummy
                            then 
                                structures.CurrentN := structures.GetNodeP findSppfNode findSppfPackedNode structures.Dummy !structures.CurrentLabel !structures.CurrentN !structures.CurrentR
                            condition := false
                    else 
                        let getIndex nTerm term = 
                            let mutable index = nTerm
                            index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                            index <- index + term - parser.NonTermCount
                            index

                        let index = getIndex curSymbol curToken
                        let key = int((int32 curSymbol <<< 16) ||| int32 curToken)
                        currentGSSNode := create (!currentIndex * 1<positionInInput>) (packLabelNew (rule) (position + 1)) !currentGSSNode  !structures.CurrentN
                        if table.ContainsKey key 
                        then
                            for r in table.[key] do 
                                let newLabel = packLabelNew rule 0
                                structures.AddContext setU !currentIndex newLabel !currentGSSNode structures.Dummy 
                            
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
                        let key = int((int32 curSymbol <<< 16) ||| int32 (curToken - parser.NonTermCount))
                        currentGSSNode := create (!currentIndex * 1<positionInInput>) (packLabelNew (rule) (position + 1)) !currentGSSNode  !structures.CurrentN
                        if table.ContainsKey key
                        then
                            for r in table.[key] do 
                                let newLabel = packLabelNew rule 0
                                structures.AddContext setU !currentIndex newLabel !currentGSSNode structures.Dummy 
                            
                        condition := true
                    condition := true
                                    
            else
                let curRight =  sppfNodes.[int !structures.CurrentN]
                structures.FinalMatching curRight parser.LeftSide.[parser.StartRule] [|finalExtension|] findSppfNode findSppfPackedNode currentGSSNode currentIndex pop
    

    let control () =
            while not !stop do
            if !condition then dispatcher() else processing()
    control()

    match !structures.ResultAST with
        | None -> Error ("String was not parsed")
        | Some res -> 
            let r1 = new Tree<_> (tokens, res, parser.rules)
            //r1.AstToDot parser.NumToString parser.TokenToNumber parser.TokenData "AST123456.dot"
            Success (r1)