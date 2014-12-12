module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.Common.AST3
open Yard.Generators.Common.DataStructures
open Microsoft.FSharp.Collections

[<Measure>] type vertex

let inline packExtension left right : int64<extension> =  LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)

let inline packVertex level label: int64<vertex> =  LanguagePrimitives.Int64WithMeasure ((int64 level <<< 32) ||| int64 label)
let inline getIndex1Vertex (long : int64<vertex>)       = int <| ((int64 long) &&& 0xffffffffL)
let inline getIndex2Vertex (long : int64<vertex>)       = int <| ((int64 long) >>> 32)

let inline getRule packedValue  = int packedValue >>> 16
let inline getPosition (packedValue : int) = int (packedValue &&& 0xffff)

//let inline pack3ToInt64 p l r : int64<key>        = LanguagePrimitives.Int64WithMeasure (((int64 p) <<< 52) ||| ((int64 l) <<< 26) ||| (int64 r))
//let inline getProduction (long : int64<key>)      = int (int64 long >>> 52)
//let inline getLeftExtension3 (long : int64<key>)  = int((int64 long <<< 12) >>> 38)
//let inline getRightExtension3 (long : int64<key>) = int((int64 long <<< 38) >>> 38)


[<AllowNullLiteral>]
type Vertex =
    val Level            : int
    val Label            : int
    new (level, label) = {Level = level; Label = label}


[<Struct>]
type Context =
    val Index         : int
    val Label         : int
    val Vertex        : Vertex
    val Ast           : int
    new (index, label, vertex, ast) = {Index = index; Label = label; Vertex = vertex; Ast = ast}

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of string

//let drawDot (parser : ParserSource2<'TokenType>) (tokens : 'TokenType[])(path : string) (gss : array<ResizeArray<Vertex>>) =
//    use out = new System.IO.StreamWriter (path)
//    let was = new Dictionary<_,_>()
//    let levels = new Dictionary<_,_>()
//    out.WriteLine "digraph GSS {"
//    let print s = out.WriteLine ("    " + s)
//    let curNum = ref 0
//    print "rankdir=RL"
//    let getAstString (ast : ExtensionTree) =
//        let ast = ast.tree
//        match ast with
//       // | :? int as i when i >= 0 -> tokens.[i] |> parser.TokenToNumber |> parser.NumToString |> sprintf "%s"    
//       // | :? int as i when i < 0 -> "eps " + parser.NumToString (-i - 1)
//        | :? AST as ast -> 
//            let nonT = 
//                if ast.first.prod < parser.LeftSide.Length then ast.first.prod
//                else -1
//            nonT.ToString()
//        | :? IntermidiateNode as iN ->
//            (getRule iN.Position).ToString()
//        | null -> "null" 
//        | _ -> failwith "Unexpected ast"
//
//    let rec dfs (u : Vertex) =
//        was.Add (u, !curNum)
//        if not <| levels.ContainsKey u.Level then levels.[u.Level] <- [!curNum]
//        else
//            levels.[u.Level] <- !curNum :: levels.[u.Level]
//        if (getRule u.Label = -1) then print <| sprintf "%d [label=\"%s\"]" !curNum "dummy node"
//        else print <| sprintf "%d [label=\"(%d, %d), %d\"]" !curNum (getRule u.Label) (getPosition u.Label) u.Level
//        incr curNum
//        if u.OutEdges.first <> Unchecked.defaultof<_> 
//        then 
//            handleEdge u u.OutEdges.first
//            if u.OutEdges.other <> null then u.OutEdges.other |> Array.iter (handleEdge u)
//
//    and handleEdge u (e : Edge) =
//        let v = gss.[getIndex2Vertex e.Dest].[getIndex1Vertex e.Dest]
//        if not <| was.ContainsKey v then dfs v
//        print <| sprintf "%d -> %d [label=\"%s\"]" was.[u] was.[v] (getAstString e.Ast)
//    for levels in gss do
//        for v in levels do
//            if not <| was.ContainsKey v then dfs v
//    
//    for level in levels do
//        print <| sprintf "{rank=same; %s}" (level.Value |> List.map (fun (u : int) -> string u) |> String.concat " ")
//
//    out.WriteLine "}"
//    out.Close()

let buildAst<'TokenType> (parser : ParserSource2<'TokenType>) (tokens : seq<'TokenType>) (slotNumber : int) (dummySlot : int) (slots : Dictionary<int, int>) : ParseResult<_> = 
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
        let setR = new Queue<Context>()   
        let setP = new Dictionary<Vertex, ResizeArray<int>> ()
        let setU = Array.zeroCreate<Dictionary<int, Dictionary<Vertex, ResizeArray<int>>>> (inputLength + 1)

        let currentIndex = ref 0
        let currentrule = parser.StartRule
        let dummyGSSNode = new Vertex(dummySlot, !currentIndex)
        let currentLabel = ref <| packLabel currentrule 0
        let dummyAST = new TerminalNode(-1, -1)
        let dummy = 0
        let currentN = ref <| 0
        let currentR = ref <| 0

        let resultAST = ref None
        let packedNodes = Array3D.zeroCreate<int[,]> inputLength inputLength inputLength
        let symbolNodes = Array3D.zeroCreate<int> parser.NonTermCount inputLength inputLength 
        let edges = Array2D.zeroCreate<Dictionary<int, Dictionary<int, ResizeArray<int>>>> slotNumber inputLength
        
        let sppfNodes = new BlockResizeArray<INode>()
        sppfNodes.Add(dummyAST)

        let currentGSSNode = ref <| dummyGSSNode
        let currentContext = ref <| new Context(!currentIndex, !currentLabel, !currentGSSNode, 0)
        
        let finalExtension = pack3ToInt64 parser.StartRule 0 inputLength

        let containsContext index label vertex ast =
            if index < inputLength
            then
                if setU.[index] <> Unchecked.defaultof<Dictionary<int, Dictionary<Vertex, ResizeArray<int>>>>
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
                            let arr = new ResizeArray<int> ()
                            arr.Add(ast)
                            current.Add(vertex, arr)                    
                            false
                    else 
                        let dict = new Dictionary<Vertex, ResizeArray<int>>()
                        setU.[index].Add(label, dict)
                        let arr = new ResizeArray<int>()
                        arr.Add(ast)
                        dict.Add(vertex, arr) 
                        false
                else 
                    let dict1 =  new Dictionary<int, Dictionary<Vertex, ResizeArray<int>>>()
                    setU.[index] <- dict1
                    let dict2 = new Dictionary<Vertex, ResizeArray<int>>()
                    dict1.Add(label, dict2)
                    let arr = new ResizeArray<int>()
                    arr.Add(ast)
                    dict2.Add(vertex, arr)
                    false
            else true
                      

        let addContext index label  vertex ast =
            if not <| containsContext index label vertex ast
            then
                setR.Enqueue(new Context(index, label, vertex, ast)) 

        let slotIsEnd label =
            (getPosition label) = (parser.)

        let findSppfNode isEnd label lExt rExt leftChild rightChild =
            let nTerm = parser.LeftSide.[getRule label]
            if symbolNodes.[nTerm, lExt, rExt] = Unchecked.defaultof<int>
            then
                if isEnd then
                    let newNode = new NonTerminalNode(nTerm, (packExtension lExt rExt))
                    sppfNodes.Add(newNode)
                else
                    let newNode = new IntermidiateNode(label, packExtension lExt rExt, leftChild, rightChild)
                    sppfNodes.Add(newNode)
                let num = sppfNodes.Count - 1
                symbolNodes.[nTerm, lExt, rExt] <- num
                num
            else 
                symbolNodes.[nTerm, lExt, rExt]

        
                
            
        let getNodeP label (left : int) (right : int) =
            if slotIsEnd label then right
            else
                if left <> dummy
                then
                    let currentLeft = sppfNodes.Item left
                    match currentLeft with                    
                    | :? NonTerminalNode as nonTerm ->
                        let extension = nonTerm.Child
                        ()
                    | :? IntermidiateNode as interm ->
                        let extension = interm
                        ()
                        
                    let currentLeft = sppfNodes.Item right
                    let y = findSppfSymbolNode label currentLeft 
                    findSppfPackedNode 
                else
                    let y = findSppfSymbolNode
                    findSppfPackedNode
            
                
            
        //let getNodeT index =
                     
            
        let containsEdge (b : Vertex) (e : Vertex) ast =
            let dict1 = edges.[b.Label, b.Level]
            if dict1 <> Unchecked.defaultof<Dictionary<int, Dictionary<int, ResizeArray<int>>>>
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
                    let d = new Dictionary<int, ResizeArray<int>>()
                    dict1.Add(ast, d)
                    let l = new ResizeArray<int>()
                    l.Add(e.Level)
                    d.Add(e.Label, l)
                    false
            else
                edges
                false                    
            
        let create index label vertex ast = 
            let v = new Vertex(index, label)
            if not containsEdge v vertex ast
            then
                if setP.TryGetValue(v)
                then
                    let arr = setP.[v]
                    for tree in arr do
                        let y = getNodeP label ast tree
                        addContext (sppfNodes.Item tree) label vertex y 

     //   let pop (u : int64<vertex>) (i : int) (z : AST) extension =
        

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
                currentR := dummyAST
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
                                if !currentN = dummyAST 
                                then currentN := getNodeT !currentIndex
                                else currentR := getNodeT !currentIndex
                                currentIndex := !currentIndex + 1
                                currentLabel := packLabel (rule) ((position) + 1)
                                if not <| obj.ReferenceEquals(!currentR, dummyAST)
                                then currentN := getNodeP !currentLabel !currentN !currentR
                                condition := false
                        else 
                            let getIndex nTerm term = 
                                let mutable index = nTerm
                                index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                                index <- index + term - parser.NonTermCount
                                index

                            let index = getIndex curSymbol curToken
                            currentGSSNode := create (packLabel (rule) (position + 1)) !currentGSSNode !currentIndex !currentN
                            if Array.length table.[index] <> 0 
                            then
                                let a rule = 
                                    let newLabel = packLabel rule 0
                                    addContext newLabel !currentIndex !currentGSSNode dummyAST 
                                table.[index] |> Array.iter a
                    else condition := true
                                    
                else
                    let curRight =  !currentN
                    let t1 = getLeftExtension curRight.extension
                    let t2 = getRightExtension curRight.extension
                    let extension = curRight.extension
                    let key = pack3ToInt64 rule (getLeftExtension curRight.extension) (getRightExtension curRight.extension)
                    let fam = handleIntermidiate curRight.tree (rule) key
                    let resTree = findTree key fam 
                    if  key = finalExtension
                    then resultAST := Some resTree
                    pop !currentGSSNode !currentIndex resTree currentN.Value.extension
        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match !resultAST with
            | None -> Error ("String was not parsed")
            | Some res -> 
                    drawDot parser tokens "gss.dot" gss
                    let r1 = new Tree<_> (tokens, res, parser.rules)
                    r1.AstToDot parser.NumToString parser.TokenToNumber  parser.LeftSide "ast1111111.dot"
                    Success (r1)   
                        