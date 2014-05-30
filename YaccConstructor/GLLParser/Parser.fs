module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Generators.RNGLR.DataStructures

type Label =  
    val Rule             : int
    val mutable Position : int 
    static member  Equal (label1 : Label) (label2 : Label) =
        let mutable result = true
        if label1.Rule <> label2.Rule || label1.Position <> label2.Position then result <- false
        result 
    new (rule : int,position : int) = {Rule = rule; Position = position} 

type IntermidiateNode =
    val LeftChild  : obj
    val RightChild : obj
    val Pos        : int * int
    new (l , r, p) = {LeftChild = l; RightChild = r; Pos = p}

[<AllowNullLiteral>]
type Vertex  =
    val mutable OutEdges : UsualOne<Edge>
    val Level            : int
    val Value            : Label
    static member Equal (v1 : Vertex) (v2 : Vertex) =
        let mutable res = false
        if v1.Level = v2.Level && Label.Equal v1.Value v2.Value then
            res <- true
        res
    new (value, level) = {OutEdges = Unchecked.defaultof<_>; Value = value; Level = level}

and Edge =
    struct
        val Ast   : obj
        val Dest  : Vertex
        new (d, a) = {Dest = d; Ast = a}
    end

type Context =
    val Index         : int
    val Label         : Label
    val Node          : Vertex
    val Ast           : obj
    static member Equal (cntxt1 : Context) index label node ast =
        let mutable res = false
        if cntxt1.Index = index && Label.Equal cntxt1.Label label && Vertex.Equal cntxt1.Node node && cntxt1.Ast.Equals ast 
            then 
                res <- true
        res    
    new (index, label, node, ast) = {Index = index; Label = label; Node = node; Ast = ast}

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of string
    


let buildAst<'TokenType> (parser : ParserSource2<'TokenType>) (tokens : seq<'TokenType>) : ParseResult<_> = 
    let enum = tokens.GetEnumerator()
    let inputLength = Seq.length tokens
    let startNonTerm = parser.LeftSide.[parser.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parser.LeftSide)
    let getEpsilon =

         let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i-1))
         fun i -> epsilons.[i]
    if not <| enum.MoveNext() || parser.IndexEOF = parser.TokenToNumber enum.Current then
        if parser.AcceptEmptyInput then
            Success (new Tree<_>(null, getEpsilon startNonTerm, null))
        else
            Error ("This grammar does not accept empty input.")     
    else
        let tokens = Seq.toArray tokens

        let setR = new Queue<Context>();   
        let setP = new Queue<Vertex * obj>();    
        let setU = Array.init (inputLength + 1)  (fun _ -> new List<Label * Vertex * obj>())  
            
        let currentIndex = ref 0
        let previousIndex = ref 0

        let currentRule = parser.StartRule
        let dummy = box <| null
        let dummyGSSNode = new Vertex(new Label(currentRule, -1), !currentIndex)
        let currentLabel = ref <| new Label(currentRule, 0)
        let startLabel = new Label(currentRule, 0)
        //let startGSSNode = new Vertex(!currentLabel, !currentIndex)
        
        let currentN = ref <| null
        let currentR = ref <| null

        //let currentGSSNode = ref <| new Vertex(!currentLabel, !currentIndex)
        let currentGSSNode = ref <| dummyGSSNode
        let currentContext = ref <| new Context(!currentIndex,!currentLabel,!currentGSSNode, dummy)
        
        
        let gss = Array.init inputLength (fun _ -> new ResizeArray<Vertex>())
        let ast = Array.init inputLength (fun _ -> new ResizeArray<AST>())


        let terminalNodes = new BlockResizeArray<Nodes>()

        let chainCanInferEpsilon rule pos =
            let curRule = parser.Rules.[rule]
            let mutable result = true
            for i = pos to curRule.Length - 1 do
                if result && not parser.canInferEpsilon.[curRule.[i]]
                then    
                    result <- false
            result

        (*Проверить есть ли такое дерево, кого и куда вставлять и куда добавить правое и левое*)
        let handleIntermidiate node   (prod : int) = 
            let result = new List<obj>()
            let rec handle (o : obj) =
                if o <> null then
                    //let res =
                        match o with
                        | :? IntermidiateNode as interNode ->
                            let t : IntermidiateNode = unbox interNode
                            handle t.LeftChild
                            handle t.RightChild
                            //result.Add (box <| fst)
                            //result.Add (box <| snd)
                        | :? Nodes as node -> 
                            result.Add (box <| node)     
                        | :? AST as ast    -> 
                             result.Add (box <| ast)
                        | _ -> failwith "Unexpected type."
                    //res
            handle node
            let res = result.ToArray()
            let fam = new Family(prod, new Nodes(res))
            fam
                                
        let containsContext (set : List<Label * Vertex * obj>) (index : int) (label : Label) (gssNode : Vertex) (ast : obj) =
            let mutable result = false
            let first  (o, _, _) = o
            let second (_, o, _) = o
            let third  (_, _, o) = o
            for cntxt in set do    
                if (not result) 
                    && Label.Equal (first cntxt) label 
                    && Vertex.Equal (second cntxt) gssNode then
                    if ast = null && third cntxt = null 
                    then
                        result <- true
                    elif ast <> null && third cntxt <> null && ast.Equals (third cntxt)
                    then
                        result <- true
                                        
            result

        let addContext (label : Label)  (index : int) (node : Vertex) (ast : obj) =
            let res =
                if not <| containsContext setU.[index] index label node ast
                then
                    Some <| new Context(index, label, node, ast)
                else None
            res

        let getNodeP (label : Label) (left : obj) (right : obj) =
            let mutable result =  right
            let previousSym = parser.Rules.[label.Rule].[label.Position - 1]
            if parser.Rules.[label.Rule].Length >= label.Position + 1 
            then
                let nextSym = parser.Rules.[label.Rule].[label.Position]
                let cond = false
                if cond
                //parser.NumIsTerminal previousSym || not (chainCanInferEpsilon label.Rule (label.Position - 1)) && not (chainCanInferEpsilon label.Rule (label.Position))
                then 
                    box <| result
                else
                    if chainCanInferEpsilon label.Rule label.Position
                        then
                            label.Position <- parser.Rules.[label.Rule].Length
                    if not (left = null) then
                        ///add checking
                        result <- new IntermidiateNode(left, right, (label.Rule, label.Position)) 
                    else result <- new IntermidiateNode(null, right, (label.Rule, label.Position))             
                    box <| result
            else 
                if left <> null 
                then
                    result <- new IntermidiateNode(left, right, (label.Rule, label.Position)) 
                    box <| result
                else 
                    result <- new IntermidiateNode(null, right, (label.Rule, label.Position))
                    box <| result
            
        let getNodeT term b =
            box <| new Nodes(term, null, null, b, b + 1)

        let containsGSSNode (l : Label) (i : int) =  
            let curLevel = gss.[i] 
            let mutable cond = true
            let mutable res = dummyGSSNode  
            for vrtx in curLevel do
                if cond && Label.Equal vrtx.Value l
                then 
                    res <- vrtx
                    cond <-false
            if cond
            then
                res <- new Vertex(l, i)     
            res

        let containsEdge (b : Vertex) (e : Vertex) (ast : obj)=
            let edges = b.OutEdges
            let mutable result = false
            if edges.first <> Unchecked.defaultof<_>
            then
                if not (Vertex.Equal edges.first.Dest e && edges.first.Ast.Equals ast)
                then
                    if edges.other <> null
                    then
                        for edge in edges.other do
                            if Vertex.Equal edge.Dest e && edge.Ast.Equals ast
                            then
                                result <- true
                else result <- true
            result
                           

        let create (label : Label) (u : Vertex) (index : int) (ast : obj) = 
            let v = containsGSSNode label index
            if not (containsEdge v u ast)
            then
                let newEdge = new Edge(u, ast)
                for pair in setP do
                    if Vertex.Equal v (fst pair) 
                    then 
                        let y = getNodeP label ast (snd pair)
                        let cntxt = addContext label index u y
                        if  cntxt.IsSome
                        then
                            setU.[index].Add (!currentLabel, !currentGSSNode, ast) /// check!!
                            setR.Enqueue(cntxt.Value)    
                if v.OutEdges.first <> Unchecked.defaultof<_>
                then
                    if v.OutEdges.other <> null
                    then
                        v.OutEdges.other <- Array.append v.OutEdges.other [|newEdge|]
                    else 
                        v.OutEdges.other <- [|newEdge|]
                else v.OutEdges.first <- newEdge
            gss.[index].Add v
            v
          
        let pop (u : Vertex) (i : int) (z : obj) =
            if not (Vertex.Equal u dummyGSSNode) then
                let label = u.Value
                setP.Enqueue(u, z)
                let processEdge (edge : Edge) =
                    let y = getNodeP label edge.Ast z 
                    let cntxt = addContext label i edge.Dest y
                    if  cntxt.IsSome
                    then
                        setU.[i].Add (label, edge.Dest, z)
                        setR.Enqueue(cntxt.Value)    
                processEdge u.OutEdges.first
                if u.OutEdges.other <> null then 
                    for edge in u.OutEdges.other do
                        processEdge edge

        let table = parser.Table
   
        let condition = ref false 
        let stop = ref false
        
        let findTree family level prod =
            let curLevelTrees = ast.[level]
            let mutable wasAdded = false
            let mutable temp = null  
            if curLevelTrees.Count <> 0
            then
                let mutable cond = true   
                let mutable i = 0
                while cond && i < curLevelTrees.Count do
                    let tree = curLevelTrees.[i]
                    if parser.LeftSide.[tree.first.prod] = parser.LeftSide.[prod] 
                    then
                        if currentLabel.Value.Rule = tree.first.prod
                        then
                            cond <- false                                          
                        else
                            if tree.other <> null
                            then
                                for fam in tree.other do
                                    if currentLabel.Value.Rule = fam.prod
                                    then    
                                        cond <- false                                                  
                        if cond
                        then
                            if tree.other <> null
                            then
                                tree.other <- Array.append tree.other [|family|]
                            else 
                                tree.other <- [|family|]                            
                        wasAdded <- true
                        temp <- box tree
                        cond <- false
                    i <- i + 1
                if not wasAdded
                then
                    let res = new AST(family, null)
                    curLevelTrees.Add <| res
                    temp <- box res
            else
                let res = new AST(family, null)
                curLevelTrees.Add <| res
                temp <- box res
                 
            let result : AST = unbox <| temp
            result 
   
        let rec dispatcher () =  
         
            if setR.Count <> 0 then
                currentContext := setR.Dequeue()
                currentIndex := currentContext.Value.Index
                if !currentIndex <> !previousIndex 
                then
                    //setU.[!previousIndex].Clear()
                    previousIndex := !currentIndex 
                currentGSSNode := currentContext.Value.Node
                currentLabel := currentContext.Value.Label
                currentN := currentContext.Value.Ast 
                currentR := null
                condition := false        
            else 
                stop := true
                 
                       
        and processing () =  

            let getIndex(nTerm, term) = 
                let mutable index = nTerm
                index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                index <- index + term - parser.NonTermCount
                index

            if Array.length parser.Rules.[currentLabel.Value.Rule]  <> currentLabel.Value.Position then
                let curToken = parser.TokenToNumber tokens.[!currentIndex]
                let curSymbol = parser.Rules.[currentLabel.Value.Rule].[currentLabel.Value.Position]
                if parser.NumIsTerminal curSymbol  || parser.NumIsLiteral curSymbol 
                then
                    if curSymbol = curToken
                    then
                        if !currentN = null 
                        then
                            if (terminalNodes.Item !currentIndex).fst = null
                            then
                                currentN := getNodeT tokens.[!currentIndex] !currentIndex
                                terminalNodes.Set !currentIndex (unbox <| !currentN)
                            else 
                                currentN := box <| terminalNodes.Item !currentIndex
                        else
                            if (terminalNodes.Item !currentIndex).fst = null
                            then
                                currentR := getNodeT tokens.[!currentIndex] !currentIndex
                                terminalNodes.Set !currentIndex (unbox <| !currentR)
                            else 
                                currentR := box <| terminalNodes.Item !currentIndex
                        currentIndex := !currentIndex + 1
                        currentLabel.Value.Position <- currentLabel.Value.Position + 1
                        if !currentR <> null
                        then
                            currentN := getNodeP !currentLabel !currentN !currentR
                        condition := false
                    else 
                        condition := true

                else 
                    let index = getIndex(curSymbol, curToken)
                    let temp = table.[index]
                    if Array.length table.[index] <> 0 
                    then
                        currentGSSNode := create (new Label(currentLabel.Value.Rule, currentLabel.Value.Position + 1)) !currentGSSNode !currentIndex !currentN
                        for ruleN in table.[index] do
                            let newLabel = new Label(ruleN, 0)
                            let cntxt = addContext newLabel !currentIndex !currentGSSNode dummy
                            if  cntxt.IsSome
                            then
                                setU.[!currentIndex].Add (newLabel, !currentGSSNode, dummy)
                                setR.Enqueue(cntxt.Value)    
                            
                    condition := true             
            else 
//                if not (parser.LeftSide.[currentLabel.Value.Rule] = startNonTerm) 
//                then 
                let curRight = unbox <| !currentN
                let resTree = handleIntermidiate curRight currentLabel.Value.Rule
                let resTree = findTree resTree currentGSSNode.Value.Level currentLabel.Value.Rule
                pop !currentGSSNode !currentIndex resTree
                condition := true
//                else
//                    let curRight = unbox <| !currentN
//                    let resTree = handleIntermidiate curRight currentLabel.Value.Rule
//                    let resTree = findTree resTree currentGSSNode.Value.Level currentLabel.Value.Rule
//                    condition := true
        let control () =
            while not !stop do
                if !condition then dispatcher() else processing()
        control()
             
        let root = ref None
        if ast.[0].Count <> 0
        then
            for tree in ast.[0] do
                if parser.LeftSide.[tree.first.prod] = parser.LeftSide.[parser.StartRule]
                then
                    root := Some <| tree
            match !root with
                | None -> Error ("String was not parsed")
                | Some res -> 
        //    debugFuns().drawGSSDot "res.dot"
                    Success (new Tree<_> (terminalNodes.ToArray(), res, parser.Rules))
        else
            Error("String was not parsed.")
         