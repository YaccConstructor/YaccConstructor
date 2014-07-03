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
        let mutable result = false
        if label1.Rule = label2.Rule && label1.Position = label2.Position then result <- true
        result 
    new (rule : int,position : int) = {Rule = rule; Position = position} 

type IntermidiateNode =
    val LeftChild  : obj
    val RightChild : obj
    val Position   : int * int
    val Extention  : int * int
    new (l , r, p, e) = {LeftChild = l; RightChild = r; Position = p; Extention = e}

[<AllowNullLiteral>]
type Vertex  =
    val mutable OutEdges : UsualOne<Edge>
    val Level            : int
    val Value            : Label
    static member Equal (v1 : Vertex) (v2 : Vertex) =
        let mutable result = false
        if v1.Level = v2.Level && Label.Equal v1.Value v2.Value then
            result <- true
        result
    new (value, level) = {OutEdges = Unchecked.defaultof<_>; Value = value; Level = level}

and Edge =
    struct
        val Ast   : obj
        val Dest  : Vertex
        new (d, a) = {Dest = d; Ast = a}
    end
let inline treeCompare (ast1 : obj) (ast2 : obj) =
    let mutable result = false
    let ext1, prod1 =
        match ast1 with
        | :? AST as a -> Some(a.leftExt, a.rightExt), a.first.prod
        | :? IntermidiateNode as i -> Some i.Extention, fst i.Position
        | _ -> None,0
    let ext2, prod2 =
        match ast2 with
        | :? AST as a -> Some(a.leftExt, a.rightExt), a.first.prod
        | :? IntermidiateNode as i -> Some i.Extention, fst i.Position
        | _ -> None,0

    if ext1 = ext2 && prod1 = prod2
    then
        result <- true
    result

type Context =
    val Index         : int
    val Label         : Label
    val Node          : Vertex
    val Ast           : obj
    static member Equal (label1 : Label) (node1 : Vertex) (ast1 : obj) (label2 : Label) (node2 : Vertex) (ast2 : obj) =
        let mutable result = false
        if Label.Equal label1 label2 && Vertex.Equal node1 node2 && treeCompare ast1 ast2
            then 
                result <- true
        result    
    new (index, label, node, ast) = {Index = index; Label = label; Node = node; Ast = ast}

type ParseResult<'TokenType> =
    | Success of List<Tree<'TokenType>>
    | Error of string

let buildAst<'TokenType> (parser : ParserSource2<'TokenType>) (tokens : seq<'TokenType>) : ParseResult<_> = 
    let tokens = Seq.toArray tokens
    let inputLength = Seq.length tokens
    let startNonTerm = parser.LeftSide.[parser.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parser.LeftSide)
    let resultAST = ref None
    let getEpsilon =

         let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i-1))
         fun i -> epsilons.[i]
    if inputLength = 0 || parser.IndexEOF = parser.TokenToNumber tokens.[0] then
//        if parser.AcceptEmptyInput then
//            Success (new Tree<_>(null, getEpsilon startNonTerm, null))
//        else
        Error ("This grammar does not accept empty input.")     
    else
        let epsilon = new Nodes()
        let setR = new Queue<Context>();   
        let setP = new Queue<Vertex * obj>();    
        let setU = Array.init (inputLength + 1)  (fun _ -> new List<Label * Vertex * obj>())  
            
        let currentIndex = ref 0
        let previousIndex = ref 0

        let currentRule = parser.StartRule
        let dummy = box <| null
        let dummyGSSNode = new Vertex(new Label(-1, -1), !currentIndex)
        let currentLabel = ref <| new Label(currentRule, 0)
        
        let currentN = ref <| null
        let currentR = ref <| null

        let currentGSSNode = ref <| dummyGSSNode
        let currentContext = ref <| new Context(!currentIndex,!currentLabel,!currentGSSNode, dummy)
        
        let gss = Array.init inputLength (fun _ -> new ResizeArray<Vertex>())
        let astTemp = new List<obj>()

        let terminalNodes = new BlockResizeArray<Nodes>()

        let chainCanInferEpsilon rule pos =
            let curRule = parser.Rules.[rule]
            let mutable result = true
            for i = pos to curRule.Length - 1 do
                if result && not parser.canInferEpsilon.[curRule.[i]]
                then    
                    result <- false
            result

        let rec astToTokens (x : obj) =
            let mutable res = []
            match x : obj with 
            | :? int as t when t >= 0 -> res <- x :: res
            | :? Family as fam ->
                for i = 0 to fam.nodes.Length - 1 do
                    res <- res @ astToTokens fam.nodes.[i]
            | :? AST as ast ->
                if ast.other <> null 
                then
                    for family in ast.other do
                        if family.prod = parser.LeftSide.Length
                        then res <- res @ [ast]
                        else res <- res @ astToTokens family
                            
                if ast.first.prod = parser.LeftSide.Length
                then res <- [ast] @ res
                else res <- astToTokens ast.first @ res
            | _ -> ()
            res
        
        let handleIntermidiate node prod = 
            let result = new List<obj>()
            let rec handle (o : obj) =
                if o <> null then
                        match o with
                        | :? IntermidiateNode as i ->
                            let t : IntermidiateNode = unbox i
                            handle t.LeftChild
                            handle t.RightChild
                        | :? Nodes as n -> 
                            result.Add (box <| n)     
                        | :? AST as a    -> 
                             result.Add (box <| a)
                        | _ -> failwith "Unexpected type."
                  
            handle node
            let result = result.ToArray()
            let l =
                match result.[0] with
                | :? Nodes as n ->
                    n.leftExt
                | :? AST as a -> a.leftExt
                | _ -> failwith "Unexpected type."
            let r =
                match result.[result.Length - 1] with
                | :? Nodes as n ->
                    n.rightExt
                | :? AST as a -> a.rightExt 
                | _ -> failwith "Unexpected type."
            let family = new Family(prod, new Nodes(result, l, r, -1),l, r)
            family
               
        let containsContext index (label : Label) (gssNode : Vertex) (ast : obj) =
            let set = setU.[index]
            let mutable contains = false
            let first  (o, _, _) = o
            let second (_, o, _) = o
            let third  (_, _, o) = o
            for cntxt in set do    
                if (not contains) && (Context.Equal (first cntxt) (second cntxt) (third cntxt) label gssNode ast)
                then
                    contains <- true   
                    
            contains

        let addContext (label : Label)  (index : int) (node : Vertex) (ast : obj) =
            if index <= inputLength && index >=0 then
                if not <| containsContext index label node ast
                then
                    let cntxt = new Context(index, label, node, ast)
                    setU.[index].Add (label, node, ast)
                    setR.Enqueue(cntxt)  
        
        let getNodeP (label : Label) (left : obj) (right : obj) =
            let mutable rExt =
                printfn "This is debug"
                match right with
                | :? AST as a   -> Some(a.leftExt, a.rightExt)
                | :? Nodes as n -> Some(n.leftExt, n.rightExt) 
                | :? IntermidiateNode as i -> Some i.Extention
                | _ -> None
            let mutable result =  right
            if left <> null
            then
                let mutable lExt =    
                    match left with
                    | :? AST as a   -> (a.leftExt, a.rightExt)
                    | :? Nodes as n -> (n.leftExt, n.rightExt) 
                    | :? IntermidiateNode as i ->  i.Extention
                    | _ -> failwith "Unexpected type."
            
                result <- new IntermidiateNode(left, right, (label.Rule, label.Position), (fst lExt, (if rExt.IsNone then lExt else rExt.Value)|> snd)) 
            else 
                result <- new IntermidiateNode(left, right, (label.Rule, label.Position), (fst rExt.Value, snd rExt.Value))
            box <| result
                
            
        let getNodeT term b =
            if term.Equals epsilon
            then
                box <| new Nodes(term, null, null, b, b)
            else
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
                curLevel.Add res   
            res

        let containsEdge (b : Vertex) (e : Vertex) (ast : obj)=
            let edges = b.OutEdges
            let mutable result = false
            if edges.first <> Unchecked.defaultof<_>
            then
                if Vertex.Equal edges.first.Dest e && treeCompare edges.first.Ast ast
                then
                    result <- true
                else
                    if edges.other <> null
                    then
                        for edge in edges.other do
                            if Vertex.Equal edge.Dest e && treeCompare edge.Ast ast
                            then
                                result <- true
            result <- false
            result                        

        let findTree prod lExt rExt family =
            let result = ref None
            let currentNonTerm = parser.LeftSide.[prod]
            let mutable wasAdded = false
            for pair in setP do
                match snd pair with
                | :? AST as a -> 
                    if parser.LeftSide.[a.first.prod] = currentNonTerm && a.leftExt = lExt && a.rightExt = rExt
                    then
                        if a.first.prod = prod
                        then
                            wasAdded <- true
                        elif a.other <> null
                        then
                            for fam in a.other do
                                if fam.prod = prod
                                then
                                    wasAdded <- true
                        if not wasAdded
                        then
                            if a.other <> null
                            then
                                a.other <- Array.append a.other [|family|]
                            else
                                a.other <- [|family|]
                        result := Some a                            
                | _ -> failwith "Unexpected type of tree."
            let result =
                match !result with
                | Some tree -> tree
                | None -> new AST(family, null, lExt, rExt)
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
                        let temp : AST = unbox <| snd pair
                        addContext label (temp.rightExt - 1) u y
                if v.OutEdges.first <> Unchecked.defaultof<_>
                then
                    if v.OutEdges.other <> null
                    then
                        v.OutEdges.other <- Array.append v.OutEdges.other [|newEdge|]
                    else 
                        v.OutEdges.other <- [|newEdge|]
                else v.OutEdges.first <- newEdge
            v
          
        let pop (u : Vertex) (i : int) (z : obj) prod =
            if not (Vertex.Equal u dummyGSSNode) then
                let label = u.Value
                setP.Enqueue(u, z)
                let processEdge (edge : Edge) =
                    let y1 = getNodeP label edge.Ast z
                    addContext label i edge.Dest y1    
                processEdge u.OutEdges.first
                if u.OutEdges.other <> null then 
                    for edge in u.OutEdges.other do
                        processEdge edge

        let table = parser.Table
   
        let condition = ref false 
        let stop = ref false

        let rec dispatcher () =
            if setR.Count <> 0 then
                currentContext := setR.Dequeue()
                currentIndex := currentContext.Value.Index
                
                if !currentIndex <> !previousIndex 
                then
                    previousIndex := !currentIndex 
                currentGSSNode := currentContext.Value.Node
                currentLabel := currentContext.Value.Label
                currentN := currentContext.Value.Ast 
                currentR := null
                condition := false
            else 
                
                stop := true  
                
                printfn  "%A" setP
                let v,ast =
                    setP
                    |> Seq.find 
                        (fun (x,y) -> 
                            let ast = unbox y :> AST
                            x.Value.Rule = parser.StartRule && ast.leftExt = 0 && ast.rightExt = inputLength)
                let mutable resList = []
                let a = astTemp.Count
                for pair in astTemp do
                    let ast11 = unbox pair :> AST
                  //  let ast11 = unbox pair : AST      
                    if (ast11.first.prod = parser.StartRule) && ast11.leftExt = 0 && ast11.rightExt = inputLength
                    then
                        printfn "UAAAAAAAAAAA"
                        resList <- ast11 :: resList
                //resultAST := Some ast 
                resultAST := Some resList 
                              
        and processing () =  
            let getIndex(nTerm, term) = 
                let mutable index = nTerm
                index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                index <- index + term - parser.NonTermCount
                index

            if Array.length parser.Rules.[currentLabel.Value.Rule] = 0 
            then
                currentR := getNodeT epsilon !currentIndex
                currentN := getNodeP !currentLabel !currentN !currentR  
                //pop !currentGSSNode !currentIndex !currentN
                condition := true
            else
                if Array.length parser.Rules.[currentLabel.Value.Rule]  <> currentLabel.Value.Position
                then
                    if !currentIndex < inputLength 
                    then
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
                                        currentN := getNodeT (box <| tokens.[!currentIndex]) !currentIndex
                                        terminalNodes.Set !currentIndex (unbox <| !currentN)
                                    else 
                                        currentN := box <| terminalNodes.Item !currentIndex
                                else
                                    if (terminalNodes.Item !currentIndex).fst = null
                                    then
                                        currentR := getNodeT (box <| tokens.[!currentIndex]) !currentIndex
                                        terminalNodes.Set !currentIndex (unbox <| !currentR)
                                    else 
                                        currentR := box <| terminalNodes.Item !currentIndex
                                currentIndex := !currentIndex + 1
                                currentLabel.Value.Position <- currentLabel.Value.Position + 1
                                currentN := getNodeP !currentLabel !currentN !currentR  //нужно ли тут всегда создавать упакованную ячейку или если только правый уже есть
                                condition := false
                            else 
                                condition := true
                        else 
                            let index = getIndex(curSymbol, curToken)
                            let temp = table.[index]
                            currentGSSNode := create (new Label(currentLabel.Value.Rule, currentLabel.Value.Position + 1)) !currentGSSNode !currentIndex !currentN
                            if Array.length table.[index] <> 0 
                            then
                                for ruleN in table.[index] do
                                    let newLabel = new Label(ruleN, 0)
                                    addContext newLabel !currentIndex !currentGSSNode dummy
                                condition := true
                    else condition := true           
                else
                    let curRight = unbox <| !currentN
                    let resTree = handleIntermidiate curRight currentLabel.Value.Rule
                    //let resTree = findTree currentLabel.Value.Rule resTree.leftExt resTree.rightExt resTree
                    let resTree = new AST(resTree, null, resTree.leftExt, resTree.rightExt)
                    if resTree.leftExt = 0 && resTree.rightExt = inputLength then
                        astTemp.Add(resTree)
                    pop !currentGSSNode !currentIndex resTree currentLabel.Value.Rule
                    condition := true
        let control () =
            while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match !resultAST with
            | None -> Error ("String was not parsed")
            | Some res -> 
                    let r1 = new Tree<_> (tokens, res.[0], parser.Rules)
                    let r2 = new Tree<_> (tokens, res.[1], parser.Rules)
                    let rr = new List<Tree<'TokenType>>()
                    rr.Add(r1)
                    rr.Add(r2)
                    Success (rr)       