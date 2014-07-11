module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Generators.RNGLR.DataStructures

type Label =  
    val Rule     : int
    val Position : int 
    static member Equal1 (label1 : Label) (label2 : Label) =
        let mutable result = false
        if label1.Rule = label2.Rule && label1.Position = label2.Position then result <- true
        result
    static member Equal2 (label1 : (int*int)) (label2 : Label) =
        let mutable result = false
        if fst label1 = label2.Rule && snd label1 = label2.Position then result <- true
        result 
    member this.labelToString () = 
        let str = "(" + this.Rule.ToString() + ", " + this.Position.ToString() + ")"
        str
    new (rule : int,position : int) = {Rule = rule; Position = position} 

type IntermidiateNode =
    val LeftChild  : obj
    val RightChild : obj
    val Position   : int * int
    val Extention  : int * int
    new (l, r, p, e) = {LeftChild = l; RightChild = r; Position = p; Extention = e}

[<AllowNullLiteral>]
type Vertex =
    val mutable OutEdges : UsualOne<Edge>
    val Level            : int
    val Value            : Label
    static member Equal (v1 : Vertex) (v2 : Vertex) =
        let mutable result = false
        if v1.Level = v2.Level && Label.Equal1 v1.Value v2.Value then result <- true
        result
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
    static member Equal1 (label1 : Label) (node1 : Vertex) (ast1 : obj) (label2 : Label) (node2 : Vertex) (ast2 : obj) inputLength =
        let mutable result = false
        if Label.Equal1 label1 label2 && Vertex.Equal node1 node2 && ast1.Equals ast2 then result <- true
        result
    static member Equal2 (label1 : (int*int)) (node1 : Vertex) (ast1 : obj) (label2 : Label) (node2 : Vertex) (ast2 : obj) inputLength =
        let mutable result = false
        if Label.Equal2 label1 label2 && Vertex.Equal node1 node2
        then 
            if ast1 = null && ast2 = null
            then result <- true
            elif ast1 <> null && ast2 <> null && ast1.Equals ast2
            then result <- true 
        result
    new (index, label, node, ast) = {Index = index; Label = label; Node = node; Ast = ast}

type ParseResult<'TokenType> =
    //| Success of List<Tree<'TokenType>>
    | Success of Tree<'TokenType>
    | Error of string
let mutable tempCount = 0

let drawDot (parser : ParserSource2<'TokenType>) (tokens : 'TokenType[])(path : string) gss =
    use out = new System.IO.StreamWriter (path)
    let was = new Dictionary<_,_>()
    let levels = new Dictionary<_,_>()
    out.WriteLine "digraph GSS {"
    let print s = out.WriteLine ("    " + s)
    let curNum = ref 0
    print "rankdir=RL"
    let getAstString (ast : obj) =
        match ast with
        | :? int as i when i >= 0 -> tokens.[i] |> parser.TokenToNumber |> parser.NumToString |> sprintf "%s"    
        | :? int as i when i < 0 -> "eps " + parser.NumToString (-i - 1)
        | :? AST as ast -> 
            let nonT = 
                if ast.first.prod < parser.LeftSide.Length then ast.first.prod
                else -1
            parser.NumToString parser.LeftSide.[nonT]
        | :? IntermidiateNode as iN ->
            iN.Position.ToString() + iN.Extention.ToString()
        | null -> "n"
        | :? Nodes as n -> n.leftExt.ToString() + n.rightExt.ToString() 
        | _ -> failwith "Unexpected ast"

    let rec dfs (u : Vertex) =
        was.Add (u, !curNum)
        if not <| levels.ContainsKey u.Level then levels.[u.Level] <- [!curNum]
        else
            levels.[u.Level] <- !curNum :: levels.[u.Level]
        let labelStr = u.Value.labelToString()
        print <| sprintf "%d [label=\"%s, %d\"]" !curNum  labelStr u.Level
        incr curNum
        if u.OutEdges.first <> Unchecked.defaultof<_> 
        then 
            handleEdge u u.OutEdges.first
            if u.OutEdges.other <> null then u.OutEdges.other |> Array.iter (handleEdge u)

    and handleEdge u (e : Edge) =
        let v = e.Dest
        if not <| was.ContainsKey v then dfs v
        print <| sprintf "%d -> %d [label=\"%s\"]" was.[u] was.[v] (getAstString e.Ast)
    for levels in gss do
        for v in levels do
            if not <| was.ContainsKey v then dfs v
    
    for level in levels do
        print <| sprintf "{rank=same; %s}" (level.Value |> List.map (fun (u : int) -> string u) |> String.concat " ")

    out.WriteLine "}"
    out.Close()

let buildAst<'TokenType> (parser : ParserSource2<'TokenType>) (tokens : seq<'TokenType>) : ParseResult<_> = 
    let tokens = Seq.toArray tokens
    let inputLength = Seq.length tokens
    let startNonTerm = parser.LeftSide.[parser.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parser.LeftSide)
    let resultAST = ref None
    let getEpsilon =

         let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i - 1))
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
        let setU = Array.init (inputLength + 1) (fun _ -> new List<(int * int) * Vertex * obj>())  
            
        let currentIndex = ref 0
        let previousIndex = ref 0

        let wasChanged = ref false

        let currentRule = parser.StartRule
        let dummy = box <| null
        let dummyGSSNode = new Vertex(new Label(-1, -1), !currentIndex)
        let currentLabel = ref <| new Label(currentRule, 0)
        
        let currentN = ref <| null
        let currentR = ref <| null

        let currentGSSNode = ref <| dummyGSSNode
        let currentContext = ref <| new Context(!currentIndex,!currentLabel,!currentGSSNode, dummy)
        
        let gss = Array.init inputLength (fun _ -> new ResizeArray<Vertex>())
        //let astTemp = new List<obj>()
        let familyTemp = Array.init parser.RulesCount (fun _ -> new List<Family>())  

        let terminalNodes = new BlockResizeArray<Nodes>()
        
        let findFamily prod (nodes : Nodes) lExt rExt : Family=
            let mutable result = None
            result <- 
                familyTemp.[prod]
                |> Seq.tryFind (fun fam -> fam.leftExt = lExt && fam.rightExt = rExt && nodes.fst.Equals fam.nodes.fst) 
            if result.IsNone
            then 
                result <- Some <| new Family(prod, nodes, lExt, rExt)
                familyTemp.[prod].Add(result.Value)
            let r = result.Value
            r

        let handleIntermidiate node prod = 
            let result = new List<obj>()
            let rec handle (o : obj) =
                if o <> null 
                then
                    match o with
                    | :? IntermidiateNode as i ->
                        let t : IntermidiateNode = unbox i
                        handle t.LeftChild
                        handle t.RightChild
                    | :? Nodes as n -> 
                        result.Add (box <| n)     
                    | :? AST as a -> 
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
            let nodes = new Nodes(result, l, r, -1)
            let family = findFamily prod nodes l r
            family
               
        let containsContext index (label : Label) (gssNode : Vertex) (ast : obj) =
            let set = setU.[index]
            let first (o, _, _)  = o
            let second (_, o, _) = o
            let third (_, _, o)  = o
            let contains = set.Exists (fun cntxt -> (Context.Equal2 (first cntxt) (second cntxt) (third cntxt) label gssNode ast inputLength))
            contains

        let addContext (label : Label) (index : int) (node : Vertex) (ast : obj) =
            if index <= inputLength + 1 && index >= 0 
            then
                if not <| containsContext index label node ast
                then
                    let cntxt = new Context(index, label, node, ast)
                    setU.[index].Add ((label.Rule, label.Position), node, ast)
                    setR.Enqueue(cntxt)  
        
        let getNodeP (label : Label) (left : obj) (right : obj) =      
            let rExt =
                match right with
                | :? AST as a -> Some(a.leftExt, a.rightExt)
                | :? Nodes as n -> Some(n.leftExt, n.rightExt) 
                | :? IntermidiateNode as i -> Some i.Extention
                | _ -> None
            let mutable result = right
            if left <> null
            then
                let lExt =    
                    match left with
                    | :? AST as a -> (a.leftExt, a.rightExt)
                    | :? Nodes as n -> (n.leftExt, n.rightExt) 
                    | :? IntermidiateNode as i -> i.Extention
                    | _ -> failwith "Unexpected type."
                if left <> null && right <> null && lExt = (0,1) && rExt.Value = (1,3)
                then
                    printfn "this is debug"
                result <- new IntermidiateNode(left, right, (label.Rule, label.Position), (fst lExt, snd rExt.Value))//(if rExt.IsNone then (snd lExt) else (rExt.Value)|> snd))) 
            else 
                result <- new IntermidiateNode(left, right, (label.Rule, label.Position), (fst rExt.Value, snd rExt.Value))
            box <| result
                
            
        let getNodeT index =
            let mutable result = (terminalNodes.Item index).fst
            if result = null
            then
                result <- box <| new Nodes(box <| tokens.[index], null, null, index, index + 1)
                terminalNodes.Set !currentIndex (unbox <| result)
            else result <- box <| terminalNodes.Item index
            result
            
        let containsGSSNode (l : Label) (i : int) =  
            let curLevel = gss.[i] 
            let mutable cond = true
            let mutable result = curLevel.Find (fun v -> Label.Equal1 v.Value l)
            if result = null
            then
                result <- new Vertex(l, i)  
                curLevel.Add result   
            result

        let containsEdge (b : Vertex) (e : Vertex) (ast : obj)=
            let edges = b.OutEdges
            let mutable result = false
            if edges.first <> Unchecked.defaultof<_>
            then
                if Vertex.Equal edges.first.Dest e && (*treeCompare edges.first.Ast ast inputLength*) ast.Equals edges.first.Ast
                then result <- true
                elif edges.other <> null
                then result <- edges.other |> Array.exists (fun edge -> Vertex.Equal edge.Dest e && (*treeCompare edge.Ast ast inputLength*) ast.Equals edge.Ast)
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
                        if a.first.Equals family
                        then wasAdded <- true
                        elif a.other <> null
                        then
                            for fam in a.other do
                                if fam.Equals family
                                then wasAdded <- true
                        if not wasAdded
                        then 
                            if a.other <> null
                            then a.other <- Array.append a.other [|family|]
                            else a.other <- [|family|]
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
                wasChanged := true
                let newEdge = new Edge(u, ast)
                for pair in setP do
                    if Vertex.Equal v (fst pair) 
                    then 
                        let y = getNodeP label ast (snd pair)
                        let temp : AST = unbox <| snd pair
                        addContext label (temp.rightExt) u y //temp temp temp temp
                if v.OutEdges.first <> Unchecked.defaultof<_>
                then
                    if v.OutEdges.other <> null
                    then v.OutEdges.other <- Array.append v.OutEdges.other [|newEdge|]
                    else v.OutEdges.other <- [|newEdge|]
                else v.OutEdges.first <- newEdge
            v

        let pop (u : Vertex) (i : int) (z : obj) =
            if not (Vertex.Equal u dummyGSSNode)
            then
                let label = u.Value
                setP.Enqueue(u, z)
                let processEdge (edge : Edge) =
                    let y1 = getNodeP label edge.Ast z
                    addContext label i edge.Dest y1    
                processEdge u.OutEdges.first
                if u.OutEdges.other <> null 
                then u.OutEdges.other |> Array.iter processEdge

        let table = parser.Table
        let condition = ref false 
        let stop = ref false

        let rec dispatcher () =
            if setR.Count <> 0
            then
                currentContext := setR.Dequeue()
                setU.[0].Count
                currentIndex := currentContext.Value.Index
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

            condition := true

            if Array.length parser.Rules.[currentLabel.Value.Rule] = 0 
            then
                currentR := getNodeT !currentIndex
                currentN := getNodeP !currentLabel !currentN !currentR  
                //pop !currentGSSNode !currentIndex !currentN
            else
                if Array.length parser.Rules.[currentLabel.Value.Rule] <> currentLabel.Value.Position
                then
                    if !currentIndex < inputLength 
                    then
                        let curToken = parser.TokenToNumber tokens.[!currentIndex]
                        let curSymbol = parser.Rules.[currentLabel.Value.Rule].[currentLabel.Value.Position]
                        if parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol 
                        then
                            if curSymbol = curToken
                            then
                                if !currentN = null 
                                then
                                    currentN := getNodeT !currentIndex
                                else currentR := getNodeT !currentIndex
                                currentIndex := !currentIndex + 1
                                currentLabel := new Label(currentLabel.Value.Rule, currentLabel.Value.Position + 1)
                                if !currentR <> null
                                then currentN := getNodeP !currentLabel !currentN !currentR
                                condition := false
                        else 
                            let index = getIndex(curSymbol, curToken)
                            let temp = table.[index]
                            currentGSSNode := create (new Label(currentLabel.Value.Rule, currentLabel.Value.Position + 1)) !currentGSSNode !currentIndex !currentN
                            if Array.length table.[index] <> 0 
                            then
                                let a rule = 
                                    let newLabel = new Label(rule, 0)
                                    addContext newLabel !currentIndex !currentGSSNode dummy    
                                table.[index] |> Array.iter a
                     else condition := true
                                    
                else
                    let curRight = unbox <| !currentN
                    let resTree = handleIntermidiate curRight currentLabel.Value.Rule
                    let resTree = findTree currentLabel.Value.Rule resTree.leftExt resTree.rightExt resTree
                    if resTree.leftExt = 0 && resTree.rightExt = inputLength && currentLabel.Value.Rule = parser.StartRule
                    then resultAST := Some resTree
                    pop !currentGSSNode !currentIndex resTree
        let control () =
            while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match !resultAST with
            | None -> Error ("String was not parsed")
            | Some res -> 
                    let r1 = new Tree<_> (tokens, res, parser.Rules)
                    //let rr = new List<Tree<'TokenType>>()
                   // drawDot parser tokens "res.dot" gss
                    //rr.Add(r1)
                    Success (r1)       