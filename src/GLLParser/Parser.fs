module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Generators.RNGLR.DataStructures
open Microsoft.FSharp.Collections

let inline pack2ToInt64 left right =  (int64 left <<< 32) ||| int64 right
let inline getLowBits long         = int32 <| (long &&& 0xffffffffL)
let inline getHighBits long        = int32 <| (long >>> 32)
let inline pack3ToInt64 p l r      = ((uint64 p) <<< 52) ||| ((uint64 l) <<< 26) ||| (uint64 r)
let inline getProduction long      = int(long >>> 52)
let inline getLeftExtension long   = int((long <<< 12) >>> 38)
let inline getRightExtension long  = int((long <<< 38) >>> 38)

type [<CustomEquality;CustomComparison>] IntermidiateNode =
    struct
        val LeftChild  : obj
        val RightChild : obj
        val Position   : Int64
        val Extension  : Int64
        override x.Equals(intermidiateNode) =
                match intermidiateNode with
                | :? AST as a -> (obj.ReferenceEquals(x, a))
                | :? Nodes as n -> (n.Equals x)
                | _ -> false
        interface System.IComparable with
            member x.CompareTo n =
                match n with
                | :? AST as a -> compare x.Extension a.extension
                | _ -> invalidArg "yobj" "cannot compare values of different types"
        new (l, r, p, e) = {LeftChild = l; RightChild = r; Position = p; Extension = e}
    end

[<AllowNullLiteral>]
type Vertex =
    val mutable OutEdges : UsualOne<Edge>
    val Level            : int
    val Label            : Int64
    static member Equal (v1 : Vertex) (v2 : Vertex) =
        v1.Level = v2.Level && v1.Label = v2.Label
    new (level, label) = {OutEdges = Unchecked.defaultof<_>; Level = level; Label = label}

and Edge =
    struct
        val Ast   : obj
        val Dest  : Vertex
        new (d, a) = {Dest = d; Ast = a}
    end

type VComparer() =
    interface IEqualityComparer<Vertex> with
     member x.Equals(a,b) = Vertex.Equal a b
     member x.GetHashCode y = y.GetHashCode()

type PoppedPair =
    struct
        val Node : Vertex
        val Ast  : obj
        new (n, a) = {Node = n; Ast = a}
    end

type PreviousContexts =
    struct
        val Label  : Int64
        val Vertex : Vertex
        val Ast    : obj
        new (l, v, a) = {Label = l; Vertex = v; Ast = a}
        member this.Equal l v a =
            this.Label = l && Vertex.Equal this.Vertex v && obj.ReferenceEquals(this.Ast, a)
    end

type Context =
    val Index         : int
    val Label         : Int64
    val Node          : Vertex
    val Ast           : obj
    static member Equal1 label1 (node1 : Vertex) (ast1 : obj) label2 (node2 : Vertex) (ast2 : obj) =
        label1 = label2 && Vertex.Equal node1 node2 && ast1.Equals ast2
    new (index, label, node, ast) = {Index = index; Label = label; Node = node; Ast = ast}

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of string

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
            iN.Position.ToString()
        | null -> "n"
        | :? Nodes as n -> (getHighBits n.extension).ToString() + (getLowBits n.extension).ToString() 
        | _ -> failwith "Unexpected ast"

    let rec dfs (u : Vertex) =
        was.Add (u, !curNum)
        if not <| levels.ContainsKey u.Level then levels.[u.Level] <- [!curNum]
        else
            levels.[u.Level] <- !curNum :: levels.[u.Level]
        //let labelStr = u.Label.labelToString()
        print <| sprintf "%d [label=\", %d\"]" !curNum  (*labelStr*) u.Level
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
        let setR = new Queue<Context>()   
        let setP = new System.Collections.Generic.Dictionary<Vertex,ResizeArray<obj>> (new VComparer())
        let astDictionary = new System.Collections.Generic.Dictionary<uint64,obj> ()
        let setU = Array.init (inputLength + 1) (fun _ -> new ResizeArray<PreviousContexts>())  //temp проверять не был ли уже такой добавлен
            
        let currentIndex = ref 0

        let currentrule = parser.Startrule
        let dummy = box <| null
        let dummyGSSNode = new Vertex(!currentIndex, (pack2ToInt64 -1 -1))
        let currentLabel = ref <| pack2ToInt64 currentrule 0
        
        let currentN = ref <| null
        let currentR = ref <| null

        let currentGSSNode = ref <| dummyGSSNode
        let currentContext = ref <| new Context(!currentIndex,!currentLabel,!currentGSSNode, dummy)
        
        let gss = Array.init inputLength (fun _ -> new Microsoft.FSharp.Collections.ResizeArray<Vertex>())
        let createdFamilies = Array.init parser.rulesCount (fun _ -> new Dictionary<Int64, Microsoft.FSharp.Collections.ResizeArray<Family>>())  

        let terminalNodes = new BlockResizeArray<Nodes>()
        let finalExtension = pack2ToInt64 0 inputLength
        
        let findFamily prod (nodes : Nodes) extension : Family =
            let mutable result = None
            if createdFamilies.[prod].ContainsKey extension
            then
                result <- 
                Microsoft.FSharp.Collections.ResizeArray.tryFind 
                    (fun (fam : Family) -> obj.ReferenceEquals(nodes.fst, fam.nodes.fst))  
                    <| createdFamilies.[prod].[extension]
            else
                createdFamilies.[prod].Add(extension, new Microsoft.FSharp.Collections.ResizeArray<Family>(5))
                result <- Some <| new Family(prod, nodes)
                createdFamilies.[prod].[extension].Add result.Value
            if result.IsNone
            then
                result <- Some <| new Family(prod, nodes)
                createdFamilies.[prod].[extension].Add result.Value
            result.Value

        let handleIntermidiate node prod = 
            let result = new ResizeArray<obj>()
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
                    getHighBits n.extension
                | :? AST as a -> getHighBits a.extension
                | _ -> failwith "Unexpected type."
            let r =
                match result.[result.Length - 1] with
                | :? Nodes as n ->
                    getLowBits n.extension
                | :? AST as a -> getLowBits a.extension 
                | _ -> failwith "Unexpected type."
            let extension = pack2ToInt64 l r
            let nodes = new Nodes(result, extension)
            let family = findFamily prod nodes extension
            family, extension
               
        let containsContext index label  (gssNode : Vertex) (ast : obj) =
            let set = setU.[index]
            let contains = set.Exists (fun cntxt -> cntxt.Equal label gssNode ast)
            contains

        let addContext label (index : int) (node : Vertex) (ast : obj) =
            if index <= inputLength + 1 && index >= 0 && not <| containsContext index label node ast
            then
                let cntxt = new Context(index, label, node, ast)
                setU.[index].Add (new PreviousContexts(label, node, ast))
                setR.Enqueue(cntxt)  
        
        let getNodeP label (left : obj) (right : obj) =      
            let rExt =
                match right with
                | :? AST as a -> a.extension
                | :? Nodes as n -> n.extension 
                | :? IntermidiateNode as i -> i.Extension
                | _ -> failwith "Unexpected type."
            let mutable result = right
            if left <> null
            then
                let lExt =    
                    match left with
                    | :? AST as a -> a.extension
                    | :? Nodes as n -> n.extension 
                    | :? IntermidiateNode as i -> i.Extension
                    | _ -> failwith "Unexpected type."

                result <- new IntermidiateNode(left, right, label, pack2ToInt64 (getHighBits lExt) (getLowBits rExt))
            else 
                result <- new IntermidiateNode(left, right, label, pack2ToInt64 (getHighBits rExt) (getLowBits rExt))
            box <| result
                
            
        let getNodeT index =
            let mutable result = (terminalNodes.Item index).fst
            if result = null
            then
                result <- box <| new Nodes(box <| tokens.[index], null, null, pack2ToInt64 index (index + 1))
                terminalNodes.Set !currentIndex (unbox <| result)
            else result <- box <| terminalNodes.Item index
            result
            
        let containsGSSNode l i =  
            let curLevel = gss.[i]
            let mutable result = curLevel.Find (fun v -> v.Label = l)
            if result = null
            then
                result <- new Vertex(i, l)  
                curLevel.Add result   
            result

        let containsEdge (b : Vertex) (e : Vertex) (ast : obj) =
            let edges = b.OutEdges
            edges.first <> Unchecked.defaultof<_>
            && ( Vertex.Equal edges.first.Dest e && ast.Equals edges.first.Ast 
                 || (edges.other <> null && edges.other |> Array.exists (fun edge -> Vertex.Equal edge.Dest e && ast.Equals edge.Ast)))

        let findTree prod extension family =      
            let key = pack3ToInt64 prod (getHighBits extension) (getLowBits extension)      
            let result = 
                if astDictionary.ContainsKey key
                then
                    let a = astDictionary.[key] :?> AST
                    if not <| (a.first.Equals family || (a.other <> null && a.other |> Array.exists(family.Equals)))
                    then
                        if a.other <> null
                        then a.other <- Array.append a.other [|family|]
                        else a.other <- [|family|]
                    a
                else
                    let value = new AST(family, null, extension)
                    astDictionary.Add(key, value)
                    value
            
            result

        let create label (u : Vertex) (index : int) (ast : obj) = 
            let v = containsGSSNode label index
            if not (containsEdge v u ast)
            then
                let newEdge = new Edge(u, ast)
                if setP.ContainsKey v
                then
                    let trees = setP.[v]
                    let handleTree ast2 =
                        let y = getNodeP label ast ast2
                        let temp : AST = unbox <| ast2
                        addContext label (getLowBits temp.extension) u y
                    trees |> ResizeArray.iter handleTree
                        
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
                let label = u.Label
                if setP.ContainsKey u
                then
                    let curTrees = setP.[u]
//                    if not <| curTrees.Exists (fun a -> obj.ReferenceEquals(a, z))
//                    then setP.[u].Add(z)
                    setP.[u].Add(z)
                else 
                    let value = new ResizeArray<obj>(5)
                    value.Add(z)
                    setP.Add(u, value)
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
            let rule = getHighBits !currentLabel
            let position = getLowBits !currentLabel
            if Array.length parser.rules.[rule] = 0 
            then
                currentR := getNodeT !currentIndex
                currentN := getNodeP !currentLabel !currentN !currentR  
                //pop !currentGSSNode !currentIndex !currentN
            else
                if Array.length parser.rules.[rule] <> position
                then
                    if !currentIndex < inputLength 
                    then
                        let curToken = parser.TokenToNumber tokens.[!currentIndex]
                        let curSymbol = parser.rules.[rule].[position]
                        if (parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol) && curSymbol = curToken //может, здесь нужен отдельный иф для проверки на совпадение текущего символа и токена
                        then
                            if !currentN = null 
                            then currentN := getNodeT !currentIndex
                            else currentR := getNodeT !currentIndex
                            currentIndex := !currentIndex + 1
                            currentLabel := pack2ToInt64 (rule) ((position) + 1)
                            if !currentR <> null
                            then currentN := getNodeP !currentLabel !currentN !currentR
                            condition := false
                        else 
                            let index = getIndex(curSymbol, curToken)
                            currentGSSNode := create (pack2ToInt64 (rule) (position + 1)) !currentGSSNode !currentIndex !currentN
                            if Array.length table.[index] <> 0 
                            then
                                let a rule = 
                                    let newLabel = pack2ToInt64 rule 0
                                    addContext newLabel !currentIndex !currentGSSNode dummy    
                                table.[index] |> Array.iter a
                    else condition := true
                                    
                else
                    let curRight = unbox <| !currentN
                    let resTree, extension = handleIntermidiate curRight (rule)
                    let resTree = findTree rule extension resTree
                    if  resTree.extension = finalExtension && rule = parser.Startrule
                    then resultAST := Some resTree
                    pop !currentGSSNode !currentIndex resTree
        let control () =
            while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match !resultAST with
            | None -> Error ("String was not parsed")
            | Some res -> 
                    let r1 = new Tree<_> (tokens, res, parser.rules)
                    Success (r1)       