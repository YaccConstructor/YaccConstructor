module Yard.Generators.Common.ASTGLLFSA
open System
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Microsoft.FSharp.Reflection


[<AllowNullLiteral>]
type INode = 
    interface
    abstract member getExtension : unit -> int64<extension>
    end

[<AllowNullLiteral>]
type NonTerminalNode =
    val Extension : int64<extension>
    val Name      : int<positionInGrammar>
    val mutable First     : PackedNode 
    val mutable Others    : ResizeArray<PackedNode> 
    member this.AddChild (child : PackedNode) : unit = 
        if this.First <> Unchecked.defaultof<_>
        then 
            if this.Others <> Unchecked.defaultof<_>
            then
                this.Others.Add child
            else
                this.Others <- new ResizeArray<PackedNode>()
                this.Others.Add child
        else this.First <- child

    member this.MapChildren func =
        seq {
            if this.First <> Unchecked.defaultof<_>
            then
                yield func this.First
                if this.Others <> Unchecked.defaultof<_>
                then
                    for child in this.Others do
                        yield func child
        }

    interface INode with
        member this.getExtension () = this.Extension
    new (name, extension) = {Name = name; Extension = extension; First = Unchecked.defaultof<_>; Others = Unchecked.defaultof<_>}
    
and TerminalNode =
    val Name : int<token>
    val Extension : int64<extension>
    interface INode with
        member this.getExtension () = this.Extension
    new (name, extension) = {Name = name; Extension = extension}

and EpsilonNode =
    val Extension : int64<extension>
    interface INode with
        member this.getExtension () = this.Extension
    new (extension) = {Extension = extension}

and PackedNode = 
    val State : int<positionInGrammar>
    val mutable Left : INode
    val mutable Right : INode
    interface INode with
        member this.getExtension () = this.Right.getExtension ()
    new (state, left, right) = {State = state; Left = left; Right = right}

and IntermidiateNode = 
    val State     : int<positionInGrammar>
    val Nonterm   : int<positionInGrammar>
    val Extension : int64<extension>
    val mutable First     : PackedNode
    val mutable Others    : ResizeArray<PackedNode>
    interface INode with
        member this.getExtension () = this.Extension
    member this.AddChild (child : PackedNode) : unit = 
        if this.First <> Unchecked.defaultof<_>
        then 
            if this.Others <> Unchecked.defaultof<_>
            then
                this.Others.Add child
            else
                this.Others <- new ResizeArray<PackedNode>()
                this.Others.Add child
        else this.First <- child
    member this.MapChildren func =
        seq {
            if this.First <> Unchecked.defaultof<_>
            then
                yield func this.First
                if this.Others <> Unchecked.defaultof<_>
                then
                    for child in this.Others do
                        yield func child
        }
    new (state, nonterm, extension) = {Nonterm = nonterm; State = state; Extension = extension; First = Unchecked.defaultof<_>; Others = Unchecked.defaultof<_>}
    

type private DotNodeType = Packed | NonTerminal | Intermidiate | Terminal | Epsilon

//type ReducedTree = 
//    Term of string * TerminalNode
//    | NonTerm of string * NonTerminalNode * list<ReducedTree>

let inline packExtension left right : int64<extension> =
    LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
    
let inline getRightExtension (long : int64<extension>) : int<positionInInput> =
     (int64 long) &&& 0xffffffffL |> int |> LanguagePrimitives.Int32WithMeasure
     
let inline getLeftExtension (long : int64<extension>) : int<positionInInput> =
    (int64 long) >>> 32 |> int |> LanguagePrimitives.Int32WithMeasure

let inline getRule packedValue = int packedValue >>> 16
let inline getPosition (packedValue : int) = int (packedValue &&& 0xffff)

let gllNodeToGlr (node: INode) rightToRule intToString termToIndex =
    let rec handleNode (node : INode) =
        match node with
        | :? EpsilonNode -> 
            new AstNode.Epsilon(-1) :> AstNode.AstNode |> Some
        | :? TerminalNode as term ->
            new AstNode.Terminal(termToIndex term) :> AstNode.AstNode |> Some
        | :? NonTerminalNode as nonTerm ->
            let children = collectChildren nonTerm.First
            let childToString (child : INode) =
                 match child with
                 | :? TerminalNode as term -> intToString <| int term.Name
                 | :? NonTerminalNode as nonTerm -> intToString <| int nonTerm.Name
                 | _ -> failwith "unsupported type of node"
            let childrenString = children |> Seq.map childToString |> String.concat " "
            if Seq.isEmpty children
            then
                None
            else
                let nodes = new AstNode.Nodes(children |> Seq.map handleNode |> Seq.choose id |> Array.ofSeq)
                let family = new AstNode.Family(rightToRule childrenString, nodes)
                new AstNode.AST (Array.singleton <| family) :> AstNode.AstNode |>Some 
        | other -> failwith <| sprintf "Can not handle %A node" other
    and collectChildren (node: INode) =
        match node with
        | :? PackedNode as packed -> 
            Seq.append <|| (collectChildren packed.Left, collectChildren packed.Right)                
        | :? IntermidiateNode as inter ->
            inter.MapChildren collectChildren|> Seq.concat        
        | :? TerminalNode as term ->
            if int term.Name = -1
            then Seq.empty
            else term :> INode |> Seq.singleton
        | :? EpsilonNode ->
            Seq.empty 
        | otherNode -> Seq.singleton otherNode

    match handleNode node with
        | Some converted -> converted
        | None           -> failwith "Start rule can not be anepsilon rule"

[<Struct>]
type NumNode<'vtype> =
    val Ancestor : int
    val Node : 'vtype
    new (ancestor, node) = {Ancestor = ancestor; Node = node} 

type TranslateArguments<'Token, 'Position, 'Result> = {
    tokenToRange : 'Token -> 'Position * 'Position
    zeroPosition : 'Position
    createErrorToken : ('Token -> 'Token) option
    intToString : int -> string
    rightToRule : string -> int
    rules : int array array
    translate : AST.TranslateArguments<'Token, 'Position> -> AST.Tree<'Token> -> AST.ErrorDictionary<'Token>  -> 'Result
    withErrors : bool
}

[<Struct>]
type NodeAncestor =
    val IsLeft : bool
    val Ancestor : PackedNode
    val Node : INode
    new (node, ancestor, isLeft) = {Ancestor = ancestor; Node = node; IsLeft = isLeft}

let isDummy (n:INode) = match n with :? TerminalNode as t -> t.Extension = packExtension -1 -1 | _ -> false

type TreeNode = 
    | SPPFNonterminal of TreeNode[] * string * NonTerminalNode
    | SPPFTerminal of string * int * int

[<Struct>]
type Info =
    val LastStem : int
    val Lengths : ResizeArray<int>
    val Tree : TreeNode[]
    new (lastStem, lengths, tree) = {Lengths = lengths; LastStem = lastStem; Tree = tree}

let dummyNonterm = -3
let dictionary = new Dictionary<_,_>()
let rec getBestTree (intToString : Dictionary<int,string>) (currentNonterm : int) (node : INode) : Info =
    let contains, value = dictionary.TryGetValue(node)
    if contains then value else
    let newInfo = new Info() |> ref
    match node with
    | :? NonTerminalNode as n -> 
        if int n.Name = 6 && n.Extension = 481036337303L<extension>
        then
            printfn "name is 6"
        let children = 
            if n.Others <> null
            then
                n.Others |> Array.ofSeq
            else
                [||]
        let children = Array.append children [|n.First|] 
        let isStem = (intToString.[int n.Name]).Contains("toCount")
        let currentNonterm = if isStem then int n.Name else dummyNonterm
        let subtreeToChoose, info =
            children
            |> Array.map(fun x -> x, getBestTree intToString currentNonterm x)
            |> Array.maxBy(fun (_,x) -> if x.Lengths.Count > 0 then x.Lengths |> Seq.averageBy(fun x -> float x ) else 0.0)
        n.Others <- null
        n.First <- subtreeToChoose

        let st, stack = 
            if isStem
            then
                let stack = new ResizeArray<_>(info.Lengths)
                if int info.LastStem = int n.Name
                then
                    stack.[stack.Count - 1] <- stack.[stack.Count - 1] + 1
                else
                    stack.Add(1)
                int n.Name,stack
            else
                dummyNonterm, info.Lengths
            
        newInfo := new Info(st, stack, [|SPPFNonterminal(info.Tree, intToString.[int n.Name], n)|])
        
    | :? PackedNode as p ->
        let left = getBestTree intToString currentNonterm p.Left
        let right = getBestTree intToString currentNonterm p.Right
        let stack = new ResizeArray<_>()
        
        if currentNonterm = right.LastStem
        then
            for i in right.Lengths do stack.Add(i)
            for i in left.Lengths do stack.Add(i)
        else
            for i in left.Lengths do stack.Add(i)
            for i in right.Lengths do stack.Add(i)

        newInfo :=
            new Info ((if currentNonterm = right.LastStem then right.LastStem
                       elif currentNonterm = left.LastStem then left.LastStem
                       else dummyNonterm),
                      stack,
                      Array.append left.Tree right.Tree)
    | :? IntermidiateNode as i ->
        let children = 
            if i.Others <> null
            then
                i.Others |> Array.ofSeq
            else
                [||]
        let children = Array.append children [|i.First|] 
        let subtreeToChoose, info =
            children
            |> Array.map(fun x -> x, getBestTree intToString currentNonterm x)
            |> Array.maxBy(fun (_,x) -> if x.Lengths.Count > 0 then x.Lengths |> Seq.averageBy(fun x -> float x ) else 0.0)
        i.Others <- null
        i.First <- subtreeToChoose
        newInfo := info
    | :? TerminalNode as t ->
        if int t.Name <> -1
        then
            newInfo := new Info(0,new ResizeArray<_>(),[|SPPFTerminal(intToString.[int t.Name], getLeftExtension t.Extension |> int, getRightExtension t.Extension |> int)|])
        else
            newInfo := new Info(0,new ResizeArray<_>(),[||])
    | :? EpsilonNode as e ->
        newInfo := new Info(0,new ResizeArray<_>(),[|SPPFTerminal("eps", -1, -1)|])
    | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()
    
    dictionary.Add(node, !newInfo)
    !newInfo
    
[<AllowNullLiteral>]
type Tree<'TokenType> (roots : INode[], unpackPos, indToString) =
    member this.MinimizeBinarized() =
        ()
    member this.GetBestTree() = 
        let result = SPPFTerminal("dummy", -1,-1) |> ref
        let func () =
            try
                result := (getBestTree indToString dummyNonterm roots.[0]).Tree.[0]
            with
            | e -> printfn "%A" e
        let stackSize : int = 2000000000 
        let thread = new System.Threading.Thread(func, stackSize)
        thread.Start()
        thread.Join()
        !result

    member this.AstToDot (*(indToString : Dictionary<int,_>)*) (path : string) =
        use out = new System.IO.StreamWriter (path : string)
        out.WriteLine("digraph AST {")

        let differentTreesCount = ref 1
        let createNode isRoot num isAmbiguous nodeType (str : string) =
            let label =
                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
                let cur2 = 
                    if not isAmbiguous then cur
                    else cur + " !"
                if isRoot then cur2 + " root"
                else cur2
            let shape =
                match nodeType with
                | Intermidiate -> ",shape=box"
                | Packed -> ",shape=point"
                | Terminal -> ",shape=box"
                | Epsilon -> ",shape=box"
                | NonTerminal -> ",shape=oval"
            let color =
                if not isAmbiguous then 
                    if isRoot then ",style=\"filled\",fillcolor=green"
                    else ""
                else 
                    if isRoot then ",style=\"filled\",fillcolor=yellow"
                    else ",style=\"filled\",fillcolor=red"
                
            out.WriteLine ("    " + num.ToString() + " [label=\"" + label + "\"" + color + shape + "]")

        let createEdge (b : int) (e : int) isBold (str : string) =
            let label = str.Replace("\n", "\\n").Replace ("\r", "")
            let bold = 
                if not isBold then ""
                else "style=bold,width=10,"
            out.WriteLine ("    " + b.ToString() + " -> " + e.ToString() + " [" + bold + "label=\"" + label + "\"" + "]")
        
        let nodeQueue = new Queue<NumNode<INode>>()
        let used = new Dictionary<_,_>()
        let num = ref -1
        for root in roots do
            nodeQueue.Enqueue(new NumNode<INode>(-1, root))
        let isDummy (n:INode) = match n with :? TerminalNode as t -> t.Extension = packExtension -1 -1 | _ -> false

        while nodeQueue.Count <> 0  do
            let currentPair = nodeQueue.Dequeue()
            let key = ref 0
            if currentPair.Node <> null && used.TryGetValue(currentPair.Node, key)
            then
                if currentPair.Ancestor <> -1
                then
                    createEdge currentPair.Ancestor !key false ""
            else    
                num := !num + 1
                used.Add(currentPair.Node, !num)
                match currentPair.Node with 
                | :? NonTerminalNode as a -> 
                    let isAmbiguous = a.Others <> null
                    let isRoot = currentPair.Ancestor = -1
                    createNode isRoot !num isAmbiguous NonTerminal (sprintf "%s,%s,%s" (indToString.[int a.Name]) (unpackPos <| getLeftExtension a.Extension) (unpackPos <| getRightExtension a.Extension))
                    if not isRoot
                    then
                        createEdge currentPair.Ancestor !num false ""
                    if a.First <> Unchecked.defaultof<_>
                    then
                        nodeQueue.Enqueue(new NumNode<INode>(!num, a.First))
                    if isAmbiguous
                    then
                        //printfn "* %i" (a.Others.Count+1)
                        differentTreesCount := !differentTreesCount * (a.Others.Count+1)
                        for n in a.Others do
                            nodeQueue.Enqueue(new NumNode<INode>(!num, n))
                | :? PackedNode as p ->
                    createNode false !num false Packed ""
                    createEdge currentPair.Ancestor !num false ""
                    if not <| isDummy p.Left then 
                        nodeQueue.Enqueue(new NumNode<INode>(!num, p.Left))
                    if not <| isDummy p.Right then 
                        nodeQueue.Enqueue(new NumNode<INode>(!num, p.Right))
                | :? IntermidiateNode as i ->
                    createNode false !num false Intermidiate (sprintf "%i,%s,%s" i.State (unpackPos <| getLeftExtension i.Extension) (unpackPos <| getRightExtension i.Extension))
                    createEdge currentPair.Ancestor !num false ""
                    if i.First <> Unchecked.defaultof<_>
                    then
                        nodeQueue.Enqueue(new NumNode<INode>(!num, i.First))
                    if i.Others <> null
                    then
                        printfn "* %i" (i.Others.Count+1)
                        differentTreesCount := !differentTreesCount * (i.Others.Count+1)
                        for nodes in i.Others do
                            nodeQueue.Enqueue(new NumNode<INode>(!num, nodes))
                | :? TerminalNode as t ->
                    if t.Extension <> packExtension -1 -1 
                    then
                        if t.Name <> -2<token>
                        then
                            createNode false !num false Terminal (sprintf "%s,%s,%s" (indToString.[int t.Name]) (unpackPos <| getLeftExtension t.Extension) (unpackPos <| getRightExtension t.Extension))
                            createEdge currentPair.Ancestor !num false ""
                        else
                            createNode false !num false Terminal ("dummy")
                            createEdge currentPair.Ancestor !num false ""
                    else
                        ()
                | :? EpsilonNode as e ->
                    if e.Extension <> packExtension -1 -1 
                    then
                        createNode false !num false Epsilon ((sprintf "epsilon,%s" (unpackPos <| getRightExtension e.Extension) ))
                        createEdge currentPair.Ancestor !num false ""
                    else
                        ()
                //                            createNode !num false Terminal ("dummy")
//                            createEdge currentPair.Num !num false ""

                | null -> ()
                | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()
        out.WriteLine("}")
        out.Close()
        //printfn "Different trees count: %i" !differentTreesCount

    (*member this.ReduceTree (tokenToNumber : 'TokenType -> int) (indToString : int -> string) : ReducedTree =
        let rec cleanTree (st : INode)  =             
            match st with 
            | :? IntermidiateNode as i ->
                cleanTree i.First   
            | :? TerminalNode as t ->
                if t.Name <> -1 
                then 
                    seq { yield (ReducedTree.Term((indToString <| tokenToNumber tokens.[t.Name]), t))}
                else    
                    Seq.empty
            | :? PackedNode as p ->
                Seq.append (cleanTree p.Left) (cleanTree p.Right)
            | :? NonTerminalNode as n ->
                let child = cleanTree n.First
                seq{yield ReducedTree.NonTerm(indToString (int n.Name), n, Seq.toList child )}
            | _ -> failwith "Unexpected node type."
        Seq.head <| (cleanTree root)
    member this.ReducedTreeToDot (tree : ReducedTree)  (tokenData : 'TokenType -> obj) (path : string) =
        use out = new System.IO.StreamWriter (path : string)
        out.WriteLine("digraph AST {")
        let createNode num isAmbiguous nodeType (str : string) =
            let label =
                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
                if not isAmbiguous then cur
                else cur + " !"
            let shape =
                match nodeType with
                | Terminal -> ",shape=box"
                | NonTerminal -> ",shape=point"
            let color = ""
            out.WriteLine ("    " + num.ToString() + " [label=\"" + label + "\"" + color + shape + "]")
        let createEdge (b : int) (e : int) isBold (str : string) =
            let label = str.Replace("\n", "\\n").Replace ("\r", "")
            let bold = 
                if not isBold then ""
                else "style=bold,width=10,"
            out.WriteLine ("    " + b.ToString() + " -> " + e.ToString() + " [" + bold + "label=\"" + label + "\"" + "]")
        
        let nodeQueue = new Queue<NumNode<ReducedTree>>()
        let used = new Dictionary<_,_>()
        let num = ref -1
        nodeQueue.Enqueue(new NumNode<ReducedTree>(!num, tree))
        while nodeQueue.Count <> 0 do
            let currentPair = nodeQueue.Dequeue()
            let key = ref 0
            if !num <> -1
            then
                if used.TryGetValue(currentPair.Node, key)
                then
                    createEdge currentPair.Num !key false ""
                else     
                    num := !num + 1
                    used.Add(currentPair.Node, !num)
                    match currentPair.Node with 
                    | NonTerm(name, node, children) -> 
                        createNode !num false NonTerminal name
                        createEdge currentPair.Num !num false ""
                        for c in children do
                            nodeQueue.Enqueue(new NumNode<ReducedTree>(!num, c))
                    | Term(name, tnode) ->
                        if tnode.Name <> -1
                        then
                            createNode !num false Terminal (name )
                            createEdge currentPair.Num !num false ""
                        else
                            createNode !num false Terminal ("epsilon")
                            createEdge currentPair.Num !num false ""
                       
                    | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()
            else
                num := !num + 1
                match currentPair.Node with
                | NonTerm(name, node, children) -> 
                    createNode !num false NonTerminal name
                    for c in children do
                        nodeQueue.Enqueue(new NumNode<_>(!num, c))
                | _ -> ()
        out.WriteLine("}")
        out.Close()
    *)
    (*
    /// Returns all paths found for specific nonterminal
    member this.GetStrings nTerm numToString = 
        //todo: add cycle detection
        let q = new ResizeArray<_>()
        let mutable cond = true
        let rec findNodes (node : INode) = 
            match node with
            | :? NonTerminalNode as n ->
                if n.Name = nTerm
                then q.Add n
                else 
                    findNodes n.First
                    if n.Others <> Unchecked.defaultof<_>
                    then
                        for ch in n.Others do
                            findNodes ch
            | :? TerminalNode as t -> ()
            | :? PackedNode as p ->
                findNodes p.Left
                findNodes p.Right
            | :? IntermidiateNode as i ->
                findNodes i.First
                if i.Others <> Unchecked.defaultof<_>
                then
                    for ch in i.Others do
                        findNodes ch
        findNodes root
        let results = Dictionary<INode,_>()
        let index = ref 0
        let res = new HashSet<string> ()
        let rec extractPath (node : INode) : HashSet<string> =  
            match node with
            | :? NonTerminalNode as n ->
                let isGot,value = results.TryGetValue n
                if isGot then
                    if value = Unchecked.defaultof<_> |> not then value else
                    failwith "cycle detected"
                else
                    results.Add(n, Unchecked.defaultof<_>)
                    let paths = new HashSet<string> (extractPath n.First)
                    if n.Others <> Unchecked.defaultof<_>
                    then n.Others.ForEach (fun o -> paths.UnionWith(extractPath o))
                    results.Remove n |> ignore
                    results.Add(n, paths)
                    paths
            | :? TerminalNode as t -> if t.Name <> -1 
                                      then
                                          let qwqw = tokens.[t.Name]
                                          let res = new HashSet<string> ([numToString tokens.[t.Name]])
                                          res
                                      else new HashSet<string> ()
            | :? PackedNode as p ->
                let isGot,value = results.TryGetValue p
                if isGot then
                    if value = Unchecked.defaultof<_> |> not then value else
                    failwith "cycle detected"
                else
                    results.Add(p, Unchecked.defaultof<_>)
                    let leftPaths = extractPath p.Left
                    let rightPaths = extractPath p.Right
                    let paths = ref (new HashSet<string> ())
                    if leftPaths.Count = 0 then paths := rightPaths
                    elif rightPaths.Count = 0 then paths := leftPaths
                    else leftPaths 
                         |> Seq.iter (fun (l:string) -> rightPaths
                                                        |> Seq.iter (fun (r:string) -> (!paths).Add(l+r) |> ignore ))
                    results.Remove p |> ignore
                    results.Add(p, !paths)
                    !paths
                (*let e = p.getExtension()
                if cycleNode.Contains e
                then
                    extractPath p.Left
                else
                    cycleNode.Add e
                    extractPath p.Left
                    extractPath p.Right*)
            | :? IntermidiateNode as i ->
                let isGot,value = results.TryGetValue i

                if isGot then
                    if value = Unchecked.defaultof<_> |> not then value else
                    failwith "cycle detected"
                else
                    results.Add(i, Unchecked.defaultof<_>)
                    let paths = new HashSet<string> (extractPath i.First)
                    if i.Others <> Unchecked.defaultof<_>
                    then i.Others.ForEach (fun o -> paths.UnionWith(extractPath o))
                    results.Remove i |> ignore
                    results.Add(i, paths)
                    paths
        
        for i in 0..q.Count-1 do
            index := i
            extractPath q.[i] |> res.UnionWith
        Seq.toList res
    *)
    (*
    member this.GetPath nTerm numToString = 
        let q = new ResizeArray<_>()
        let mutable cond = true
        let rec findNodes (node : INode) = 
            match node with
            | :? NonTerminalNode as n ->
                if n.Name = nTerm
                then q.Add n
                else 
                    findNodes n.First
                    if n.Others <> Unchecked.defaultof<_>
                    then
                        for ch in n.Others do
                            findNodes ch
            | :? TerminalNode as t -> ()
            | :? PackedNode as p ->
                findNodes p.Left
                findNodes p.Right
            | :? IntermidiateNode as i ->
                findNodes i.First
                if i.Others <> Unchecked.defaultof<_>
                then
                    for ch in i.Others do
                        findNodes ch
        findNodes root
        let cycleNode = ResizeArray<_>()
        let index = ref 0
        let res = Array.init q.Count (fun _ -> new ResizeArray<_>())
        let rec extractPath (node : INode) : HashSet<_> =  
            match node with
            | :? NonTerminalNode as n ->
                let paths = new HashSet<string> (extractPath n.First)
                if n.Others <> Unchecked.defaultof<_>
                then n.Others.ForEach (fun o -> paths.UnionWith(extractPath o))
                paths
            | :? TerminalNode as t -> res.[!index].Add (getLeftExtension t.Extension)
            | :? PackedNode as p ->
                let e = p.getExtension()
                if cycleNode.Contains e
                then
                    extractPath p.Left
                else
                    cycleNode.Add e
                    extractPath p.Left
                    extractPath p.Right
            | :? IntermidiateNode as i ->
                extractPath i.First
                if i.Others <> Unchecked.defaultof<_>
                then
                    for ch in i.Others do
                        extractPath ch
        for i in 0..q.Count-1 do
            index := i
            extractPath q.[i]
        res
    *)
    member this.CountCounters  =
        let nodesCount = ref 0
        let edgesCount = ref 0
        let termsCount = ref 0
        let ambiguityCount = ref 0
        let isDummy (n:INode) = match n with :? TerminalNode as t -> t.Extension = packExtension -1 -1 | _ -> false

        let nodeQueue = new Queue<_>()
        let visited = new HashSet<_>()
        let rootsLeft = ref roots.Length
        for root in roots do
            nodeQueue.Enqueue(root)
        while nodeQueue.Count <> 0 do
            let currentNode = nodeQueue.Dequeue()
            if currentNode = null then () else
            if !rootsLeft <= 0 then incr edgesCount
            decr rootsLeft
            if not <| visited.Contains(currentNode)
            then
                visited.Add(currentNode) |> ignore
                incr nodesCount
                match currentNode with 
                | :? NonTerminalNode as a -> 
                    nodeQueue.Enqueue(a.First)
                    if a.Others <> Unchecked.defaultof<_>
                    then
                        incr ambiguityCount
                        for n in a.Others do
                            nodeQueue.Enqueue(n)
                | :? PackedNode as p ->
                    if not <| isDummy p.Left then nodeQueue.Enqueue(p.Left)
                    nodeQueue.Enqueue(p.Right)
                | :? IntermidiateNode as i ->
                    nodeQueue.Enqueue(i.First)
                    if i.Others <> Unchecked.defaultof<_>
                    then
                        incr ambiguityCount
                        for nodes in i.Others do
                            nodeQueue.Enqueue(nodes)
                | :? TerminalNode as t ->
                    incr termsCount   
                | :? EpsilonNode as e ->
                    incr termsCount   
                                             
                | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()

        !nodesCount, !edgesCount, !termsCount, !ambiguityCount 

    member this.ChooseSingleAst isErrorToken =
        let rec handleNode (node: INode) =
            match node with
            | :? EpsilonNode -> 0
            | :? TerminalNode as term -> if isErrorToken term.Name then 1 else 0
            | :? NonTerminalNode as nonTerm -> 
                let minNode, minValue =
                    nonTerm.MapChildren (fun n -> n, handleNode n.Left + handleNode n.Right)
                    |> Seq.minBy snd
                nonTerm.First <- minNode
                nonTerm.Others <- null
                minValue
            | :? PackedNode as packed ->
                handleNode packed.Left + handleNode packed.Right
            | :? IntermidiateNode as inter ->
                inter.MapChildren handleNode |> Seq.sum
            | _ -> failwith "unsupported node type"

        roots |> Array.map handleNode |> ignore

    member this.StringRepr intToString = 
        let ident i = String.replicate (i * 2) " "
        let idented = sprintf "%s%s" << ident
        let rec handleNode (node: INode) idents =
                match node with
                | :? EpsilonNode -> 
                    sprintf "EPS\n" |> idented idents
                | :? TerminalNode as term ->
                    sprintf "T(%s)\n"
                            <| intToString (int term.Name) 
                        |> idented idents
                | :? NonTerminalNode as nonTerm ->
                    let chidrenStringified =
                        nonTerm.MapChildren(fun packed -> handleNode packed <| idents + 1) |> String.concat ""
                    sprintf "NT(%s):\n%s"
                            <| intToString (int nonTerm.Name) 
                            <| chidrenStringified
                        |> idented idents
                | :? PackedNode as packed ->
                    let stringifyChild name node=
                        sprintf "%s:\n%s"
                                <| (name |> idented 1)
                                <| handleNode node (idents + 2)
                            |> idented idents
                    sprintf "P:\n%s%s"
                            <| stringifyChild "L" packed.Left                    
                            <| stringifyChild "R" packed.Right
                        |> idented idents
                | :? IntermidiateNode as inter ->
                    let childrenStringified = 
                         inter.MapChildren(fun packed -> handleNode packed <| idents + 1) |> String.concat ""
                    sprintf "IN:\n%s"
                            <| childrenStringified
                        |> idented idents   
                | _ -> failwith "unsupported node type"              
        roots
            |> Array.mapi (fun i r -> sprintf "R(%i):\n%s" i <| handleNode r 1)
            |> String.concat ""

    member this.SelectBiggestRoot() = 
        roots
        |> Array.map (fun root -> getRightExtension <| root.getExtension(), root)
        |> Seq.maxBy fst
        |> snd

    member this.Translate tokens (arguments: TranslateArguments<_,_,_>) =
        let tokenToString (token : 'a) =
            match FSharpValue.GetUnionFields(token, typeof<'a>) with
                | case, _ -> case.Name.ToUpper()
        let termToIndex (term : TerminalNode) =
            let tokenIndex = (term.Extension |> getRightExtension |> int) - 1
            if arguments.withErrors
            then
                if arguments.intToString <| int term.Name <> "ERROR" then 2 * tokenIndex else 2 * tokenIndex + 1
            else
                tokenIndex
        let toRnglTree (root : AstNode.AstNode) =
            let handleToken token =
                if arguments.withErrors && tokenToString token <> "RNGLR_EOF"
                then [|token; arguments.createErrorToken.Value token|]
                else [|token|]
            let tokensWithErrors =
                tokens
                |> Array.map handleToken
                |> Array.concat
            new AST.Tree<_>(tokensWithErrors, root, arguments.rules)

        let root = this.SelectBiggestRoot()
        let glrRoot = gllNodeToGlr root arguments.rightToRule arguments.intToString termToIndex 
        let glrTree = toRnglTree glrRoot

        let translateArgs : AST.TranslateArguments<_,_> = {        
            tokenToRange = arguments.tokenToRange
            zeroPosition = arguments.zeroPosition
            clearAST = false
            filterEpsilons = true
        }
        let errorDict = new AST.ErrorDictionary<_>()
        let translated = arguments.translate translateArgs glrTree errorDict
        translated


type FSAParseResult<'a> =
    | Success of Tree<'a>
    | Error of string