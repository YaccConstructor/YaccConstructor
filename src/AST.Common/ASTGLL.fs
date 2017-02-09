module Yard.Generators.Common.ASTGLL
open System
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common

[<AllowNullLiteral>]
type INode = 
    interface
    abstract member getExtension : unit -> int64<extension>
    end

[<AllowNullLiteral>]
type NonTerminalNode =
    val Extension : int64<extension>
    val Name      : int
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
    interface INode with
        member this.getExtension () = this.Extension
    

    new (name, extension) = {Name = name; Extension = extension; First = Unchecked.defaultof<_>; Others = Unchecked.defaultof<_>}
    
and TerminalNode =
    val Name : int
    val Extension : int64<extension>
    interface INode with
        member this.getExtension () = this.Extension
    new (name, extension) = {Name = name; Extension = extension}

and PackedNode = 
    val Production : int
    val Left : INode
    val Right : INode
    interface INode with
        member this.getExtension () = this.Right.getExtension ()
    new (p, l, r) = {Production = p; Left = l; Right = r}

and IntermidiateNode = 
    val Slot      : int
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
    new (slot, extension) = {Slot = slot; Extension = extension; First = Unchecked.defaultof<_>; Others = Unchecked.defaultof<_>}
    

type private DotNodeType = Packed | NonTerminal | Intermidiate | Terminal

type ReducedTree = 
    Term of string * TerminalNode
    | NonTerm of string * NonTerminalNode * list<ReducedTree>

let inline packExtension left right : int64<extension> =  LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)

let inline getRule packedValue = int packedValue >>> 16
let inline getPosition (packedValue : int) = int (packedValue &&& 0xffff)

[<Struct>]
type NumNode<'vtype> =
    val Num : int
    val Node : 'vtype
    new (num, node) = {Num = num; Node = node} 

[<AllowNullLiteral>]
type Tree<'TokenType> (tokens : 'TokenType[], root : INode, rules : int[][]) =
    member this.Root = root
    member this.AstToDot (indToString : int -> string) (tokenToNumber : 'TokenType -> int) (tokenData : 'TokenType -> obj) (path : string) =
        use out = new System.IO.StreamWriter (path : string)
        out.WriteLine("digraph AST {")

        let createNode num isAmbiguous nodeType (str : string) =
            let label =
                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
                if not isAmbiguous then cur
                else cur + " !"
            let shape =
                match nodeType with
                | Intermidiate -> ",shape=box"
                | Packed -> ",shape=point"
                | Terminal -> ",shape=box"
                | NonTerminal -> ",shape=oval"
            let color =
                if not isAmbiguous then ""
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
        nodeQueue.Enqueue(new NumNode<INode>(!num, root))
        let isDummy (n:INode) = match n with :? TerminalNode as t -> t.Extension = packExtension -1 -1 | _ -> false
        while nodeQueue.Count <> 0 do
            let currentPair = nodeQueue.Dequeue()
            let key = ref 0
            if !num <> -1
            then

                if currentPair.Node <> null && used.TryGetValue(currentPair.Node, key)
                then
                    createEdge currentPair.Num !key false ""
                else    
                    num := !num + 1
                    used.Add(currentPair.Node, !num)
                    match currentPair.Node with 
                    | :? NonTerminalNode as a -> 
                        if a.Others <> null
                        then
                            createNode !num true NonTerminal (indToString a.Name)
                        else    
                            createNode !num false NonTerminal (indToString a.Name)
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode<INode>(!num, a.First))
                        if a.Others <> null
                        then
                            for n in a.Others do
                                nodeQueue.Enqueue(new NumNode<INode>(!num, n))
                    | :? PackedNode as p ->
                        createNode !num false Packed ""
                        createEdge currentPair.Num !num false ""
                        if not <| isDummy p.Left then nodeQueue.Enqueue(new NumNode<INode>(!num, p.Left))
                        if not <| isDummy p.Right then nodeQueue.Enqueue(new NumNode<INode>(!num, p.Right))
                    | :? IntermidiateNode as i ->
                        createNode !num false Intermidiate ((getRule i.Slot).ToString() + " " + (getPosition i.Slot).ToString())
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode<INode>(!num, i.First))
                        if i.Others <> null
                        then
                            for nodes in i.Others do
                                nodeQueue.Enqueue(new NumNode<INode>(!num, nodes))
                    | :? TerminalNode as t ->
                        if t.Extension <> packExtension -1 -1 
                        then
                            if t.Name <> -1
                            then
                                createNode !num false Terminal ("t " +  (indToString <| tokenToNumber tokens.[t.Name])) 
                                createEdge currentPair.Num !num false ""
                            else
                                createNode !num false Terminal ("epsilon")
                                createEdge currentPair.Num !num false ""
                        else
                            ()
//                            createNode !num false Terminal ("dummy")
//                            createEdge currentPair.Num !num false ""

                    | null -> ()
                    | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()
            else
                let a = currentPair.Node :?> NonTerminalNode
                num := !num + 1
                createNode !num false NonTerminal (indToString a.Name)
                nodeQueue.Enqueue(new NumNode<INode>(!num, a.First))
                if a.Others <> Unchecked.defaultof<_>
                then
                    for n in a.Others do
                        nodeQueue.Enqueue(new NumNode<INode>(!num, n))
        out.WriteLine("}")
        out.Close()

    member this.ReduceTree (tokenToNumber : 'TokenType -> int) (indToString : int -> string) : ReducedTree =
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
                seq{yield ReducedTree.NonTerm(indToString n.Name, n, Seq.toList child )}
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

        let nodeQueue = new Queue<NumNode<_>>()
        let used = new Dictionary<_,_>()
        let num = ref -1
        nodeQueue.Enqueue(new NumNode<_>(!num, root))
        while nodeQueue.Count <> 0 do
            let currentPair = nodeQueue.Dequeue()
            let key = ref 0
            if !num <> -1
            then

                if currentPair.Node <> null && used.TryGetValue(currentPair.Node, key)
                then
                    incr edgesCount
                else    
                    num := !num + 1
                    used.Add(currentPair.Node, !num)
                    match currentPair.Node with 
                    | :? NonTerminalNode as a -> 
                        if a.Others <> Unchecked.defaultof<_>
                        then
                            incr nodesCount
                            incr ambiguityCount
                        else    
                            incr nodesCount
                        
                        incr edgesCount
                        nodeQueue.Enqueue(new NumNode<_>(!num, a.First))
                        if a.Others <> Unchecked.defaultof<_>
                        then
                            for n in a.Others do
                                nodeQueue.Enqueue(new NumNode<_>(!num, n))
                    | :? PackedNode as p ->
                        incr nodesCount
                        incr edgesCount
                        nodeQueue.Enqueue(new NumNode<_>(!num, p.Left))
                        nodeQueue.Enqueue(new NumNode<_>(!num, p.Right))
                    | :? IntermidiateNode as i ->
                        incr nodesCount
                        incr edgesCount
                        nodeQueue.Enqueue(new NumNode<_>(!num, i.First))
                        if i.Others <> Unchecked.defaultof<ResizeArray<PackedNode>>
                        then
                            for nodes in i.Others do
                                nodeQueue.Enqueue(new NumNode<_>(!num, nodes))
                    | :? TerminalNode as t ->
                            incr termsCount
                            incr nodesCount
                            incr edgesCount
                    | null -> ()
                    | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()
            else
                let a = currentPair.Node :?> NonTerminalNode
                num := !num + 1
                incr nodesCount
                nodeQueue.Enqueue(new NumNode<_>(!num, a.First))
                if a.Others <> Unchecked.defaultof<_>
                then
                    for n in a.Others do
                        nodeQueue.Enqueue(new NumNode<_>(!num, n))
        !nodesCount, !edgesCount, !termsCount, !ambiguityCount 