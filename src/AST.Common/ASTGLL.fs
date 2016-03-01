module Yard.Generators.Common.ASTGLL
open System
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common

[<Measure>] type extension


[<AllowNullLiteral>]
type INode = 
    interface
    abstract member getExtension : unit -> int64<extension>
    abstract member getLength : unit -> int
    end

[<AllowNullLiteral>]
type NonTerminalNode =
    val Extension : int64<extension>
    val Name      : int
    val mutable Length    : int
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
        member this.getLength () = this.Length
    
    member this.SetLength l =
        this.Length <- l

    new (name, extension, len) = {Name = name; Extension = extension; First = Unchecked.defaultof<_>; Others = Unchecked.defaultof<_>; Length = len}
    
and TerminalNode =
    val Name : int
    val Extension : int64<extension>
    val Length : int
    interface INode with
        member this.getExtension () = this.Extension
        member this.getLength () = this.Length
    new (name, extension, len) = {Name = name; Extension = extension; Length = len}

and PackedNode =    
    val Production : int
    val Left : INode
    val Right : INode
    val mutable Length : int 
    interface INode with
        member this.getExtension () = this.Right.getExtension ()
        member this.getLength () = this.Length
    member this.SetLength l =
        this.Length <- l
    new (p, l, r, len) = {Production = p; Left = l; Right = r; Length = len}

and IntermidiateNode = 
    val Slot      : int
    val Extension : int64<extension>
    val mutable First     : PackedNode
    val mutable Others    : ResizeArray<PackedNode>
    val mutable Length    : int
    interface INode with
        member this.getExtension () = this.Extension
        member this.getLength () = this.Length
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
    member this.SetLength l =
        this.Length <- l
    new (slot, extension, len) = {Slot = slot; Extension = extension; First = Unchecked.defaultof<_>; Others = Unchecked.defaultof<_>; Length = len}
    

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
type Tree<'TokenType> (graph : BioParserInputGraph<'TokenType>, root : INode, rules : int[][]) =
    member this.graph = graph
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
                                createNode !num false Terminal ("t " +  (indToString <| (this.graph.Edges.[(t.Name >>> 16)].Tokens.[t.Name &&& 0xffff]))) 
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
                    seq { yield (ReducedTree.Term((indToString <| (this.graph.Edges.[(t.Name >>> 16)].Tokens.[t.Name &&& 0xffff])), t))}
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
      
    member this.ExtractFinalPaths =
        let nodeQueue = new Queue<NumNode<_>>()
        let visitedNodes = new Dictionary<_, Dictionary<_,_>>()
        let createdPaths = new Dictionary<_,_>()

        let cartesianProduct seq1 seq2 = 
            seq1 |> Seq.collect(fun x -> Seq.map (fun y -> seq{yield! x; yield! y}) seq2)

        let rec getSubtree (tree : INode) = 
            match tree with
            | :? NonTerminalNode as nTerm ->
                let c, v = visitedNodes. TryGetValue(nTerm.Name)
                if c then
                    let c2, v2 = v.TryGetValue(nTerm.Extension)
                    if c2 then
                        Seq.empty<_>
                    else
                        v.Add(nTerm.Extension, true)
                        getSubtree nTerm.First
                else
                    let d = new Dictionary<_,_>()
                    d.Add(nTerm.Extension, true)
                    visitedNodes.Add(nTerm.Name, d)
                    getSubtree nTerm.First
                        
            | :? PackedNode as pNode ->
                getSubtree pNode.Left             
            | :? IntermidiateNode as iNode ->
                getSubtree iNode.First
            | :? TerminalNode as term ->
                seq{yield seq{yield term}}
            | x -> failwith "Error"
        ()                     
    
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
