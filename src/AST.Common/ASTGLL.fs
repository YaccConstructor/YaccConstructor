module Yard.Generators.Common.ASTGLL
open System
open System.Collections.Generic
open Yard.Generators.Common.DataStructures

[<Measure>] type extension


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

let inline packExtension left right : int64<extension> =  LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)

let inline getRule packedValue = int packedValue >>> 16
let inline getPosition (packedValue : int) = int (packedValue &&& 0xffff)

[<Struct>]
type NumNode =
    val Num : int
    val Node : obj
    new (num, node) = {Num = num; Node = node} 

[<AllowNullLiteral>]
type Tree<'TokenType> (tokens : 'TokenType[], root : INode, rules : int[][]) =
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
        
        let nodeQueue = new Queue<NumNode>()
        let used = new Dictionary<_,_>()
        let num = ref -1
        nodeQueue.Enqueue(new NumNode(!num, root))
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
                        if a.Others <> Unchecked.defaultof<_>
                        then
                            createNode !num true NonTerminal (indToString a.Name)
                        else    
                            createNode !num false NonTerminal (indToString a.Name)
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode(!num, a.First))
                        if a.Others <> Unchecked.defaultof<_>
                        then
                            for n in a.Others do
                                nodeQueue.Enqueue(new NumNode(!num, n))
                    | :? PackedNode as p ->
                        createNode !num false Packed ""
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode(!num, p.Left))
                        nodeQueue.Enqueue(new NumNode(!num, p.Right))
                    | :? IntermidiateNode as i ->
                        createNode !num false Intermidiate ((getRule i.Slot).ToString() + " " + (getPosition i.Slot).ToString())
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode(!num, i.First))
                        if i.Others <> Unchecked.defaultof<ResizeArray<PackedNode>>
                        then
                            for nodes in i.Others do
                                nodeQueue.Enqueue(new NumNode(!num, nodes))
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
                            createNode !num false Terminal ("dummy")
                            createEdge currentPair.Num !num false ""
                    | null -> ()
                    | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()
            else
                let a = currentPair.Node :?> NonTerminalNode
                num := !num + 1
                createNode !num false NonTerminal (indToString a.Name)
                nodeQueue.Enqueue(new NumNode(!num, a.First))
                if a.Others <> Unchecked.defaultof<_>
                then
                    for n in a.Others do
                        nodeQueue.Enqueue(new NumNode(!num, n))
        out.WriteLine("}")
        out.Close()

                    seq { yield (ReducedTree.Term((indToString <| tokenToNumber tokens.[t.Name]), t))}
    member this.ExtractFinalPaths =
        let nodeQueue = new Queue<NumNode>()
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

        let nodeQueue = new Queue<NumNode>()
        let used = new Dictionary<_,_>()
        let num = ref -1
        nodeQueue.Enqueue(new NumNode(!num, root))
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
                        nodeQueue.Enqueue(new NumNode(!num, a.First))
                        if a.Others <> Unchecked.defaultof<_>
                        then
                            for n in a.Others do
                                nodeQueue.Enqueue(new NumNode(!num, n))
                    | :? PackedNode as p ->
                        incr nodesCount
                        incr edgesCount
                        nodeQueue.Enqueue(new NumNode(!num, p.Left))
                        nodeQueue.Enqueue(new NumNode(!num, p.Right))
                    | :? IntermidiateNode as i ->
                        incr nodesCount
                        incr edgesCount
                        nodeQueue.Enqueue(new NumNode(!num, i.First))
                        if i.Others <> Unchecked.defaultof<ResizeArray<PackedNode>>
                        then
                            for nodes in i.Others do
                                nodeQueue.Enqueue(new NumNode(!num, nodes))
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
                nodeQueue.Enqueue(new NumNode(!num, a.First))
                if a.Others <> Unchecked.defaultof<_>
                then
                    for n in a.Others do
                        nodeQueue.Enqueue(new NumNode(!num, n))
        !nodesCount, !edgesCount, !termsCount, !ambiguityCount 
