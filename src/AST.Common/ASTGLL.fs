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

type private NotPacked =
    | NonTerm  of NonTerminalNode
    | Term of TerminalNode
    | Inter of IntermidiateNode

type private ResVert =
    | Pack of PackedNode * bool
    | Others of NotPacked * bool * int64<extension>
    | Err of string

type private ResNode =
    | Suc of NonTerminalNode
    | None
    | Error of string

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
type Tree<'TokenType> (toks : array<'TokenType>, root : obj, rules : int[][]) =
    member this.Root = root
    member this.tokens = toks
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
                                createNode !num false Terminal ("t " +  (indToString <| (tokenToNumber this.tokens.[t.Name])) + " " + string(tokenData this.tokens.[t.Name]))
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

    member this.Minimise : unit =

        let getNxtNd (node : NotPacked) : INode =
            match node with
            | NonTerm(trt) -> trt :> INode
            | Term(trt) -> trt :> INode
            | Inter(trt) -> trt :> INode

        let mutable been : list<obj> = []
        let rec f (current : obj) (prevAble : bool) : ResVert =
            match current with
            | :? TerminalNode as cur ->
                been <- List.append been [cur]
                Others(Term(cur), true, cur.Extension)
            | :? PackedNode as cur ->
                if List.contains current been
                then
                    Pack(cur, false)
                else
                    been <- List.append been [cur]
                    let l = f cur.Left true
                    let r = f cur.Right true
                    match l, r with
                    | Others(vrtl, isl, extl), Others(vrtr, isr, extr) ->
                        if extl = packExtension -1 -1 then
                            if prevAble
                            then
                                Others(vrtr, isr, extr)
                            else
                                let trt = getNxtNd vrtr
                                let vrt = new PackedNode(cur.Production, cur.Left, trt)
                                Pack(vrt, true)
                        elif extr = packExtension -1 -1 then
                            if prevAble
                            then
                                Others(vrtl, isl, extl)
                            else
                                let trt = getNxtNd vrtl
                                let vrt = new PackedNode(cur.Production, trt, cur.Right)
                                Pack(vrt, true)
                        else
                            if isl || isr
                            then
                                let vrt = new PackedNode(cur.Production, (if isl then getNxtNd vrtl else cur.Left), (if isr then getNxtNd vrtr else cur.Right) )
                                Pack (vrt, true)
                            else
                                Pack (cur, false)
                    | Pack(vrtl, isl), Others(vrtr, isr, extr) ->
                        if extr = packExtension -1 -1
                        then
                            l
                        else
                            let vrt = new PackedNode(cur.Production, vrtl, (if isr then getNxtNd vrtr else cur.Right) )
                            Pack (vrt, true)
                    | Others(vrtl, isl, extl), Pack(vrtr, isr) ->
                        if extl = packExtension -1 -1
                        then
                            r
                        else
                            let vrt = new PackedNode(cur.Production, (if isl then getNxtNd vrtl else cur.Left), vrtr )
                            Pack (vrt, true)
                    | Pack(vrtl, isl), Pack(vrtr, isr) ->
                        let vrt = new PackedNode(cur.Production, vrtl, vrtr)
                        Pack (vrt, true)
                    | _, Err msg -> Err msg
                    | Err msg, _ -> Err msg
                    | Err msg, Err msg2 -> Err (msg + " and " + msg2)
            | :? NonTerminalNode as cur ->
                if List.contains current been
                then
                    Others(NonTerm(cur), false, cur.Extension)
                else
                    been <- List.append been [cur]
                    let fs = f cur.First false
                    match fs with
                    | Pack(pckd, is) ->
                        if is
                        then
                            cur.First <- pckd
                        let mutable errer = (false, "")
                        if cur.Others <> Unchecked.defaultof<_>
                        then

                            for i in 0..cur.Others.Count - 1 do
                                let rs = f (cur.Others.Item i) false
                                match rs with
                                | Pack(pckd, is) ->
                                    if is
                                    then
                                        cur.Others.RemoveAt i
                                        cur.Others.Insert(i,pckd)
                                | Err msg -> errer <- (true, msg)
                        if fst errer
                        then
                            Err (snd errer)
                        else
                            Others(NonTerm(cur), false, cur.Extension)
                    | Err msg -> Err msg
            | :? IntermidiateNode as cur ->
                if List.contains current been
                then
                    Others(Inter(cur), false, cur.Extension)
                else
                    been <- List.append been [cur]
                    if cur.Others = Unchecked.defaultof<_>
                    then
                        let res = f cur.First true
                        match res with
                        | Others(_, _, _) ->
                            res
                        | Pack(vrt, is) ->
                            if is
                            then res
                            else Pack(vrt, true)
                        | Err msg -> Err msg                      
                    else
                        let fs = f cur.First false
                        match fs with
                        | Pack(pckd, is) ->
                            if is
                            then
                                cur.First <- pckd
                            let mutable errer = (false, "")
                            if cur.Others <> Unchecked.defaultof<_>
                            then

                                for i in 0..cur.Others.Count - 1 do
                                    let rs = f (cur.Others.Item i) false
                                    match rs with
                                    | Pack(pckd, is) ->
                                        if is
                                        then
                                            cur.Others.RemoveAt i
                                            cur.Others.Insert(i,pckd)
                                    | Err msg -> errer <- (true, msg)
                            if fst errer
                            then
                                Err (snd errer)
                            else
                                Others(Inter(cur), false, cur.Extension)
                        |Err msg -> Err msg
            | _ -> Err "Unexpected type of node"
        match f this.Root false with
        | Err msg -> failwith msg

    member this.ExtractMinimalLengthPathTree (ext : int64<extension>) : Tree<'TokenType> =
    
        let getNonTermNode (start : obj) (ext : int64<extension>) : ResNode =
            let mutable been : list<obj> = []
            let rec f (current : obj) =
                if current <> null 
                then
                    match current with
                    | :? TerminalNode as node ->
                        if not (List.contains current been)
                        then
                            been <- List.append been [node]
                        None
                    | :? PackedNode as node ->
                        if List.contains current been
                        then
                            None
                        else
                            been <- List.append been [node]
                            let l = f node.Left
                            if l <> None
                            then
                                l
                            else
                                let r = f node.Right
                                r
                    | :? NonTerminalNode as node ->
                        if List.contains current been
                        then
                            None
                        else
                            been <- List.append been [node]
                            if node.Extension = ext
                            then
                                Suc(node)
                            else
                                let fst = f node.First
                                if fst <> None
                                then
                                    fst
                                else
                                    let mutable is = false
                                    let mutable nd = node
                                    for t in node.Others do
                                        let cu = f t
                                        if cu <> None
                                        then
                                            is <- true
                                            match cu with Suc(x) -> nd <- x
                                    if is
                                    then
                                        Suc(nd)
                                    else
                                        None
                    | :? IntermidiateNode as node ->
                        if List.contains current been
                        then
                            None
                        else
                            been <- List.append been [node]
                            let fst = f node.First
                            if fst <> None
                                then
                                    fst
                                else
                                    let mutable is = false
                                    let mutable nd = new NonTerminalNode(1, ext)
                                    for t in node.Others do
                                        let cu = f t
                                        if cu <> None
                                        then
                                            is <- true
                                            match cu with Suc(x) -> nd <- x
                                    if is
                                    then
                                        Suc(nd)
                                    else
                                        None
                    | _ -> None
                else
                    Error "There is no nodes in tree"
            f root

        match getNonTermNode root ext with
        | Error msg -> failwith msg
        | None -> failwith "Thete is no such nodes in the tree"
        | Suc node -> 
            let newRoot = new NonTerminalNode (node.Name, node.Extension)
            let getNodesOfMinLenTree (node : NonTerminalNode) =
                let mutable been : list<obj> = []
                let rec f (curr : obj) (len : int) (nodes : list<obj>) =
                    match curr with
                        | :? TerminalNode as node ->
                            if List.contains curr been
                            then
                                (len, nodes)
                            else
                                if node.Extension <> packExtension -1 -1
                                then
                                    (len + 1, List.append nodes [node] )
                                else
                                    (len, nodes)
                        | :? PackedNode as node ->
                            if List.contains curr been
                            then
                                (len, nodes)
                            else
                                let lenl, ndsl = f node.Left len nodes
                                let lenr, ndsr = f node.Right len nodes
                                (lenr + lenl, List.append (List.append ndsl ndsr) [node] )
                        | :? NonTerminalNode as node ->
                            if List.contains curr been
                            then
                                (len, nodes)
                            else
                                let ln, nods = f node.First len nodes
                                let mutable min = ln
                                let mutable ndes = nods
                                if node.Others <> null
                                then
                                    for t in node.Others do
                                        let lnn, nds = f t  len nodes
                                        if lnn < min
                                        then
                                            min <- lnn
                                            ndes <- nds
                                (min, List.append ndes [node])
                        | :? IntermidiateNode as node ->
                            if List.contains curr been
                            then
                                (len, nodes)
                            else
                                let ln, nods = f node.First len nodes
                                let mutable min = ln
                                let mutable ndes = nods
                                if node.Others <> null
                                then
                                    for t in node.Others do
                                        let lnn, nds = f t len nodes
                                        if lnn < min
                                        then
                                            min <- lnn
                                            ndes <- nds
                                (min, List.append ndes [node])
                let _, nodes = f node 0 []
                (nodes)
            let nodes = getNodesOfMinLenTree node
            let makeTree (oldNode : obj) =
                let rec f (cur : obj) =
                    match cur with
                    | :? TerminalNode as node->
                        if (List.contains cur nodes)
                        then
                            let vrt = new TerminalNode (node.Name, node.Extension)
                            Option<obj>.Some(vrt)
                        else
                            Option.None
                    | :? PackedNode as node->
                        if (List.contains cur nodes)
                        then
                            match f node.Left with
                            | Option.Some x -> 
                                let l = x
                                match f node.Right with
                                | Option.Some x -> 
                                    let r = x
                                    match l with
                                    | :? INode as lt ->
                                        match r with
                                        | :? INode as rt ->
                                            let vrt = new PackedNode(node.Production, lt, rt)
                                            Option<obj>.Some(vrt)
                                | Option.None -> failwith "packed node doesn't have enoght children"
                            | Option.None -> failwith "packed node doesn't have enoght children"                                

                        else
                            Option.None
                    | :? NonTerminalNode as node ->
                        if (List.contains cur nodes)
                        then
                            let vrt = new NonTerminalNode (node.Name, node.Extension)
                            let fst = f node.First
                            match fst with
                            | Option.Some nxt ->
                                match nxt with
                                | :? PackedNode as pckd ->
                                    vrt.AddChild pckd
                            | Option.None ->
                                for t in node.Others do
                                    match f t with
                                    | Option.Some nxt ->
                                        match nxt with
                                        | :? PackedNode as pckd ->
                                            vrt.AddChild pckd
                            Option<obj>.Some(vrt)
                        else
                            Option.None
                    | :? IntermidiateNode as node ->
                        if (List.contains cur nodes)
                        then
                            let vrt = new IntermidiateNode (node.Slot, node.Extension)
                            let fst = f node.First
                            match fst with
                            | Option.Some nxt ->
                                match nxt with
                                | :? PackedNode as pckd ->
                                    vrt.AddChild pckd
                                if node.Others <> null
                                then
                                    for t in node.Others do
                                        match f t with
                                        | Option.Some nxt ->
                                            match nxt with
                                            | :? PackedNode as pckd ->
                                                vrt.AddChild pckd
                            | Option.None ->
                                for t in node.Others do
                                    match f t with
                                    | Option.Some nxt ->
                                        match nxt with
                                        | :? PackedNode as pckd ->
                                            vrt.AddChild pckd
                            Option<obj>.Some(vrt)
                        else
                            Option.None
                match f oldNode with
                | Option.Some vrt ->
                    match vrt with
                    | :? PackedNode as pckd ->
                        newRoot.AddChild pckd
                | Option.None -> failwith "there is no nodes in tree"
            let newTree = new Tree<'TokenType> (toks, newRoot, rules)
            newTree