module Yard.Generators.RNGLR.OtherSPPF

open System.Collections.Generic
open System.IO

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open Yard.Generators.Common.DataStructures
open FSharpx.Collections.Experimental


type BracketSearchInfo<'T> = 
    ///Number of left paired token
    val LeftBraceNumber : int
    ///Number of right paired token
    val RightBraceNumber : int
    val CurrentPosition : 'T
    //True, if we search close token
    val ToRight : bool

    new (left, right, position, toRight) = 
        {
            LeftBraceNumber = left;
            RightBraceNumber = right;
            CurrentPosition = position;
            ToRight = toRight;
        }

    member this.GetCurrentNumber() = 
        if this.ToRight 
        then this.LeftBraceNumber
        else this.RightBraceNumber

    member this.GetSearchableNumber() = 
        if this.ToRight 
        then this.RightBraceNumber
        else this.LeftBraceNumber

[<AllowNullLiteral>]
type OtherAST =
    val mutable First : OtherFamily
    val mutable Other : OtherFamily[]
    val mutable Pos : int
    val mutable Parent : UsualOne<obj>
    new (f, o) = {Pos = -1; First = f; Other = o; Parent = Unchecked.defaultof<UsualOne<obj>>}
    new (f) = new OtherAST(f, null)
    
    member inline this.FindFamily f =
        if f this.First then Some this.First
        elif this.Other <> null then Array.tryFind f this.Other
        else None

    member this.AddParent (p : obj) = 
        let newParent = 
            if this.Parent.first = Unchecked.defaultof<obj> 
            then new UsualOne<_>(p, [||])
            else new UsualOne<_>(this.Parent.first, Seq.append this.Parent.other [|p|] |> Array.ofSeq)
        this.Parent <- newParent

    member this.DoForAll f = 
        f this.First
        if this.Other <> null
        then
            this.Other 
            |> Array.iter f

and OtherFamily =
    val mutable Parent : obj
    val Prod : int
    val mutable Nodes : OtherNodes
    new (pro, n) = {Prod = pro; Nodes = n; Parent = Unchecked.defaultof<UsualOne<obj>>}

    member this.AddParent (p : obj) = this.Parent <- p

    member this.ReplaceNodes (nodes) = this.Nodes <- nodes

and OtherNodes =
    val mutable Parent : UsualOne<obj>
    val mutable Fst : obj
    val mutable Snd : obj
    val mutable Other : obj[]

    new (f, s, o) = {Fst = f; Snd = s; Other = o; Parent = Unchecked.defaultof<UsualOne<obj>>}
    new (f, s) = new OtherNodes(f, s, null)
    new (f) = new OtherNodes(f, null, null)

    new (arr : array<_>) =
        let mutable fs = null
        let mutable sn = null
        let mutable other = null
        if arr <> null then
            if arr.Length > 0 
            then
                fs <- arr.[0]
                if arr.Length > 1 
                then
                    sn <- arr.[1]
                    if arr.Length > 2 
                    then
                        other <- arr.[2..]
        {Fst = fs; Snd = sn; Other = other; Parent = Unchecked.defaultof<UsualOne<obj>>}
            
            
        member nodes.DoForAll f =
            if nodes.Fst <> null 
            then
                f nodes.Fst
                if nodes.Snd <> null 
                then
                    f nodes.Snd
                    if nodes.Other <> null 
                    then
                        nodes.Other 
                        |> Array.iter f

        member this.AddParent (p : obj) = 
            this.DoForAll 
                (
                    fun node -> 
                        match node with 
                        | :? OtherAST as ast -> ast.AddParent p
                        | _ -> ()
                )
                
            let newParent = 
                if this.Parent.first = Unchecked.defaultof<obj> 
                then new UsualOne<_>(p, [||])
                else new UsualOne<_>(this.Parent.first, Seq.append this.Parent.other [|p|] |> Array.ofSeq)
                
            this.Parent <- newParent

            
        member nodes.Exist f = 
            if nodes.Fst <> null 
            then 
                if f nodes.Fst 
                then true
                else 
                    if nodes.Snd <> null 
                    then 
                        if f nodes.Snd 
                        then true
                        else
                            if nodes.Other <> null 
                            then nodes.Other |> Array.exists f
                            else false
                    else false
            else false
            
        /// <summary>
        /// Applies function to nodes which are located to the right than node nd. 
        /// Right sibling of nd is first.
        /// </summary>
        member nodes.DoForAllAfterNode nd f =
            let mutable needDo = false
            if nodes.Fst <> null 
            then
                needDo <- nodes.Fst = nd
                if nodes.Snd <> null 
                then 
                    if needDo then f nodes.Snd
                    needDo <- needDo || nodes.Snd = nd
                    if nodes.Other <> null 
                    then
                        for i = 0 to nodes.Other.Length - 1 do
                            if needDo 
                            then f nodes.Other.[i]
                            needDo <- needDo || nodes.Other.[i] = nd

        /// <summary>
        /// Applies function f to nodes that are located to the left than node nd. 
        /// Left sibling of nd is first.
        /// </summary>
        member nodes.DoForAllBeforeNode nd f = 
            if nodes.Fst <> null && nodes.Fst <> nd
            then 
                if nodes.Snd <> null && nodes.Snd <> nd
                then 
                    if nodes.Other <> null 
                    then 
                        let bound = 
                            let indexOpt = nodes.Other |> Array.tryFindIndex ((=) nd) 
                            
                            match indexOpt with
                            | Some value -> value - 1
                            | None -> nodes.Other.Length - 1
                        for i = bound downto 0 do
                            f nodes.Other.[i]
                    f nodes.Snd
                f nodes.Fst
            
        /// <summary>
        /// Applies function to all nodes (right node is first)
        /// </summary>
        member nodes.DoForAllRev f =
            if nodes.Fst <> null 
            then
                if nodes.Snd <> null 
                then
                    if nodes.Other <> null 
                    then
                        for i = nodes.Other.Length - 1 downto 0 do
                            f nodes.Other.[i]
                    f nodes.Snd
                f nodes.Fst

                    
type private DotNodeType = Prod | AstNode

type private Context = 
    struct
        val Parent : OtherFamily
        val Child : obj
        val Count : int

        new (p, ch, co) = {Parent = p; Child = ch; Count = co}
    end

[<AllowNullLiteral>]
type OtherTree<'TokenType> (tree : Tree<'TokenType>) = 
    let tokens = tree.Tokens
    
    let astToOtherAst processFamily (cache : Dictionary<_, _>) (ast : AST)  = 
        if cache.ContainsKey ast 
        then 
            cache.[ast]
        else
            let fstChild = processFamily ast.first
            let otherChildren = 
                if ast.other = null
                then null
                else ast.other |> Array.map processFamily

            let newAST = new OtherAST (fstChild, otherChildren)
            cache.Add (ast, newAST)

            fstChild.AddParent newAST
            if otherChildren <> null
            then otherChildren |> Array.iter (fun child -> child.AddParent newAST) 
            newAST
            
    let processNode processAST (dict : Dictionary<_, _>) (node : obj) (newNodes : list<_> ref)= 
        let newElem = 
            match node with 
            | :? AST as ast -> 
                if dict.ContainsKey ast
                then box <| dict.[ast]
                else box <| processAST ast
            | :? Terminal as term -> box term
            | :? Epsilon as eps -> box eps
            | _ -> failwithf "Unexpected node type in OtherSppf: %s" <| node.GetType().ToString()
                
        newNodes := newElem :: !newNodes

    let addReverseEdge (otherFamily : OtherFamily) (oldNodes : Nodes) (nodesDict : Dictionary<_, _>) (dict : Dictionary<_, _>)= 
        
        let newNodes = 
            if nodesDict.ContainsKey oldNodes 
            then 
                nodesDict.[oldNodes]
            else
                let children = ref []
                let handle (node : obj) = 
                    let newElem = 
                        match node with 
                        | :? AST as ast -> 
                            if dict.ContainsKey ast
                            then box dict.[ast]
                            else failwith "Unexpected AST"
                        | :? Terminal as terminal -> box terminal.TokenNumber
                        | _ -> failwithf "Unexpected node type in OtherSppf: %s" <| node.GetType().ToString()
                
                    children := newElem :: !children
                
                oldNodes.doForAll handle
                new OtherNodes(!children |> List.rev |> Array.ofList)
                
        otherFamily.ReplaceNodes newNodes
        newNodes.AddParent otherFamily
    
    
    let root = 
        let treeRoot = 
            match tree.Root with
            | :? AST as ast -> ast
            | :? Epsilon -> Unchecked.defaultof<_>
            | _ -> failwith "Strange tree - singleNode with non-negative value"

        let dict = new Dictionary<AST, OtherAST>()
        
        // to avoid problems with cycles
        let knownNodes = new ResizeArray<_>()
        
        let nodesDict = new Dictionary<Nodes, OtherNodes>()
        // reverse edges that will be added after all
        let postActions = new Dictionary<OtherFamily, Nodes>() 

        let rec processFamily (family : Family) = 
            
            let processAST' = astToOtherAst processFamily dict
            let processNode' = processNode processAST' dict

            if family.nodes.isForAll knownNodes.Contains
            then
                let fakeNodes = new OtherNodes(new obj())
                let newFamily = new OtherFamily (family.prod, fakeNodes)
                postActions.Add (newFamily, family.nodes)
                newFamily
            else
                let newNodes = ref []
                family.nodes.doForAll 
                    (
                        fun node -> 
                            knownNodes.Add node
                            processNode' node newNodes
                    )
                let newNodes = new OtherNodes(!newNodes |> List.rev |> Array.ofList)
                nodesDict.Add (family.nodes, newNodes)
                let newFamily = new OtherFamily (family.prod, newNodes)
                newNodes.AddParent newFamily
                newFamily

        let family = processFamily treeRoot.first
        
        postActions
        |> Seq.iter (fun item -> addReverseEdge item.Key item.Value nodesDict dict)

        let rootAST = new OtherAST(family)
        family.AddParent rootAST
        rootAST

    let order =
        let stack = new Stack<_>()
        stack.Push root
        let res = new BlockResizeArray<_>()
        //if not isEpsilon then
        while stack.Count > 0 do
            let u = stack.Pop()
            let children = u
            if children.Pos = -2 
            then
                children.Pos <- res.Length
                res.Add u
            elif children.Pos = -1 
            then
                children.Pos <- -2
                stack.Push u
                let inline handle (family : OtherFamily) = 
                    let inline handleAst (ast : obj) =
                        match ast with
                        | :? OtherAST as ast ->
                            if ast.Pos = -1 
                            then stack.Push ast
                        | _ -> ()
                    family.Nodes.DoForAllRev handleAst
                handle children.First
                if children.Other <> null 
                then children.Other |> Array.iter handle
        res.ToArray()

    let familyToTokens = 
        let dict = new Dictionary<OtherFamily, int list>()
        // to avoid problems with cycles
        let processedAst = new ResizeArray<_>()

        let rec calcTokens (family : OtherFamily) = 
            if dict.ContainsKey family 
            then 
                dict.[family]
            else
                let tokens = ref []

                let processNode (node : obj) = 
                    match node with
                    | :? OtherAST as ast -> 
                        if not <| processedAst.Contains ast
                        then
                            processedAst.Add ast

                            let temp = 
                                if ast.Other = null 
                                then 
                                    calcTokens ast.First 
                                else
                                    ast.Other
                                    |> Array.map calcTokens
                                    |> Array.append [| calcTokens ast.First |]
                                    |> List.concat
                            tokens := !tokens @ temp
                            processedAst.Remove ast |> ignore
                    | :? Terminal as t ->
                        tokens := t.TokenNumber :: !tokens
                    | _ -> ()

                family.Nodes.DoForAll processNode
                
                dict.Add (family, !tokens)
                !tokens

        calcTokens root.First |> ignore
        dict

    let findNodeWithParents (now : 'range) (tokenToPos : 'TokenType -> seq<'range>)= 
        let parents = ref []
        let child = ref null

        let containsRange number = 
            tokenToPos tokens.[number]
            |> Seq.exists ((=) now)

        let rec handleFamily (family : OtherFamily) = 
           
            let processNode (node : obj) = 
                match node with
                | :? OtherAST as ast -> 
                    let processFam fam = 
                        let toks = familyToTokens.[fam]
                        if toks |> List.exists containsRange
                        then handleFamily fam

                    processFam ast.First
                    if ast.Other <> null 
                    then ast.Other |> Array.iter processFam
                | :? Terminal as t -> 
                    let isNewParent = !parents |> List.forall ((<>) family) 
                    if containsRange t.TokenNumber && isNewParent
                    then
                        parents := family :: !parents
                        child := node
                | :? Epsilon -> ()
                | _ -> failwithf "Unexpected node type in OtherSppf: %s" <| child.GetType().ToString()

            family.Nodes.DoForAll processNode

        handleFamily root.First
        !child, !parents

    /// <summary>
    /// Returns all paired tokens for token that is located in range. 
    /// For example it returns all paired right_brackets for left_bracket 
    /// </summary>
    member this.FindAllPair (info : BracketSearchInfo<'range>) tokenToNumber (tokenToPos : 'TokenType -> seq<'range>) = 
        let leaf, parents = findNodeWithParents info.CurrentPosition tokenToPos
        let res = new ResizeArray<_>()
        let contexts = new Stack<_>()

        parents
        |> List.iter (fun family -> contexts.Push <| new Context (family, leaf, 1))

        let nowNumber, pairNumber = info.GetCurrentNumber(), info.GetSearchableNumber()
        let handleSomeNodes node (family : OtherFamily) f = 
            if info.ToRight
            then family.Nodes.DoForAllAfterNode node f
            else family.Nodes.DoForAllBeforeNode node f

        let handleAllNodes (family : OtherFamily) f = 
            if info.ToRight
            then family.Nodes.DoForAll f
            else family.Nodes.DoForAllRev f

        let isBrace tokenIndex = 
            let tokNumber = tokenToNumber tokens.[tokenIndex]
            tokNumber = pairNumber || tokNumber = nowNumber

        while contexts.Count > 0 do
            let state = contexts.Pop()
            let parent = state.Parent
            let child = ref state.Child
            let count = ref state.Count

            let rec processFamily (family : OtherFamily) = 
                //familyToTokens.ContainsKey family is always true
                if familyToTokens.[family] |> List.exists isBrace
                then 
                    let handle (node : obj) =
                        match node with 
                        | :? Terminal as t when isBrace t.TokenNumber -> 
                            let token = tokens.[t.TokenNumber]
                            
                            let tokNumber = tokenToNumber token
                            if nowNumber = tokNumber then incr count
                            elif pairNumber = tokNumber then decr count
                            
                            if !count = 0 then res.Add token
                        | :? OtherAST as ast -> processAST ast
                        | :? Epsilon 
                        | :? Terminal -> ()
                        | x -> failwithf "Unexpected node type in OtherSppf: %A" x
        
                    if family.Nodes.Exist ((=) !child)
                    then handleSomeNodes !child family handle 
                    else handleAllNodes family handle

            and processAST (ast : OtherAST) = 
                let value = !count
                let oldChild = !child
                
                let restoreContextAndProcess family = 
                    count := value
                    child := oldChild
                    processFamily family

                ast.DoForAll restoreContextAndProcess

            processFamily parent
            if !count <> 0 then
                match parent.Parent with
                | :? OtherAST as ast -> 
                    if ast.Parent <> Unchecked.defaultof<UsualOne<_>>
                    then 
                        ast.Parent.DoForAll(fun family -> contexts.Push <| new Context(family :?> OtherFamily, ast, !count))
                    
                | x -> failwithf "Unexpected node: %s" <| x.GetType().ToString()
        res

    /// <summary>
    /// Prints ast in console
    /// </summary>
    member this.PrintAst() = 
        let processed = new ResizeArray<_>()
        let rec printAst ind ast =
            let printInd num (x : 'a) =
                printf "%s" (String.replicate (num * 4) " ")
                printfn x

            match (ast : obj) with
            | :? Epsilon -> printInd ind "e"
            | :? Terminal as t -> printInd ind "t: %A" tokens.[t.TokenNumber]
            | :? OtherAST as fam ->
                
                processed.Add(ast)
                let children = fam
                let needGroup = children.Other <> null
                if needGroup 
                then printInd ind "^^^^"
                
                let inline handle separate (family : OtherFamily) =
                    if separate 
                    then printInd ind "----"
                    printInd ind "prod %d" family.Prod
                    family.Nodes.DoForAll (printAst <| ind + 1)
                
                if not <| processed.Contains ast
                then
                    children.First |> handle false
                
                    if needGroup 
                    then children.Other |> Array.iter (handle true)
                       
                    if needGroup then printInd ind "vvvv"
            | x -> failwithf "Unexpected node type in OtherSppf: %s" <| x.GetType().ToString()
        printAst 0 root

    /// <summary>
    /// Prints sppf in .dot file
    /// </summary>
    member this.ToDot (indToString : int -> string) tokenToNumber (leftSide : array<int>) (path : string) =
        let next =
            let cur = ref order.Length
            fun () ->
                incr cur
                !cur

        use out = new StreamWriter (path : string)
        out.WriteLine("digraph AST {")
        
        let createNode num isAmbiguous isTerminal nodeType (str : string) =
            let label =
                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
                if not isAmbiguous then cur else cur + " !"
            let shape =
                match nodeType with
                | AstNode -> ",shape=box"
                | Prod -> ""
            let color =
                if isTerminal then ",style=\"filled\",fillcolor=gray"
                elif not isAmbiguous then ""
                else ",style=\"filled\",fillcolor=red"
            out.WriteLine (sprintf "    %d [label=\"%s\" %s %s]" num label color shape)
        
        let createEdge (b : int) (e : int) isBold (str : string) =
            let label = str.Replace("\n", "\\n").Replace ("\r", "")
            let bold = 
                if not isBold then "" else "style=bold,width=10,"
            sprintf "    %d  -> %d [ %s label=\" %s \" ]" b e bold label
            |> out.WriteLine
        
        let createEpsilon ind = 
            let res = next()
            createNode res false false AstNode ("n " + indToString (-1-ind))
            let u = next()
            createNode u false false AstNode "eps"
            createEdge res u true ""
            res

        let createTerm t =
            let res = next()
            createNode res false true AstNode ("t " + indToString (tokenToNumber tokens.[t]))
            res
        
        for i = order.Length-1 downto 0 do
            let x = order.[i]
            if x.Pos <> -1 
            then
                let children = x
                    
                let label = indToString leftSide.[children.First.Prod]
                     
                createNode i (children.Other <> null) false AstNode ("n " + label)
                     
                let inline handle (family : OtherFamily) =
                    let u = next()
                    createNode u false false Prod (sprintf "prod %d" family.Prod)
                    createEdge i u true ""
                    family.Nodes.DoForAll <| fun child ->
                        let v = 
                            match child with
                            | :? OtherAST as v -> v.Pos
                            | :? Epsilon as eps -> createEpsilon eps.EpsilonNonTerm
                            | :? Terminal as t -> createTerm t.TokenNumber
                            | _ -> failwithf "Unexpected node type in OtherSppf: %s" <| child.GetType().ToString()
                        createEdge u v false ""
                
                children.DoForAll handle
                
        out.WriteLine("}")
        out.Close()