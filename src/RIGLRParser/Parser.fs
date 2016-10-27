module Yard.Generators.RIGLR.Parser 

open System.Collections.Generic

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode

[<Measure>] type state
[<Measure>] type vertex
[<Measure>] type ptr

[<Struct>]
type Vertex =
    val Label : int<state>
    val Edges : ResizeArray<int<vertex> * int<ptr>>
    new (l, e) = {Label = l; Edges = e}

[<Struct>]
type Context =
    val State   : int<state>    // RCA state    
    val Vertex  : int<vertex>   // RCG vertex (position in Vertex[])
    val Pointer : int<ptr>      // pointer to sppf nodes
    new (s, v, p) = {State = s; Vertex = v; Pointer = p}

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of string

let hashptr (nodes: int list) =
    let sum = ref (int64 nodes.Length + 1L)
    nodes |> List.iter (fun x -> sum := !sum * 17L + (int64 x))
    !sum

let buildAst<'TokenType> (parser : ParserSource<'TokenType>) (tokens : seq<'TokenType>) =
    let tokenEnum = tokens.GetEnumerator()
    let table = parser.Table
    let rules = parser.Rules
    let leftSide = parser.LeftSide
    
    let dummyPtr = 0<ptr>

    let verticies = new ResizeArray<Vertex>( [new Vertex(-1<state>, new ResizeArray<_>())] )  
    let pointers = new ResizeArray<int list>()
    let epsPointers = new Dictionary<int, int<ptr>>()
    let sppfNodes = new ResizeArray<AstNode>()
    let setU = new HashSet<Context>()
    let setP = new Dictionary<int<vertex>, ResizeArray<int<ptr>>>()
    let setN = new HashSet<int>()
    let setW = new Dictionary<int64, ResizeArray<int<ptr>>>()
    let reductions = new Dictionary<int<ptr> * int, int>()
    let nonTermChildren = new Dictionary<int, ResizeArray<int list>>()
    
    setP.Add (0<vertex>, new ResizeArray<_>())
    pointers.Add []

    let nextSetU = new HashSet<Context>([new Context(0<state>, 0<vertex>, dummyPtr)])
    let nextSetW = new Dictionary<int64, ResizeArray<int<ptr>>>()

    let createPointer nodes (set: Dictionary<_, ResizeArray<int<ptr>>>) =
        pointers.Add nodes
        let newPtrNum = (pointers.Count - 1) * 1<ptr>
        let hashKey = hashptr nodes
        set.Add (hashKey, new ResizeArray<_>([newPtrNum])) |> ignore
        newPtrNum
    
    let addNode node =
        sppfNodes.Add node
        let newNodeNum = sppfNodes.Count - 1
        setN.Add newNodeNum |> ignore
        newNodeNum
    
    let addContext context (queue: Queue<_>) =
        if not (setU.Contains context)
        then 
            setU.Add context |> ignore
            queue.Enqueue context

    let findSppfNode (predicate: AstNode -> bool) =
        setN |> Seq.tryFind (fun i -> predicate sppfNodes.[i])
    
    let addFamily (node: AstNode) family =
        match node with
        | :? AST as n ->                     
            if n.first = Unchecked.defaultof<_> 
            then n.first <- family 
            else
                if n.other = null 
                then n.other <- [|family|]
                else n.other <- Array.append n.other [|family|]
        | _ -> failwith "Unexpected AstNode"

    let nodePos (node: AstNode) =
        match node with
        | :? Terminal as t -> t.TokenNumber
        | :? AST as n -> n.pos
        | :? Epsilon -> Seq.length tokens
        | _ -> failwith "Incorrect AstNode"

    let getNodeT termNum pos =
        let checkNode (node: AstNode) =
            match node with
            | :? Terminal as t -> t.TokenNumber = pos 
                                  && (parser.TokenToNumber (Seq.item t.TokenNumber tokens) = termNum)
            | _ -> false
        let termNodeNum = findSppfNode checkNode
        match termNodeNum with
        | Some i -> i
        | None -> addNode (Terminal(pos))
            
    let getNodeN pos prod children =
        let nonTerm = leftSide.[prod]
        let checkNode (node: AstNode) =
            match node with 
            | :? AST as n -> nonTerm = leftSide.[n.first.prod] && pos = n.pos 
            | _ -> false                            
        let nodes = children |> List.map (fun i -> sppfNodes.[i]) |> List.toArray
        let family = new Family(prod, new Nodes(nodes))
        let nonTermNodeNum = findSppfNode checkNode
        match nonTermNodeNum with
        | Some i ->
            if not (nonTermChildren.[i].Contains children)
            then 
                addFamily sppfNodes.[i] family
                nonTermChildren.[i].Add children
            i
        | None -> 
            let newNodeNum = addNode (new AST(family, [||], pos))
            nonTermChildren.Add (newNodeNum, new ResizeArray<_>([children]))
            newNodeNum
                 
    let getPointer nodes (set: Dictionary<_, ResizeArray<int<ptr>>>) =        
        let hashKey = hashptr nodes
        if set.ContainsKey hashKey
        then 
            let ptrsWithSameHash = set.[hashKey]
            let x = ptrsWithSameHash.Find (fun p -> nodes = pointers.[int p])
            if int x = 0 && (not nodes.IsEmpty)
            then
                pointers.Add nodes
                let newPtrNum = (pointers.Count - 1) * 1<ptr>  
                ptrsWithSameHash.Add newPtrNum    
                newPtrNum   
            else x
        else createPointer nodes set 

    let shift state vertex (pointer: int<ptr>) tokenNum pos =
        let termNode = getNodeT tokenNum pos
        let ptr = getPointer (pointers.[int pointer] @ [termNode]) nextSetW
        nextSetU.Add (new Context(state, vertex, ptr)) |> ignore
    
    let reduce state vertex (pointer: int<ptr>) prod queue =
        let rule = rules.[prod] 
        let nodes = pointers.[int pointer]
        let ptr = ref dummyPtr
        if Array.isEmpty rule
        then
            let epsPointer = epsPointers.[prod] 
            if List.isEmpty nodes
            then ptr := epsPointer
            else ptr := getPointer (nodes @ pointers.[int epsPointer]) setW
        else 
            let index = nodes.Length - rule.Length
            let nonTermNode = ref -1
            if reductions.ContainsKey (pointer, prod)
            then nonTermNode := reductions.[pointer, prod]
            else
                let nodesToReduce = nodes.[index ..]
                let minPos = nodesToReduce   // nodes.[.. index] ???
                             |> List.map (fun i -> nodePos sppfNodes.[i])
                             |> List.min
                nonTermNode := getNodeN minPos prod nodesToReduce
                reductions.Add ((pointer, prod), !nonTermNode)
            if index = 0
            then ptr := getPointer [!nonTermNode] setW
            else ptr := getPointer (nodes.[.. index - 1] @ [!nonTermNode]) setW
        addContext (new Context(state, vertex, !ptr)) queue         
    
    let pop vertex (pointer: int<ptr>) queue =
        let currentVertex = verticies.[int vertex]
        let returnState = currentVertex.Label
        if setP.ContainsKey vertex
        then
            if not (setP.[vertex].Contains pointer)
            then setP.[vertex].Add pointer
        for edge in currentVertex.Edges do
            let left, right = pointers.[snd edge |> int], pointers.[int pointer]            
            let ptr = getPointer (left @ right) setW
            addContext (new Context(returnState, fst edge, ptr)) queue
    
    let push state label currentVertex pointer queue =
        let hashKey = hashptr pointers.[int pointer]
        if nextSetW.ContainsKey hashKey
        then nextSetW.[hashKey].Add pointer
        else nextSetW.Add (hashKey, new ResizeArray<_>([pointer]))

        let vertexNum = setP.Keys |> Seq.tryFind (fun i -> verticies.[int i].Label = label)        
        match vertexNum with
        | Some i ->
            let vertex = verticies.[int i]
            let setF = setP.[i]
            if not (vertex.Edges.Contains (currentVertex, pointer))
            then
                vertex.Edges.Add (currentVertex, pointer)
                for p in setF do
                    let left, right = pointers.[int pointer], pointers.[int p]
                    let ptr = getPointer (left @ right) setW
                    addContext (new Context(state, i, ptr)) queue
        | None ->
            verticies.Add (new Vertex(label, new ResizeArray<_>([(currentVertex, pointer)])))
            let newVertexNum = (verticies.Count - 1) * 1<vertex>
            addContext (new Context(state, newVertexNum, dummyPtr)) queue
            setP.Add (newVertexNum, new ResizeArray<_>())                          
    
    let createEpsNodesAndPtr () =
        for i in 0 .. rules.Length - 1 do
            if Array.isEmpty rules.[i]
            then
                let epsNode = new Epsilon(leftSide.[i])
                let family = new Family(i, new Nodes( [|epsNode|] ))
                let nonTermNode = new AST(family, [||], Seq.length tokens)
                let pointerNum = createPointer [addNode nonTermNode] setW
                epsPointers.Add (i, pointerNum)                

    let step token pos (queue: Queue<Context>) (table: (int * int)[][][]) =
        while queue.Count <> 0 do
            let currentContext = queue.Dequeue()
            let currentState = currentContext.State
            let currentVertex = currentContext.Vertex
            let currentPointer = currentContext.Pointer

            let shiftActions = table.[int currentState].[2]
            let shiftInfo = shiftActions |> Array.tryFind (fun (t, s) -> t = token)
            match shiftInfo with
            | Some (term, stateTo) -> shift (stateTo * 1<state>) currentVertex currentPointer token pos
            | None -> ()

            let ptrNodesLength = pointers.[int currentPointer].Length
            let reduceActions = table.[int currentState].[0] 
                                |> Array.filter (fun (prod, s) -> rules.[prod].Length <= ptrNodesLength)
            for r in reduceActions do
                let prod, stateTo = fst r, snd r * 1<state>
                reduce stateTo currentVertex currentPointer prod queue
            
            if parser.PopStates.Contains (int currentState)
            then pop currentVertex currentPointer queue

            let pushActions = table.[int currentState].[1]
            for p in pushActions do
                let label, stateTo = fst p * 1<state>, snd p * 1<state>
                push stateTo label currentVertex currentPointer queue
    
    let refreshSets () =
        setU.Clear()
        setU.UnionWith nextSetU
        setW.Clear()
        for v in nextSetW do setW.Add (v.Key, v.Value)
        nextSetU.Clear()
        nextSetW.Clear()
        setP.Clear()
        reductions.Clear()
        setN.Clear()
        nonTermChildren.Clear()

    let rec run pos =                                                      
        if tokenEnum.MoveNext()
        then
            refreshSets()
            let tokenNum = parser.TokenToNumber tokenEnum.Current            
            step tokenNum pos (new Queue<_>(setU)) table
            run (pos + 1)
        elif nextSetU.Count <> 0
        then 
            refreshSets()
            step -1 pos (new Queue<_>(setU)) table
    
    let getResult () =           
        sppfNodes |> Seq.iter (fun node -> match node with   // lol
                                           | :? AST as ast -> ast.pos <- -1
                                           | _ -> ())
        
        let acceptContext = setU |> Seq.tryFind (fun context -> 
                                                     int context.State = parser.FinalState.[0]
                                                     && context.Vertex = 0<vertex>)
        match acceptContext with
        | Some context ->
            let root = sppfNodes.[pointers.[int context.Pointer].[0]] 
            Success (new Tree<_>(Seq.toArray tokens, root, rules))                      
        | None -> Error "nope"

    createEpsNodesAndPtr()
    run 0
    getResult()