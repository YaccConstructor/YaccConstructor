module YC.GLL.SPPF

open System.Collections.Generic
open FSharpx.Collections.Experimental

open QuickGraph
open Yard.Generators.GLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open Yard.Generators.Common.ASTGLLFSA
open YC.GLL.GSS

type NodeGenerator() =
    let currentNode = ref -1
    member this.getNode() =
            incr currentNode
            !currentNode       

type SPPF(startState : int<positionInGrammar>, finalStates : HashSet<int<positionInGrammar>>) =
    let dummyNode = -1<nodeMeasure>
    let dummyAST = new TerminalNode(-1<token>, packExtension -1 -1)
    let epsilon = -1<token>
    let unpackNode = function
        | TreeNode x -> x
        | _ -> failwith "Wrong type"

    let nonTerminalNodes = new Dictionary<int64<extension>, Dictionary<int<positionInGrammar>,int<nodeMeasure>>>()
    let intermidiateNodes = new Dictionary<int64<extension>, Dictionary<int<positionInGrammar> * int<positionInGrammar>, int<nodeMeasure>>>()
    let terminalNodes = new Dictionary<int64<extension>, Dictionary<int<token>,int<nodeMeasure>>>()
    let epsilonNodes = new Dictionary<int, int<nodeMeasure>>()
    let nodes = new BlockResizeArray<INode>()
    let mostRootedNodes = new HashSet<int<nodeMeasure>>()
    let partialNonterminalNodes = new Dictionary<int64,int<nodeMeasure>>()

    member this.Nodes = nodes
    member this.TerminalNodes = terminalNodes
    member this.NonTerminalNodes = nonTerminalNodes
    member this.IntermidiateNodes = intermidiateNodes
    member this.EpsilonNodes = epsilonNodes
    member this.MostRootedNodes = mostRootedNodes
    member this.PartialNonterminalNodes = partialNonterminalNodes

    member this.GetTerminalNodes() = 
        this.Nodes |> Seq.filter (fun x -> x :? TerminalNode) |> Seq.cast<TerminalNode>

    member this.FindOrCreateIntermedOrNontermNode (t : TypeOfNode) lExt rExt : int<nodeMeasure> =
        match t with 
        | Nonterm state ->
            let hash = packExtension lExt rExt 
            let contains, n = this.NonTerminalNodes.TryGetValue hash
            if not contains
            then
                let dict1 = new Dictionary<_,_>()
                let newNode = new NonTerminalNode(state, (packExtension lExt rExt)) 
                let num = this.Nodes.Length *1<nodeMeasure>
                dict1.Add(state,num)
                this.Nodes.Add(newNode)
                this.NonTerminalNodes.Add(hash, dict1)
                num
            else
                let cont, n1 = n.TryGetValue state
                if not cont
                then
                    let newNode = new NonTerminalNode(state, (packExtension lExt rExt)) 
                    let num = this.Nodes.Length *1<nodeMeasure>
                    this.Nodes.Add(newNode)
                    n.Add(state, num)
                    num
                else
                    n1
        | Intermed (state, nonterm) -> 
            let hash = packExtension lExt rExt
            let contains, n = this.IntermidiateNodes.TryGetValue hash
            if not contains
            then
                let dict1 = new Dictionary<_,_>()
                let newNode = new IntermidiateNode(state, nonterm, (packExtension lExt rExt))
                this.Nodes.Add(newNode)
                let num = (this.Nodes.Length - 1)*1<nodeMeasure>
                dict1.Add((state,nonterm), num)
                this.IntermidiateNodes.Add(hash, dict1)
                num  
            else
                let cont, n1 = n.TryGetValue((state, nonterm))
                if not cont
                then
                    let newNode = new IntermidiateNode(state, nonterm, (packExtension lExt rExt))
                    this.Nodes.Add(newNode)
                    let num = (this.Nodes.Length - 1)*1<nodeMeasure>
                    n.Add((state, nonterm), num)
                    num  
                else
                    n1

    member this.CreateAndAddPackedNodeToNontermOrIntermedNode parent (state : int<positionInGrammar>) leftExtension rightExtension (leftChild : INode) (rightChild : INode) (leftChildIndex : int<nodeMeasure>) (rightChildIndex : int<nodeMeasure>) =
        let createNode () =
            let newNode = new PackedNode(state, leftChild, rightChild)
            this.Nodes.Add(newNode)
            let num = (this.Nodes.Length - 1 )*1<nodeMeasure>
            ///
            if parent = dummyNode then failwith "try to get dummyNode from sppfNodes"
            ///

            if mostRootedNodes.Remove leftChildIndex || mostRootedNodes.Remove rightChildIndex
            then
                mostRootedNodes.Add parent |> ignore

            match (this.Nodes.Item (int parent)) with
            | :? NonTerminalNode as n ->
                n.AddChild newNode
            | :? IntermidiateNode as i ->
                i.AddChild newNode
            | _ -> failwith "adjf;sawf"
            num
        
        let newNode = createNode()
        newNode

    /// Creates new or returns existing terminal node
    member this.FindOrCreateTerminalNode (symbol : int<token>) (pos : int<positionInInput>) (nextPos : int<positionInInput>) =
        let index = int pos + 1
        let result = ref 0<nodeMeasure>
        if symbol = epsilon
        then
            let contains, v = this.EpsilonNodes.TryGetValue index
            if not contains
            then
                let t = new EpsilonNode(packExtension pos pos)
                result := this.Nodes.Length *1<nodeMeasure>
                this.Nodes.Add t
                this.EpsilonNodes.Add(index, !result)
            else
                result := v
        else
            let hash = packExtension index (int nextPos + 1)
            let contains, v = this.TerminalNodes.TryGetValue hash
            if not contains
            then
                let dict1 = new Dictionary<_,_>()
                let t = new TerminalNode(symbol, packExtension pos nextPos)
                result := this.Nodes.Length *1<nodeMeasure>
                dict1.Add(symbol, !result)
                this.Nodes.Add t
                this.TerminalNodes.Add(hash, dict1)
            else
                let cont, v1 = v.TryGetValue symbol
                if not cont
                then
                    let t = new TerminalNode(symbol, packExtension pos nextPos)
                    result := this.Nodes.Length *1<nodeMeasure>
                    this.Nodes.Add t
                    v.Add(symbol, !result)
                else
                    result := v1

        mostRootedNodes.Add !result |> ignore
        TreeNode(!result)
    
    member this.GetParentNode (state : int<positionInGrammar>) (t : TypeOfNode) currentLeft currentRight = 
        let currRight = this.Nodes.Item (int currentRight)
        let extRight = currRight.getExtension ()
        let leftExtOfRight, rightExtOfRight = getLeftExtension extRight, getRightExtension extRight
         
        if currentLeft <> dummyNode
        then
            let currLeft = this.Nodes.Item (int currentLeft)
            let extLeft = currLeft.getExtension ()
            let leftExtOfLeft = getLeftExtension extLeft
            let y = this.FindOrCreateIntermedOrNontermNode t leftExtOfLeft rightExtOfRight
            let extra = this.CreateAndAddPackedNodeToNontermOrIntermedNode y state extLeft extRight currLeft currRight currentLeft currentRight
            if extra = -1<nodeMeasure> then failwith "boom"
            TreeNode(y)
        else
            let y = this.FindOrCreateIntermedOrNontermNode t leftExtOfRight rightExtOfRight
            let extra = this.CreateAndAddPackedNodeToNontermOrIntermedNode y state extRight extRight dummyAST currRight dummyNode currentRight
            if extra = -1<nodeMeasure> then failwith "boom"
            TreeNode(y)

    member this.GetNodes posInGrammar stateOfCurrentNonterm (dataCurrentLeft : ParseData) (dataCurrentRight : ParseData) = 
        let currentLeft = unpackNode dataCurrentLeft
        let currentRight = unpackNode dataCurrentRight

        let nontermNode = 
            if posInGrammar |> finalStates.Contains
            then
                this.GetParentNode posInGrammar (Nonterm stateOfCurrentNonterm) currentLeft currentRight
            else
                TreeNode(dummyNode)

        let otherNode =
            let isCurrentRNontermAndItsExtentsEqual = 
                match this.Nodes.Item (int currentRight) with
                | :? NonTerminalNode as n ->
                    getRightExtension n.Extension = getLeftExtension n.Extension
                | _ -> false

            if (currentLeft = dummyNode)&&(not isCurrentRNontermAndItsExtentsEqual)
            then
                dataCurrentRight
            else
                this.GetParentNode posInGrammar (Intermed (posInGrammar, stateOfCurrentNonterm)) currentLeft currentRight
        otherNode, nontermNode
    
    member this.GetNode posInGrammar stateOfCurrentNonterm (dataCurrentLeft : ParseData) (dataCurrentRight : ParseData) = 
        let currentLeft = unpackNode dataCurrentLeft
        let currentRight = unpackNode dataCurrentRight

        let isCurrentRNontermAndItsExtentsEqual = 
            match this.Nodes.Item (int currentRight) with
            | :? NonTerminalNode as n ->
                getRightExtension n.Extension = getLeftExtension n.Extension
            | _ -> false

        if (currentLeft = dummyNode)&&(not isCurrentRNontermAndItsExtentsEqual)
        then
            dataCurrentRight
        else
            this.GetParentNode posInGrammar (Intermed (posInGrammar, stateOfCurrentNonterm)) currentLeft currentRight


    member this.FindOrCreatePartialNonterminal (nonterm : int<positionInGrammar>) (beginning: int<positionInInput> ): int<nodeMeasure> =
        let hash = pack nonterm beginning 
        let contains, n = this.PartialNonterminalNodes.TryGetValue hash
        if not contains
        then
            let newNode = new PartialNonTerminalNode(nonterm, beginning) 
            let num = this.Nodes.Length *1<nodeMeasure>
            this.Nodes.Add(newNode)
            this.PartialNonterminalNodes.Add(hash, num)
            num
        else
            n

    member this.RemovePartioalNonterminalChild (gssVert : GSSVertex) data = 
        let node = unpackNode data
        let nonterm = this.Nodes.Item (this.FindOrCreatePartialNonterminal gssVert.Nonterm gssVert.PositionInInput |> int)
        (nonterm :?> PartialNonTerminalNode).Children.Remove node |> ignore

    member this.AddChildToPartialNonterm (gssVert : GSSVertex) data = 
        let node = unpackNode data
        let nonterm = this.Nodes.Item (this.FindOrCreatePartialNonterminal gssVert.Nonterm gssVert.PositionInInput |> int)
        (nonterm :?> PartialNonTerminalNode).Children.Add node |> ignore

    member this.GetRoots (gss : GSS) startPosition = 
        let gssRoot = 
            gss.Vertices
            |> Seq.filter (fun vert -> vert.Nonterm = startState && vert.PositionInInput = startPosition)
            |> (fun x -> (Array.ofSeq x).[0])
        
        gssRoot.P.SetP
        |> Seq.map (fun x -> match x.data with
                             | TreeNode n -> this.Nodes.Item (int n)
                             | _ -> failwith "wrongType")
        |> Seq.sortByDescending(fun x -> getRightExtension(x.getExtension()) )
        |> Array.ofSeq
        //|> (fun x -> [|x.[0]|])

    member this.GetNonTermByName name (ps : ParserSourceGLL) = 
        let token = ps.NameToId.Item name
        this.Nodes 
        |> Seq.filter (fun x -> x :? NonTerminalNode) 
        |> Seq.cast<NonTerminalNode> 
        |> Seq.filter (fun x -> x.Name.Equals token)

    member this.Iterate (s : seq<NonTerminalNode>) (ps : ParserSourceGLL) maxLength = 
        let queue = new Queue<INode>()
        let length = ref 0
       
        Seq.iter (fun x -> queue.Enqueue x) s
        seq {
            while queue.Count > 0 && length.Value < maxLength do
                let h = queue.Dequeue()
                match h with
                | :? NonTerminalNode as nt -> queue.Enqueue(nt.First)
                                              if nt.Others <> null
                                              then nt.Others.ForEach(fun x -> queue.Enqueue(x))
                | :? IntermidiateNode as interm -> queue.Enqueue(interm.First)
                                                   if interm.Others <> null
                                                   then interm.Others.ForEach(fun x -> queue.Enqueue(x))
                | :? PackedNode as packed-> queue.Enqueue packed.Left
                                            queue.Enqueue packed.Right
                | :? TerminalNode as term -> if term.Name <> -1<token>
                                             then incr length
                                                  yield (ps.IntToString.Item (int term.Name)), getLeftExtension term.Extension, getRightExtension term.Extension
                | :? EpsilonNode as eps -> ()
                | x -> failwithf "Strange type of node: %A" x.GetType
        }

    member this.GetPrefixTreeEdges (root : int<nodeMeasure>) beginning (nodeGenerator : NodeGenerator) (intToString : Dictionary<_,_>) =
        let vertToMerge = new Dictionary<_,_>()
        //let nodeToEdges = new Dictionary<INode,TaggedEdge<_,_>[]>()
        let node = this.Nodes.[int root]

        let rec buildTree (beginning : int) (destination : int) (node: INode) : TaggedEdge<_,_> [] =
            //if nodeToEdges.ContainsKey(node) then nodeToEdges.[node] else
            //let node = this.Nodes.[int root]
            match node with
            | :? TerminalNode as n ->
                if n.Name = -1<token> && n.Extension = packExtension -1 -1
                then
                    let cond, value = vertToMerge.TryGetValue beginning
                    if cond
                    then
                        vertToMerge.Remove(beginning) |> ignore
                        vertToMerge.Add(destination, value)
                    else
                        vertToMerge.Add(destination, beginning)
                    [||]
                else
                [| new TaggedEdge<_,_>(beginning, destination, (n.Extension |> int64 |> CommonFuns.getRight)-1 ) |]
                    
            | :? IntermidiateNode as n ->
                let length = 
                    if n.Others <> null
                    then
                        n.Others.Count + 1
                    else 1
                let allChildren = Array.init length (fun x -> if x > 0 then n.Others.[x-1] else n.First)
                allChildren
                |> Array.collect (fun x -> buildTree beginning destination x)

            | :? PackedNode as n ->
                let node = nodeGenerator.getNode()
                Array.append (buildTree beginning node n.Left) (buildTree node destination n.Right)

            | :? NonTerminalNode as n ->
                let length = 
                    if n.Others <> null
                    then
                        n.Others.Count + 1
                    else 1
                let allChildren = Array.init length (fun x -> if x > 0 then n.Others.[x-1] else n.First)
                allChildren
                |> Array.collect (fun x -> buildTree beginning destination x)
            | _ -> failwith "unexpected type of node"
        
        let edges = buildTree beginning (nodeGenerator.getNode()) node

        edges
        |> Array.map(fun x ->
            let target =
                let cond, value = vertToMerge.TryGetValue(x.Target)
                if cond
                then
                    value
                else
                    x.Target
            let source = 
                let cond, value = vertToMerge.TryGetValue(x.Source)
                if cond
                then
                    value
                else
                    x.Source
            new TaggedEdge<_,_>(source, target, x.Tag)
            )


let GetTerminals (sppf : SPPF) = 
    sppf.GetTerminalNodes() |> Seq.map (fun x -> x.Name, getLeftExtension x.Extension, getRightExtension x.Extension)

//let getNewNode() = 
//    let edge = new TaggedEdge<int,int>()
//    let vert = new QuickGraph.TaggedEdge<_,_>(startVertex, endVertex, new GSSEdgeLbl(stateToContinue, data))

  


type BandBNode = {
    Parent           : BandBNode option
    Index            : int
    PositionsInTrees : int [] []
}

let mergeSPPFS inputLength (prefixTreesAndBeginnings : (int * AdjacencyGraph<int,TaggedEdge<int,int>>) []) = 
    printfn "Number of trees %A" prefixTreesAndBeginnings.Length
    prefixTreesAndBeginnings
    |> Array.iteri ( fun i (_,x) -> 
        printfn "Number of vertices if tree %i %i" i (Array.ofSeq x.Vertices).Length)
    let front = new Stack<BandBNode>()
    let edges = new ResizeArray<_>()

    let currentNode = ref -1
    let getNode() = 
        incr currentNode
        !currentNode

    //let prefixTrees = prefixTreesAndBeginnings |> Array.map(fun (_,tree) -> tree)
    front.Push({Parent = None; Index = 0; PositionsInTrees = prefixTreesAndBeginnings |> Array.map(fun (beginning,_) -> [|beginning|])})

    let counter = ref 0
    let border = ref 1000
    let old = ref 0
    let found = ref 0
    while (front.Count <> 0) || !found < 1 do
        incr counter
        if !counter - !border = !old
        then
            //printfn "Processed %A" !counter
            old := !counter
            border := !border * 2
        let banbbNode = front.Pop()

        //if banbbNode.Index = inputLength then () else            
        banbbNode.PositionsInTrees
        |> Array.iteri (fun treeNumber possInTree -> 
            if treeNumber = 1
            then
                //printfn "Poss:%A" possInTree
                ()
            let newPoss = 
                possInTree
                |> Array.collect (fun node ->
                    let _,tree = prefixTreesAndBeginnings.[treeNumber]
                    tree.OutEdges node
                    |> Seq.filter (fun x -> x.Tag = banbbNode.Index)
                    |> Array.ofSeq
                    )
            if newPoss.Length = 0 then () else
            if banbbNode.Index + 1 = inputLength
            then 
                incr found
                //printfn "Found: %i" !found
            else
                let newPoss = 
                    Array.init 
                        banbbNode.PositionsInTrees.Length
                        (fun j ->
                            if j = treeNumber
                            then
                                newPoss
                                |> Array.map(fun x -> x.Target)
                            else
                                banbbNode.PositionsInTrees.[j])
                front.Push({Parent = Some banbbNode; Index = banbbNode.Index+1; PositionsInTrees = newPoss})
            )
    printfn "Total processed: %i" !counter
    ()