﻿module YC.GLL.SPPF

open System.Collections.Generic
open FSharpx.Collections.Experimental

open Yard.Generators.GLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open Yard.Generators.Common.ASTGLLFSA
open YC.GLL.GSS

type SimpleSyntaxTree() = 
    let nodes = new BlockResizeArray<INode>()
    member this.Nodes = nodes

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
    let nodesNB = new BlockResizeArray<INode>()

    member this.NodesNB = nodesNB
    member this.Nodes = nodes
    member this.TerminalNodes = terminalNodes
    member this.NonTerminalNodes = nonTerminalNodes
    member this.IntermidiateNodes = intermidiateNodes
    member this.EpsilonNodes = epsilonNodes

    member this.GetTerminalNodes = 
        this.Nodes |> Seq.filter (fun x -> x :? TerminalNode) |> Seq.cast<TerminalNode>

    member this.FindSppfNode (t : TypeOfNode) lExt rExt : int<nodeMeasure> =
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

    member this.FindSppfPackedNode parent (state : int<positionInGrammar>) leftExtension rightExtension (left : INode) (right : INode) =
        let createNode () =
            let newNode = new PackedNode(state, left, right)
            this.Nodes.Add(newNode)
            let num = (this.Nodes.Length - 1 )*1<nodeMeasure>
            ///
            if parent = dummyNode then failwith "try to get dummyNode from sppfNodes"
            ///
            match (this.Nodes.Item (int parent)) with
            | :? NonTerminalNode as n ->
                n.AddChild newNode
            | :? IntermidiateNode as i ->
                i.AddChild newNode
            | _ -> failwith "adjf;sawf"
            num
        
        let newNode = createNode()
        newNode

    member this.GetNodeT (symbol : int<token>) (pos : int<positionInInput>) (nextPos : int<positionInInput>) =
        let index = int pos + 1
        if symbol = epsilon
        then
            let contains, v = this.EpsilonNodes.TryGetValue index
            if not contains
            then
                let t = new EpsilonNode(packExtension pos pos)
                let res = this.Nodes.Length *1<nodeMeasure>
                this.Nodes.Add t
                this.EpsilonNodes.Add(index, res)
                TreeNode(res)
            else
                TreeNode(v)
        else
            let hash = packExtension index (int nextPos + 1)
            let contains, v = this.TerminalNodes.TryGetValue hash
            if not contains
            then
                let dict1 = new Dictionary<_,_>()
                let t = new TerminalNode(symbol, packExtension pos nextPos)
                let res = this.Nodes.Length *1<nodeMeasure>
                dict1.Add(symbol, res)
                this.Nodes.Add t
                this.TerminalNodes.Add(hash, dict1)
                TreeNode(res)
            else
                let cont, v1 = v.TryGetValue symbol
                if not cont
                then
                    let t = new TerminalNode(symbol, packExtension pos nextPos)
                    let res = this.Nodes.Length *1<nodeMeasure>
                    this.Nodes.Add t
                    v.Add(symbol, res)
                    TreeNode(res)
                else
                    TreeNode(v1)
    
    member this.GetNodeP (state : int<positionInGrammar>) (t : TypeOfNode) currentN currentR = 
        let currR = this.Nodes.Item (int currentR)
        let extR = currR.getExtension ()
        let lExtR, rExtR = getLeftExtension extR, getRightExtension extR
         
        if currentN <> dummyNode
        then
            let currL = this.Nodes.Item (int currentN)
            let extL = currL.getExtension ()
            let lExtL = getLeftExtension extL//, getRightExtension extL
            let y = this.FindSppfNode t lExtL rExtR
            let extra = this.FindSppfPackedNode y state extL extR currL currR
            if extra = -1<nodeMeasure> then failwith "boom"
            TreeNode(y)
        else
            let y = this.FindSppfNode t lExtR rExtR
            let extra = this.FindSppfPackedNode y state extR extR dummyAST currR
            if extra = -1<nodeMeasure> then failwith "boom"
            TreeNode(y)

    member this.GetNodes posInGrammar stateOfCurrentNonterm (dataCurrentN : ParseData) (dataCurrentR : ParseData) = 
        let currentN = unpackNode dataCurrentN
        let currentR = unpackNode dataCurrentR

        let nontermNode = 
            if posInGrammar |> finalStates.Contains
            then
                this.GetNodeP posInGrammar (Nonterm stateOfCurrentNonterm) currentN currentR
            else
                TreeNode(dummyNode)

        let otherNode =
            let isCurrentRNontermAndItsExtentsEqual = 
                match this.Nodes.Item (int currentR) with
                | :? NonTerminalNode as n ->
                    getRightExtension n.Extension = getLeftExtension n.Extension
                | _ -> false

            if (currentN = dummyNode)&&(not isCurrentRNontermAndItsExtentsEqual)
            then
                dataCurrentR
            else
                this.GetNodeP posInGrammar (Intermed (posInGrammar, stateOfCurrentNonterm)) currentN currentR
        otherNode, nontermNode

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

    member this.StartNTs = 
        this.Nodes |> Seq.filter (fun x -> x :? NonTerminalNode)
                   |> Seq.cast<NonTerminalNode>
                   |> Seq.filter (fun x -> x.Name.Equals startState)

    member this.GetCFRelationsByNTName name (ps : ParserSourceGLL) = 
        let mutable token = -1
        if (ps.NameToId.ContainsKey name)
        then token <- (ps.NameToId.Item name)
        else failwith "no such non terminal"
        this.Nodes
        |> Seq.filter (fun x -> x :? NonTerminalNode)
        |> Seq.cast<NonTerminalNode>
        |> Seq.filter (fun x -> x.Name.Equals token)
        |> Seq.map (fun x -> (ps.IntToString.Item (x.Name |> int), getLeftExtension x.Extension, getRightExtension x.Extension))

    member this.GetNonTermByName name (ps : ParserSourceGLL) = 
        let token = ps.NameToId.Item name
        this.Nodes 
        |> Seq.filter (fun x -> x :? NonTerminalNode) 
        |> Seq.cast<NonTerminalNode> 
        |> Seq.filter (fun x -> x.Name.Equals token)
        
    member this.ClearSPPF = 
        let ancestors = new Dictionary<INode, INode>()
        let findAncestors = 
            for n in this.Nodes do
                match n with
                | :? IntermidiateNode as i -> ancestors.Add(i.First, i)
                                              if (i.Others <> null)
                                              then i.Others.ForEach(fun x -> ancestors.Add(x, i))
                | :? NonTerminalNode as nt -> ancestors.Add(nt.First, nt)
                                              if (nt.Others <> null)
                                              then nt.Others.ForEach(fun x -> ancestors.Add(x, nt))
                | :? PackedNode as p -> ancestors.Add(p.Left, p)
                                        ancestors.Add(p.Right, p)
                | _ -> ()

        let stack = new Stack<INode>()
        let used = new ResizeArray<INode>()
        let rel = new Dictionary<INode, INode>()
        let mutable childs = new ResizeArray<INode>()
        let mutable isStarted = new Dictionary<INode, ResizeArray<INode>>()
        Seq.iter stack.Push this.StartNTs
        while stack.Count <> 0 do
            let head = stack.Pop()
            match head with
            | :? NonTerminalNode as nt -> if not (used.Contains nt)
                                          then used.Add nt
                                               if (nt.Others <> null)
                                               then nt.Others.ForEach(fun x -> stack.Push(x))
                                               stack.Push nt.First
                                               let newNt = new NBNonTerminalNode(nt.Name, nt.Extension)
                                               nodesNB.Add newNt
                                               rel.Add(nt, newNt)
                                               if (ancestors.ContainsKey nt)
                                               then let parentP = ancestors.Item nt
                                                    let mutable chlds = new ResizeArray<INode>()
                                                    if (isStarted.ContainsKey parentP)
                                                    then chlds <- isStarted.Item parentP
                                                         chlds.Add newNt
                                                    else chlds.Add newNt
                                                    isStarted.Add(parentP, chlds)
            | :? PackedNode as p -> if not (used.Contains p)
                                    then used.Add p
                                         let newP = new NBPackedNode(p.State)
                                         nodesNB.Add newP
                                         rel.Add(p, newP)
                                         let ancestor = ancestors.Item p
                                         let related = rel.Item ancestor
                                         let h = nodesNB.Find(fun x -> x.Equals(related)) :?> NBNonTerminalNode
                                         h.AddChild newP
            | :? IntermidiateNode as i -> if not (used.Contains i)
                                          then used.Add i
                                               if (i.Others <> null)
                                               then i.Others.ForEach(fun x -> stack.Push(x))
                                               stack.Push i.First
            | :? TerminalNode as t -> ()
            | _ -> ()                              
                                                                            

    member this.Iterate (s : seq<NonTerminalNode>, ps : ParserSourceGLL, ?maxLength : int, ?searchShortest : bool) = 
        let queue = new Queue<INode>()
        let used = new ResizeArray<INode>()
        let length = ref 0
        let alternatives = new Dictionary<PackedNode, IEnumerable<INode>>()
        let unwrapped = match maxLength with
                        | Some x -> x
                        | None -> -1

        let shrts = match searchShortest with
                    | Some x -> x
                    | None -> false

        Seq.iter queue.Enqueue s
       
        seq {
            while queue.Count > 0 && (unwrapped = -1 || !length < unwrapped) do
                let h = queue.Dequeue()
                match h with
                | :? NonTerminalNode as nt -> if not (used.Contains nt)
                                              then if shrts 
                                                   then used.Add nt
                                                   queue.Enqueue(nt.First)
                                                   if nt.Others <> null
                                                   then nt.Others.ForEach(fun x -> queue.Enqueue(x))

                | :? IntermidiateNode as interm -> if not (used.Contains interm)
                                                   then if shrts 
                                                        then used.Add interm
                                                        queue.Enqueue(interm.First)
                                                        if interm.Others <> null
                                                        then interm.Others.ForEach(fun x -> queue.Enqueue(x))

                | :? PackedNode as packed-> if not (used.Contains packed)
                                            then if shrts 
                                                 then used.Add packed
                                                 queue.Enqueue packed.Left
                                                 queue.Enqueue packed.Right

                | :? TerminalNode as term -> if term.Name <> -1<token>
                                             then incr length
                                                  yield (ps.IntToString.Item (int term.Name)), getLeftExtension term.Extension, getRightExtension term.Extension
                | :? EpsilonNode -> ()
                | x -> failwithf "Strange type of node: %A" x.GetType
        }


let GetTerminals (sppf : SPPF) = 
    sppf.GetTerminalNodes |> Seq.map (fun x -> x.Name, getLeftExtension x.Extension, getRightExtension x.Extension)

(*let processPacked (packed: PackedNode) = 
    let nonBinPacked = new NotBinarizedPackedNode(packed.State, packed.Right.getExtension)
    let matchX (x : INode) = 
        match x with
        | :? IntermidiateNode as interm -> matchX interm.First
                                           if interm.Others <> null 
                                           then interm.Others.ForEach(fun x -> nonBinPacked.AddChild x)
        | :? NonTerminalNode as nt -> nonBinPacked.AddChild nt
        | :? TerminalNode as term -> nonBinPacked.AddChild term

    matchX packed.Left
    matchX packed.Right
    nonBinPacked*)