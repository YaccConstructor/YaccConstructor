﻿module YC.Parsing.GLL.SPPF

open System
open System.Collections.Generic
open FSharpx.Collections.Experimental
open Microsoft.FSharp.Collections
open YC.Parsing.Common.GraphInput
open YC.Parsing.Common.GLL
open YC.Parsing.Common.ASTGLLFSA
open YC.Parsing.GLL.GSS

type SPPF(startState : int<positionInGrammar>, finalStates : HashSet<int<positionInGrammar>>) =
    let dummyNode = -1<nodeMeasure>
    let dummyAST = new TerminalNode(-1<token>, packExtension -1 -1, 0<weight>)
    let epsilon = -1<token>
    let unpackNode = function
        | TreeNode x -> x
        | _ -> failwith "Wrong type"

    let nonTerminalNodes = new Dictionary<int64<extension>, Dictionary<int<positionInGrammar>,int<nodeMeasure>>>()
    let intermidiateNodes = new Dictionary<int64<extension>, Dictionary<int<positionInGrammar> * int<positionInGrammar>, int<nodeMeasure>>>()
    let terminalNodes = new Dictionary<int64<extension>, Dictionary<int<token>,int<nodeMeasure>>>()
    let epsilonNodes = new Dictionary<int, int<nodeMeasure>>()
    let nodes = new BlockResizeArray<INode>()

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
            if parent = dummyNode then failwith "try to get dummyNode from sppfNodes"
            let newWeight = left.getWeight() + right.getWeight()
            match (this.Nodes.Item (int parent)) with
            | :? NonTerminalNode as n ->                
                if n.MapChildren (fun n -> (n.State = state && n.Left = left && n.Right = right) || n.Weight < newWeight)
                   |> Seq.exists id
                   |> not
                then
                    let r = new PackedNode(state, left, right)
                    this.Nodes.Add(r)
                    //r
                    n.AddChild r
            | :? IntermidiateNode as i ->
                if i.MapChildren (fun n -> (n.State = state && n.Left = left && n.Right = right) || n.Weight < newWeight)
                   |> Seq.exists id
                   |> not
                then
                    let r = new PackedNode(state, left, right)
                    this.Nodes.Add(r)
                    //r
                    i.AddChild r 
            | x -> failwithf "Unexpected tye of node: %A" x
                    
            (this.Nodes.Length - 1 )*1<nodeMeasure>
        
        let newNode = createNode()
        newNode

    member this.GetNodeT (symbol : int<token>) (pos : int<positionInInput>) (nextPos : int<positionInInput>) weight =
        let index = int pos + 1
        if symbol = epsilon //|| symbol = -10<token>
        then
            let contains, v = this.EpsilonNodes.TryGetValue index
            if not contains
            then
                let t = new EpsilonNode(packExtension pos pos)
                let res = this.Nodes.Length * 1<nodeMeasure>
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
                let t = new TerminalNode(symbol, packExtension pos nextPos, weight)
                let res = this.Nodes.Length * 1<nodeMeasure>
                dict1.Add(symbol, res)
                this.Nodes.Add t
                this.TerminalNodes.Add(hash, dict1)
                TreeNode(res)
            else
                let cont, v1 = v.TryGetValue symbol
                if not cont
                then
                    let t = new TerminalNode(symbol, packExtension pos nextPos, weight)
                    let res = this.Nodes.Length * 1<nodeMeasure>
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
            printfn "state = %A" state
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
        let gssRoots = 
            let s =
                gss.Vertices
                |> Seq.filter (fun vert -> vert.Nonterm = startState)
            s
            |> Seq.sortBy (fun vert -> vert.PositionInInput)
            //|> (fun x -> (Array.ofSeq x).[0])
        
        gssRoots
        |> Seq.collect (fun gssRoot ->
        gssRoot.P.SetP
        |> Seq.map (fun x -> match x.data with
                             | TreeNode n -> this.Nodes.Item (int n)
                             | _ -> failwith "wrongType")
        //|> Seq.sortByDescending(fun x -> getRightExtension(x.getExtension()) )
        //|> Array.ofSeq)
        )
        |> Array.ofSeq
        |> Array.sortByDescending(fun x -> getRightExtension(x.getExtension()) - (getLeftExtension(x.getExtension())))
        |> (fun x -> [|x.[0]|])
    
    member this.GetRootsForStartAndFinal (gss : GSS) (startPositions :_ []) (finalPositions :_ []) = 
        let startPoss = new HashSet<int<positionInInput>>(startPositions)
        let finalPoss = new HashSet<int<positionInInput>>(finalPositions)
        let gssRoots = 
            gss.Vertices
            |> Seq.filter (fun vert -> vert.Nonterm = startState && startPoss.Contains(vert.PositionInInput) )
            |> Array.ofSeq
        
        gssRoots
        |> Array.collect (fun x ->
            x.P.SetP
            |> ResizeArray.toArray
            |> Array.choose (fun x -> 
                match x.data with
                | TreeNode n -> 
                    let node = this.Nodes.Item (int n)
                    if (finalPoss.Contains(node.getExtension() |> getRightExtension))
                    then
                        Some(node)
                    else
                        None
                | _ -> failwith "wrongType")
            )

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


    member this.SetWeights (roots : array<INode>)  = 
        
        let visited = new HashSet<_>()        
        
        let rec _go (node:INode) =
            if visited.Contains node
            then 0<weight>
            else 
                visited.Add node
                match node with
                | :? NonTerminalNode as nt -> let l = _go (nt.First)
                                              let w =
                                                  if nt.Others <> null
                                                  then nt.Others |> ResizeArray.fold (fun w n -> min w (_go n)) 0<weight>
                                                  else Int32.MaxValue * 1<weight>
                                              min l w
                                              let w = min l w
                                              nt.Weight <- w
                                              w
                                              
                | :? IntermidiateNode as interm ->
                                              let l = _go (interm.First)
                                              let w =
                                                  if interm.Others <> null
                                                  then interm.Others |> ResizeArray.fold (fun w n -> min w (_go n)) 0<weight>
                                                  else Int32.MaxValue * 1<weight>
                                              let w = min l w
                                              interm.Weight <- w
                                              w
                | :? PackedNode as packed->
                    let w = _go packed.Left + _go packed.Right
                    packed.Weight <- w
                    w
                | :? TerminalNode as term -> term.Weight                                         
                | :? EpsilonNode as eps -> eps.Weight
                | x -> failwithf "Strange type of node: %A" x.GetType
                
        Array.map _go roots
        |> ignore


    member this.ChooseMinimalForest (roots : array<INode>)  = 
        
        let visited = new HashSet<_>()        
        
        let rec _go (node:INode) =
            //if visited.Contains node
            //then 0<weight>
            //else 
            //    visited.Add node
                match node with
                | :? NonTerminalNode as nt ->
                                          if nt.Others <> null
                                          then                                          
                                                  
                                                  let w = nt.Others |> ResizeArray.toArray |> Array.minBy (fun n -> n.Weight) 
                                                  if nt.First.Weight > w.Weight then nt.First <- w
                                          nt.Others <- null
                                          _go (nt.First)
                                                  
                | :? IntermidiateNode as interm ->
                                              if interm.Others <> null
                                              then                                                                                            
                                                  let w = interm.Others |> ResizeArray.toArray |> Array.minBy (fun n -> n.Weight) 
                                                  if interm.First.Weight > w.Weight then interm.First <- w
                                              interm.Others <- null
                                              _go (interm.First)

                | :? PackedNode as packed->
                    _go packed.Left
                    _go packed.Right
                    
                | _ -> ()                                         
                
        Array.iter _go roots
        


let GetTerminals (sppf : SPPF) = 
    sppf.GetTerminalNodes |> Seq.map (fun x -> x.Name, getLeftExtension x.Extension, getRightExtension x.Extension)