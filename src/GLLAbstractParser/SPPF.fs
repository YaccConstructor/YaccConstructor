module YC.GLL.SPPF

open System.Collections.Generic
open FSharpx.Collections.Experimental

open Yard.Generators.GLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open Yard.Generators.Common.ASTGLLFSA
open YC.GLL.GSS

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

    member this.GetNonTermByName name (ps : ParserSourceGLL) = 
        let rev (map : Map<int, string>) = 
            Map.fold(fun (m : Map<string, int>) k v -> m.Add(v, k)) Map.empty map
        let token = (rev (ps.IntToString |> Seq.map (|KeyValue|)|> Map.ofSeq)).Item name
        this.Nodes 
        |> Seq.filter (fun x -> x :? NonTerminalNode) 
        |> Seq.cast<NonTerminalNode> 
        |> Seq.filter (fun x -> x.Name.Equals token)

    member this.Iterate (s : seq<NonTerminalNode>) (ps : ParserSourceGLL) maxLength = 
        let queue = new Queue<INode>()
        let length = ref 0
        (*let dang = new HashSet<PackedNode>()
        let checkForStub (x : INode) = 
            match x with
            | :? TerminalNode as term -> term.Name.Equals -1<token>
            | :? EpsilonNode -> true
            | _ -> false

        for n in this.Nodes do
            match n with
            | :? PackedNode as packed -> if checkForStub packed.Left || checkForStub packed.Right
                                         then dang.Add packed |> ignore
            | _ -> ()*)

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


let GetTerminals (sppf : SPPF) = 
    sppf.GetTerminalNodes |> Seq.map (fun x -> x.Name, getLeftExtension x.Extension, getRightExtension x.Extension)