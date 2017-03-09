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

type SPPF(lengthOfInput : int, startState : int<positionInGrammar>, finalStates : HashSet<int<positionInGrammar>>) =
    (*
    let hashIntermed lExt rExt state =   
        lExt
        * (lengthOfInput + 1) 
        * (lengthOfInput + 1)
        + rExt
        * (lengthOfInput + 1)
        + state

    let hashNonterm lExt rExt state =
        (lExt * (lengthOfInput + 1) * (lengthOfInput + 1)
         + rExt * (lengthOfInput + 1)
         + state)
         * -1

    let getKeyForPackedNode x y z w =
        x
        * (lengthOfInput + 1)
        * (lengthOfInput + 1)
        * (lengthOfInput + 1)
        + y
        * (lengthOfInput + 1)
        * (lengthOfInput + 1)
        + z * (lengthOfInput + 1)
        + w
    *)
    let dummyNode = -1<nodeMeasure>
    let dummyAST = new TerminalNode(-1<token>, packExtension -1 -1)
    let epsilon = -1<token>
    let unpackNode = function
        | TreeNode x -> x
        | _ -> failwith "Wrong type"

    let nonTerminalNodes = new Dictionary<NonTerminalNode, int<nodeMeasure>>()
    //let packedNodes = new Dictionary<int, int<nodeMeasure>>()
    let intermidiateNodes = new Dictionary<IntermidiateNode, int<nodeMeasure>>()
    let terminalNodes = new Dictionary<TerminalNode, int<nodeMeasure>>()
    let epsilonNodes = new Dictionary<EpsilonNode, int<nodeMeasure>>()//new BlockResizeArray<int<nodeMeasure>>()
    let nodes = new BlockResizeArray<INode>()
    member this.Nodes = nodes
    member this.TerminalNodes = terminalNodes
    member this.NonTerminalNodes = nonTerminalNodes
    member this.IntermidiateNodes = intermidiateNodes
    member this.EpsilonNodes = epsilonNodes

    member this.FindSppfNode (t : TypeOfNode) lExt rExt : int<nodeMeasure> =
        match t with 
        | Nonterm state ->
            let newNode = new NonTerminalNode(state, (packExtension lExt rExt))
            //let key = hashNonterm lExt rExt (int state)
            let contains, n = this.NonTerminalNodes.TryGetValue newNode
            if not contains
            then
                this.Nodes.Add(newNode)
                let num = (this.Nodes.Length - 1)*1<nodeMeasure>
                this.NonTerminalNodes.Add(newNode, num)
                num
            else n
        | Intermed state -> 
            let newNode = new IntermidiateNode(state, (packExtension lExt rExt))
            //let key = hashIntermed lExt rExt (int state)
            let contains, n = this.IntermidiateNodes.TryGetValue newNode
            if not contains
            then
                this.Nodes.Add(newNode)
                let num = (this.Nodes.Length - 1)*1<nodeMeasure>
                this.IntermidiateNodes.Add(newNode, num)
                num  
            else n

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
        //let index = int pos + 1
        if symbol = epsilon
        then
            let t = new EpsilonNode(packExtension pos pos)
            let contains, v = this.EpsilonNodes.TryGetValue t
            if not contains
            then
                let res = this.Nodes.Length *1<nodeMeasure>
                this.Nodes.Add t
                this.EpsilonNodes.Add(t, res)
                TreeNode(res)
            else
                TreeNode(v)
        else
            let t = new TerminalNode(symbol, packExtension pos nextPos)
            let contains, v = this.TerminalNodes.TryGetValue t
            if not contains
            then
                let res = this.Nodes.Length *1<nodeMeasure>
                this.Nodes.Add t
                this.TerminalNodes.Add(t, res)
                TreeNode(res)
            else
                TreeNode(v)
    
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

    member this.GetNodes state nontermState (dataCurrentN : ParseData) (dataCurrentR : ParseData) = 
        let currentN = unpackNode dataCurrentN
        let currentR = unpackNode dataCurrentR

        let x = 
            if state |> finalStates.Contains
            then
                this.GetNodeP state (Nonterm nontermState) currentN currentR
            else
                TreeNode(dummyNode)

        let y =
            let isCurrentRNontermAndItsExtentsEqual = 
                match this.Nodes.Item (int currentR) with
                | :? NonTerminalNode as n ->
                    getRightExtension n.Extension = getLeftExtension n.Extension
                | _ -> false

            if (currentN = dummyNode)&&(not isCurrentRNontermAndItsExtentsEqual)
            then
                dataCurrentR
            else
                this.GetNodeP state (Intermed state) currentN currentR
        y, x

    member this.GetRoots (gss : GSS) startPosition = 
        let gssRoot = 
            gss.Vertices
            |> Seq.filter (fun vert -> vert.Nonterm = startState && vert.PositionInInput = startPosition)
            |> (fun x -> (Array.ofSeq x).[0])
        
        gssRoot.P
        |> Seq.map (fun x -> match x.data with
                             | TreeNode n -> this.Nodes.Item (int n)
                             | _ -> failwith "wrongType")
        |> Array.ofSeq