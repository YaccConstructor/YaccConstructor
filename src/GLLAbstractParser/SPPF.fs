module YC.GLL.SPPF

open System.Collections.Generic
open FSharpx.Collections.Experimental

open Yard.Generators.GLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open Yard.Generators.Common.ASTGLLFSA
//open YC.GLL.GSS

type SPPF(lengthOfInput : int, finalStates : HashSet<int<positionInGrammar>>) =
    let hashIntermed x y z =
        x
        * (lengthOfInput + 1) 
        * (lengthOfInput + 1)
        + y
        * (lengthOfInput + 1)
        + z

    let hashNonterm x y z = (x
                                 * (lengthOfInput + 1)
                                 * (lengthOfInput + 1)
                                 + y
                                 * (lengthOfInput + 1)
                                 + z)
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
    let dummyNode = -1<nodeMeasure>
    let dummyAST = new TerminalNode(-1<token>, packExtension -1 -1)
    let epsilon = -1<token>
    let unpackNode = function
        | TreeNode x -> x
        | _ -> failwith "Wrong type"

    let nonTerminalNodes = new Dictionary<int, int<nodeMeasure>>()
    let packedNodes = new Dictionary<int, int<nodeMeasure>>()
    let intermidiateNodes = new Dictionary<int, int<nodeMeasure>>()
    let terminalNodes = new BlockResizeArray<int<nodeMeasure>>()
    let epsilonNodes = new BlockResizeArray<int<nodeMeasure>>()
    member this.Nodes = new BlockResizeArray<INode>()

    member this.FindSppfNode (t : TypeOfNode) lExt rExt : int<nodeMeasure> =
        match t with 
        | Nonterm state ->
            let key = hashNonterm lExt rExt (int state)
            let contains, n = nonTerminalNodes.TryGetValue key
            if not contains
            then
                let newNode = new NonTerminalNode(state, (packExtension lExt rExt))
                this.Nodes.Add(newNode)
                let num = (this.Nodes.Length - 1)*1<nodeMeasure>
                nonTerminalNodes.Add(key, num)
                num
            else n
        | Intermed state -> 
            let key = hashIntermed lExt rExt (int state)
            let contains, n = intermidiateNodes.TryGetValue key
            if not contains
            then
                let newNode = new IntermidiateNode(state, (packExtension lExt rExt))
                this.Nodes.Add(newNode)
                let num = (this.Nodes.Length - 1)*1<nodeMeasure>
                intermidiateNodes.Add(key, num)
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
    

    member this.GetNodeT (symbol : int<token>) (pos : int<positionInInput>) =
        let index = int pos
        if symbol = epsilon
        then
            if epsilonNodes.Item index <> Unchecked.defaultof<int<nodeMeasure>>
            then
                TreeNode(epsilonNodes.Item index)
            else
                let t = new EpsilonNode(packExtension index index)
                this.Nodes.Add t
                let res = this.Nodes.Length - 1
                epsilonNodes.[index] <- ((this.Nodes.Length - 1)*1<nodeMeasure>)
                TreeNode(res * 1<nodeMeasure>)
        else
            if terminalNodes.Item index <> Unchecked.defaultof<int<nodeMeasure>>
            then
                TreeNode(terminalNodes.Item index)
            else
                let t = new TerminalNode(symbol, packExtension index (index + 1))
                this.Nodes.Add t
                let res = this.Nodes.Length - 1
                terminalNodes.[index] <- ((this.Nodes.Length - 1)*1<nodeMeasure>)
                TreeNode(res * 1<nodeMeasure>)
    
    member this.GetNodeP (state : int<positionInGrammar>) (t : TypeOfNode) currentN currentR = 
        let currR = this.Nodes.Item (int currentR)
        let extR = currR.getExtension ()
        let lExtR, rExtR = getLeftExtension extR, getRightExtension extR
         
        if currentN <> dummyNode
        then
            let currL = this.Nodes.Item (int currentN)
            let extL = currL.getExtension ()
            let lExtL, _ = getLeftExtension extL, getRightExtension extL
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