//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module Yard.Generators.Common.AST2
open System
open System.Collections.Generic
open Yard.Generators.Common.DataStructures

[<Measure>] type key
[<Measure>] type extension

/// Arguments for tanslation calling, seen by user
type TranslateArguments<'Token, 'Position> = {
    tokenToRange : 'Token -> 'Position * 'Position
    zeroPosition : 'Position
    clearAST : bool
    filterEpsilons : bool
}

[<AllowNullLiteral>]
type INode = interface end

[<Struct>]
type UsualOne<'T> =
    val mutable first : 'T
    val mutable other : 'T[]
    new (f,o) = {first = f; other = o}

[<Struct>]
type NumNode =
    val Num : int
    val Node : obj
    new (num, node) = {Num = num; Node = node} 

/// Non-terminal expansion: production, family of children
/// All nodes are stored in array, so there is a correspondence between integer and node.
/// Family of children - For one nonTerminal there can be a lot of derivation trees.
[<AllowNullLiteral>]
type AST =
    interface INode
    val mutable first : Family
    val mutable other : Family[]
    new (f, o) = {first = f; other = o}
    member inline this.findFamily f =
        if f this.first then Some this.first
        elif this.other <> null then
            Array.tryFind f this.other
        else None

and Family =
    struct
        val prod : int
        val node : INode
        new (p, n) = {prod = p; node = n}
    end

and TerminalNode =
    struct
        interface INode
        val mutable value : int
        new(v) = {value = v}
    end

and [<CustomEquality;NoComparison>] IntermidiateNode =
    struct
    interface INode
        val LeftChild  : INode
        val RightChild : INode
        val Position   : Int32
        override x.Equals(intermidiateNode) =                
                match intermidiateNode with
                | :? IntermidiateNode as i ->
                    let y =  
                        i.Position = x.Position
                        && (i.RightChild = x.RightChild)
                        && (i.LeftChild = x.LeftChild)
                    y
                | _ -> false
//        interface System.IComparable with
//            member x.CompareTo n =
//                match n with
//                | :? AST as a -> compare 2 3  //temp
//                | _ -> invalidArg "yobj" "cannot compare values of different types"
        new (l, r, p) = {LeftChild = l; RightChild = r; Position = p}
    end

let inline getFamily (node : obj) =
    match node with
    | :? AST as ast -> ast
    | _ -> failwith "Attempt to get family of not-AST"

let inline getSingleNode (node : obj) =
    match node with
    | :? int as i  -> i
    | _ -> failwith "Attempt to get singleNode of NonTerm"

//let inline private setPos p (x : AST) = match x with NonTerm n -> n.pos <- p | SingleNode _ -> failwith "Attempt to get num of single node"

let private emptyArr = [||]
type private DotNodeType = FamilyNode | AstNode | Intermidiate | Terminal

let inline packExtension left right : int64<extension> =  LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)


let inline packLabel rule position = (int rule <<< 16) ||| int position
let inline getRule packedValue = int packedValue >>> 16
let inline getPosition (packedValue : int) = int (packedValue &&& 0xffff)

let inline pack3ToInt64 p l r : int64<key>        = LanguagePrimitives.Int64WithMeasure (((int64 p) <<< 52) ||| ((int64 l) <<< 26) ||| (int64 r))
let inline getProduction (long : int64<key>)      = int (int64 long >>> 52)
let inline getLeftExtension3 (long : int64<key>)  = int((int64 long <<< 12) >>> 38)
let inline getRightExtension3 (long : int64<key>) = int((int64 long <<< 38) >>> 38)

[<AllowNullLiteral>]
type Tree<'TokenType> (tokens : array<'TokenType>, root : obj, rules : int[][]) =
    let rootFamily, isEpsilon =
        match root with
        | :? AST as ast -> ast, false
        | :? int as x when x < 0 -> Unchecked.defaultof<_>, true
        | _ -> failwith "Strange tree - singleNode with non-negative value"

    member this.AstToDot (indToString : int -> string) tokenToNumber (leftSide : array<int>) (path : string) =
        use out = new System.IO.StreamWriter (path : string)
        out.WriteLine("digraph AST {")

        let createNode num isAmbiguous nodeType (str : string) =
            let label =
                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
                if not isAmbiguous then cur
                else cur + " !"
            let shape =
                match nodeType with
                | AstNode -> ",shape=box"
                | FamilyNode -> ",shape=box"
                | Terminal -> ",shape=box"
                | Intermidiate -> ",shape=oval"
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
                    | :? AST as a -> 
                   
                        createNode !num false AstNode (indToString leftSide.[a.first.prod])
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode(!num, a.first))
                        if a.other <> Unchecked.defaultof<_> 
                        then 
                            for t in a.other do
                                nodeQueue.Enqueue(new NumNode(!num, t))
                    | :? Family as f ->
                        createNode !num false FamilyNode ("fam " + indToString leftSide.[f.prod])
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode(!num, f.node))
                    | :? IntermidiateNode as i ->
                        createNode !num false Intermidiate ((getRule i.Position).ToString() + " " + (getPosition i.Position).ToString())
                        createEdge currentPair.Num !num false ""
                        if i.LeftChild <> null then nodeQueue.Enqueue(new NumNode(!num, i.LeftChild))
                        if i.RightChild <> null then nodeQueue.Enqueue(new NumNode(!num, i.RightChild))
                    | :? TerminalNode as t ->
                        createNode !num false Terminal ("t " + indToString (tokenToNumber tokens.[t.value]))
                        createEdge currentPair.Num !num false ""
                    | null -> ()
            else
                let a = currentPair.Node :?> AST
                num := !num + 1
                createNode !num false AstNode (indToString leftSide.[a.first.prod])
                nodeQueue.Enqueue(new NumNode(!num, a.first))
                if a.other <> Unchecked.defaultof<_> 
                then 
                    for t in a.other do
                        nodeQueue.Enqueue(new NumNode(!num, t))
        

        out.WriteLine("}")
        out.Close()

