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
type private DotNodeType = Prod | AstNode | IntermidiateNode
let inline getRule packedValue = int packedValue >>> 16
let inline getPosition (packedValue : int) = int (packedValue &&& 0xffff)


[<AllowNullLiteral>]
type Tree<'TokenType> (tokens : array<'TokenType>, root : obj, rules : int[][]) =
    let rootFamily, isEpsilon =
        match root with
        | :? AST as ast -> ast, false
        | :? int as x when x < 0 -> Unchecked.defaultof<_>, true
        | _ -> failwith "Strange tree - singleNode with non-negative value"

    member this.Root = root
    member this.RulesCount = rules.GetLength(0)

  

    member this.AstToDot (indToString : int -> string) tokenToNumber (leftSide : array<int>) (path : string) =
        
        let nodeToNumber = new System.Collections.Hashtable({new Collections.IEqualityComparer with
                                                                    override this.Equals (x1, x2) = Object.ReferenceEquals (x1, x2)
                                                                    override this.GetHashCode x = x.GetHashCode()})
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
                | Prod -> ""
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
       
        let num = ref 0
        let rec dfs (node : obj) parentNum =
            num := !num + 1
            let cur = !num 
            match node with 
            | :? AST as a -> 
                createNode cur false AstNode (indToString leftSide.[a.first.prod])
                createEdge parentNum cur false ""
                dfs a.first cur
                if a.other <> Unchecked.defaultof<_> 
                then 
                    for t in a.other do
                        dfs t cur
            | :? Family as f ->
                createNode cur false AstNode (indToString leftSide.[f.prod])
                createEdge parentNum cur false ""
                dfs f.node cur
            | :? IntermidiateNode as i ->
                createNode cur false AstNode ((getRule i.Position).ToString() + " " + (getPosition i.Position).ToString())
                createEdge parentNum cur false ""
                dfs i.LeftChild cur
                dfs i.RightChild cur
            | :? TerminalNode as t ->
                createNode cur false AstNode ("t " + indToString (tokenToNumber tokens.[t.value]))
                createEdge parentNum cur false ""
            | null -> ()
            
        let root = root :?> AST
        createNode !num false AstNode (indToString leftSide.[root.first.prod])
        dfs root.first !num

        out.WriteLine("}")
        out.Close()

