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

and [<CustomEquality;CustomComparison>] IntermidiateNode =
    struct
    interface INode
        val LeftChild  : INode
        val RightChild : INode
        val Position   : Int32
        override x.Equals(intermidiateNode) =
                match intermidiateNode with
                | :? AST as a -> (obj.ReferenceEquals(x, a))
                | _ -> false
        interface System.IComparable with
            member x.CompareTo n =
                match n with
                | :? AST as a -> compare 2 3  //temp
                | _ -> invalidArg "yobj" "cannot compare values of different types"
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

type ErrorNode = 
    val errorOn : obj     // token on which error occurs
    val production : int  // now it doesn't work. Production number where error occured
    val expected : array<string>  // parser was expecting one of the these tokens

    val mutable tokens : array<obj>  //skipped tokens during error recovery
    val recTokens : array<string> // parser was look for one of these tokens during recovery

    new (errOn, prod, exp, (*skip,*) recToks) = 
        {
            errorOn = errOn; 
            production = prod; 
            expected = exp; 
            tokens = Unchecked.defaultof<_>;(*skip;*)
            recTokens = recToks
        }

[<AllowNullLiteral>]
type Tree<'TokenType> (tokens : array<'TokenType>, root : obj, rules : int[][]) =
    let rootFamily, isEpsilon =
        match root with
        | :? AST as ast -> ast, false
        | :? int as x when x < 0 -> Unchecked.defaultof<_>, true
        | _ -> failwith "Strange tree - singleNode with non-negative value"


   // member this.Order = order
    member this.Root = root
    member this.RulesCount = rules.GetLength(0)



//    member this.AstToDot (indToString : int -> string) tokenToNumber (leftSide : array<int>) (path : string) =
//        let next =
//            let cur = ref order.Length
//            fun () ->
//                incr cur
//                !cur
//
//        let nodeToNumber = new System.Collections.Hashtable({new Collections.IEqualityComparer with
//                                                                    override this.Equals (x1, x2) = Object.ReferenceEquals (x1, x2)
//                                                                    override this.GetHashCode x = x.GetHashCode()})
//        use out = new System.IO.StreamWriter (path : string)
//        out.WriteLine("digraph AST {")
//        let createNode num isAmbiguous nodeType (str : string) =
//            let label =
//                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
//                if not isAmbiguous then cur
//                else cur + " !"
//            let shape =
//                match nodeType with
//                | AstNode -> ",shape=box"
//                | Prod -> ""
//            let color =
//                if not isAmbiguous then ""
//                else ",style=\"filled\",fillcolor=red"
//            out.WriteLine ("    " + num.ToString() + " [label=\"" + label + "\"" + color + shape + "]")
//        let createEdge (b : int) (e : int) isBold (str : string) =
//            let label = str.Replace("\n", "\\n").Replace ("\r", "")
//            let bold = 
//                if not isBold then ""
//                else "style=bold,width=10,"
//            out.WriteLine ("    " + b.ToString() + " -> " + e.ToString() + " [" + bold + "label=\"" + label + "\"" + "]")
//        let createEpsilon ind = 
//            let res = next()
//            createNode res false AstNode ("n " + indToString (-1 - ind))
//            let u = next()
//            createNode u false AstNode "eps"
//            createEdge res u true ""
//            res
//        let createTerm t =
//            let res = next()
//            createNode res false AstNode ("t " + indToString (tokenToNumber tokens.[t]))
//            res
//        let createTerm2 t =
//            let res = next()
//            createNode res false AstNode ("t " + indToString (tokenToNumber t))
//            res
//        if not isEpsilon then
//            //for i in order do
//            for i = order.Length - 1 downto 0 do
//                let x = order.[i]
//                if x.pos <> -1 then
//                    let children = x
//                    
//                    let label = 
//                        if children.first.prod < leftSide.Length then indToString leftSide.[children.first.prod]
//                        else "error"
//                     
//                    createNode i (children.other <> null) AstNode ("n " + label)
//                     
//                    let handle (family : Family) =
//                        let u = next()
//                        createNode u false Prod ("prod " + family.prod.ToString())
//                        createEdge i u true ""
//                        family.nodes.doForAll <| fun child ->
//                            let v = 
//                                match child with
//                                | :? AST as v -> v.pos
//                                | :? int as e when e < 0 -> createEpsilon e
//                                | :? Nodes as n -> 
//                                    let tok : 'TokenType = unbox <| n.fst
//                                    createTerm2 tok 
//                                | :? int as t -> createTerm t
//                                | _ -> failwith ""
//                            createEdge u v false ""
//                    children.first |> handle
//                    if children.other <> null then 
//                        children.other |> Array.iter handle
//        else createEpsilon (getSingleNode root) |> ignore
//        
//        out.WriteLine("}")
//        out.Close()

