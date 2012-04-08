//  AST.fs contains description of derivation tree.
//
//  Copyright 2011-2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Yard.Generators.RNGLR.AST
open System

type AST<'TokenType> =
    | Epsilon
    /// Non-terminal expansion: production, family of children
    | Inner of int * Node<'TokenType> []

/// Family of children - For one nonTerminal there can be a lot of dirivation trees
and MultiAST<'TokenType> =
    | NonTerm of AST<'TokenType> list ref
    | Term of 'TokenType
/// Node : Family of children and reference to result
and Node<'TokenType> = MultiAST<'TokenType> * int ref

let inline getFamily node =
    match fst node with
    | NonTerm list -> list
    | Term _ -> failwith "Attempt to get family of terminal"

let isEpsilon = function
    | Epsilon -> true
    | _ -> false

let isInner = function
    | Inner _ -> true
    | _ -> false

let chooseSingleAst (ast : Node<_>) = 
    let stack = ref []
    let result = ref ast
    stack := (ast, None):: !stack
    while not stack.Value.IsEmpty do
        let cur, pos = stack.Value.Head
        stack := stack.Value.Tail
        let newTree = 
            match fst cur with
            | Term a -> Term a
            | NonTerm l ->
                match l.Value with
                | [] -> []
                | h::_ -> [h]
                |> List.map
                   (function
                    | Epsilon -> Epsilon
                    | Inner (prod,arr) ->
                        let res = Array.zeroCreate arr.Length
                        for i = 0 to arr.Length - 1 do
                            stack := (arr.[i], Some (res, i)) :: !stack
                        Inner (prod,res)
                    )
                |> ref
                |> NonTerm
            |> (fun res -> res, snd cur)
        
        match pos with
        | None -> result := newTree
        | Some (arr, pos) -> arr.[pos] <- newTree
    !result

    
let rec printAst ind (ast : Node<_>) =
    let printInd num (x : 'a) =
        printf "%s" (String.replicate (num * 4) " ")
        printfn x
    match fst ast with
    | Term t -> printInd ind "t: %A" t
    | NonTerm l ->
        match !l with
        | [] -> ()
        | l ->  if l.Length > 1 then printInd ind "^^^^"
                l |> List.iteri 
                    (fun i x -> if i > 0 then
                                    printInd ind "----"
                                match x with
                                | Epsilon -> printInd ind "e"
                                | Inner (num, children) ->
                                    printInd ind "prod %d" num
                                    children
                                    |> Array.iter (printAst <| ind+1))
                if l.Length > 1 then printInd ind "vvvv"

let astToDot<'a> (startInd : int) (indToString : int -> string) (ruleToChildren : int -> seq<int>) (path : string) (ast : Node<'a>) =
    let next =
        let cur = ref 0
        fun () ->
            incr cur
            !cur

    let nodeToNumber = new System.Collections.Hashtable({new Collections.IEqualityComparer with
                                                                override this.Equals (x1, x2) = Object.ReferenceEquals (x1, x2)
                                                                override this.GetHashCode x = x.GetHashCode()})
    use out = new System.IO.StreamWriter (path : string)
    out.WriteLine("digraph AST {")
    let createNode num node (str : string) =
        if node <> null then
            nodeToNumber.[node] <- num
        let label = str.Replace("\n", "\\n").Replace ("\r", "")
        out.WriteLine ("    " + num.ToString() + " [label=\"" + label + "\"" + "]")
    let createEdge b e isBold (str : string) =
        let label = str.Replace("\n", "\\n").Replace ("\r", "")
        let bold = 
            if not isBold then ""
            else "style=bold,width=5,"
        out.WriteLine ("    " + b.ToString() + " -> " + e.ToString() + " [" + bold + "label=\"" + label + "\"" + "]")
    let rec inner (ast : Node<_>) ind =
        if nodeToNumber.ContainsKey <| fst ast then
            nodeToNumber.[fst ast]
        else
            let res = next()
            match fst ast with
            | Term t ->
                createNode res (fst ast :> Object) ("t " + indToString ind)
            | NonTerm l ->
                createNode res (fst ast :> Object) ("n " + indToString ind)
                !l |> List.iter
                       (function
                        | Epsilon ->
                            let u = next()
                            createNode u null "eps"
                            createEdge res u true ""
                        | Inner (num, children) ->
                            let i = ref 0
                            let u = next()
                            createNode u null ("prod " + num.ToString())
                            createEdge res u true ""
                            for child in ruleToChildren num do
                                let v = inner children.[!i] child
                                createEdge u v false ""
                                incr i
                        )
            box res
    inner ast startInd |> ignore
    out.WriteLine("}")
    out.Close()
    
