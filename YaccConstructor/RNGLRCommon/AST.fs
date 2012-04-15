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
    | Inner of int * MultiAST<'TokenType> []

/// Family of children - For one nonTerminal there can be a lot of dirivation trees.
/// int - reference to result.
and MultiAST<'TokenType> =
    | NonTerm of AST<'TokenType> list ref * int ref
    | Term of 'TokenType

let inline getFamily node =
    match node with
    | NonTerm (list,_) -> list
    | Term _ -> failwith "Attempt to get family of terminal"

let isEpsilon = function
    | Epsilon -> true
    | _ -> false

let isInner = function
    | Inner _ -> true
    | _ -> false

let chooseSingleAst (ast : MultiAST<_>) = 
    let stack = new System.Collections.Generic.Stack<_>()
    //let stack = new ResizeArray<_>()
    let result = ref ast
    stack.Push(ast, None)
    while stack.Count > 0 do
        let cur, pos = stack.Pop()
        let newTree = 
            match cur with
            | Term a -> Term a
            | NonTerm (l, num) ->
                match l.Value with
                | [] -> []
                | h::_ -> [h]
                |> List.map
                   (function
                    | Epsilon -> Epsilon
                    | Inner (prod,arr) ->
                        let res = Array.zeroCreate arr.Length
                        for i = 0 to arr.Length - 1 do
                            stack.Push(arr.[i], Some (res, i))
                        Inner (prod,res)
                    )
                |> (fun x -> NonTerm(ref x, num))
        
        match pos with
        | None -> result := newTree
        | Some (arr, pos) -> arr.[pos] <- newTree
    !result

    
let rec printAst ind (ast : MultiAST<_>) =
    let printInd num (x : 'a) =
        printf "%s" (String.replicate (num * 4) " ")
        printfn x
    match ast with
    | Term t -> printInd ind "t: %A" t
    | NonTerm (l,num) ->
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

let astToDot<'a> (startInd : int) (indToString : int -> string) (ruleToChildren : int -> seq<int>) (path : string) (ast : MultiAST<'a>) =
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
    let rec inner (ast : MultiAST<_>) ind =
        if nodeToNumber.ContainsKey <| ast then
            nodeToNumber.[ast]
        else
            let res = next()
            match ast with
            | Term t ->
                createNode res (ast :> Object) ("t " + indToString ind)
            | NonTerm (l, num) ->
                createNode res (ast :> Object) ("n " + indToString ind)
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
    
