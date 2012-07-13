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

type Child =
    /// Non-terminal expansion: production, family of children
    /// All nodes are stored in array, so there is a correspondence between integer and node.
    int * int []

/// Family of children - For one nonTerminal there can be a lot of dirivation trees.
/// int - number of token, if there is an epsilon-tree derivation, -1 otherwise.
type AST<'TokenType> =
    | NonTerm of (Child list ref) * int ref
    | Term of 'TokenType

let inline getFamily node =
    match node with
    | NonTerm (list, eps) -> list
    | Term _ -> failwith "Attempt to get family of terminal"

let inline getEpsilon node =
    match node with
    | NonTerm (list, eps) -> eps
    | Term _ -> failwith "Attempt to get epsilon of terminal"

[<AllowNullLiteral>]
type Tree<'TokenType> (nodes : AST<'TokenType>[], root : int) =
    let pos = Array.create nodes.Length -1
    let reachable = Array.zeroCreate nodes.Length
    let order =
        let stack = new System.Collections.Generic.Stack<_>()
        stack.Push root
        let res = new ResizeArray<_>(nodes.Length)
        let inline iterChildren children =
            for j in snd children do
                if not reachable.[j] then
                    stack.Push j
            
        while stack.Count > 0 do
            let u = stack.Pop()
            if u < 0 then
                res.Add <| -u-1
            else
                stack.Push <| -u-1
                reachable.[u] <- true
                match nodes.[u] with
                | Term _ -> ()
                | NonTerm (list, eps) ->
                    !list |> List.iter iterChildren
            
        let ret = res.ToArray()
        for i = 0 to ret.Length-1 do
            pos.[ret.[i]] <- i
        ret

    member this.Nodes = nodes
    member this.Order = order

    member this.EliminateCycles() =
        let proper = Array.create nodes.Length true
        for x in order do
            match nodes.[x] with
            | Term _ -> ()
            | NonTerm (list, eps) ->
                list := 
                    !list |> List.filter (
                        fun (_, children) ->
                            children
                            |> Array.forall (fun j -> pos.[j] < pos.[x] && reachable.[j]))
                if list.Value.IsEmpty then
                    reachable.[x] <- false
                
    member this.ChooseSingleAst () = 
        this.EliminateCycles()
        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | Term _ -> ()
                | NonTerm (list, eps) ->
                    match !list with
                    | h::_::_ -> list := [h]
                    | _ -> ()
        for x in order do
            reachable.[x] <- false
        reachable.[root] <- true
        let inline iterChildren children =
            for j in snd children do
                reachable.[j] <- true
        for i = order.Length-1 downto 0 do
            let x = order.[i]
            if reachable.[x] then
                match nodes.[x] with
                | Term _ -> ()
                | NonTerm (list, eps) ->
                    !list |> List.iter iterChildren

    member this.Translate (funs : array<_>) (leftSides : array<_>) (concat : array<_>) (epsilons : array<Tree<_>>) =
        let result = Array.zeroCreate nodes.Length
        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | Term t -> result.[x] <- box t
                | NonTerm (list, eps) ->
                    let res = 
                        !list
                        |> List.map (
                            fun (prod, children) ->
                                funs.[prod] (children |> Array.map (fun i -> result.[i]))
                            )
                        |> (fun r ->
                                if !eps = -1 then r
                                else (epsilons.[!eps].Translate funs leftSides concat epsilons)::r)
                    result.[x] <- 
                        match res with
                        | [single] -> single
                        | _ ->
                            let nonTerm =
                                if !eps <> -1 then !eps
                                else leftSides.[fst list.Value.Head]
                            concat.[nonTerm] res
        result.[root]
            
    member this.PrintAst() =            
        let rec printAst ind ast =
            let printInd num (x : 'a) =
                printf "%s" (String.replicate (num * 4) " ")
                printfn x
            match nodes.[ast] with
            | Term t -> printInd ind "t: %A" t
            | NonTerm (l, eps) ->
                match !l with
                | [] -> ()
                | l ->  let needGroup =
                            l.Length + (if !eps <> -1 then 1 else 0) > 1
                        if needGroup then printInd ind "^^^^"
                        l |> List.iteri 
                            (fun i (num, children) ->
                                        if i > 0 then
                                            printInd ind "----"
                                        printInd ind "prod %d" num
                                        children
                                        |> Array.iter (printAst <| ind+1))
                        if !eps <> -1 then
                            if needGroup then
                                printInd ind "----"
                            printInd ind "e"
                        if needGroup then printInd ind "vvvv"
        printAst 0 root

    member this.AstToDot (startInd : int) (indToString : int -> string) (ruleToChildren : int -> seq<int>) (path : string) =
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
        let rec inner ast ind =
            if nodeToNumber.ContainsKey <| ast then
                nodeToNumber.[ast]
            else
                let res = next()
                match ast with
                | Term t ->
                    createNode res (ast :> Object) ("t " + indToString ind)
                | NonTerm (l, eps) ->
                    createNode res (ast :> Object) ("n " + indToString ind)
                    !l |> List.iter
                           (fun (num, children) ->
                                let i = ref 0
                                let u = next()
                                createNode u null ("prod " + num.ToString())
                                createEdge res u true ""
                                for child in ruleToChildren num do
                                    let v = inner nodes.[children.[!i]] child
                                    createEdge u v false ""
                                    incr i
                            )
                    if !eps <> -1 then
                        let u = next()
                        createNode u null "eps"
                        createEdge res u true ""
                box res
        inner nodes.[root] startInd |> ignore
        out.WriteLine("}")
        out.Close()
    
