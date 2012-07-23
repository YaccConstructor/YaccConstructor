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
open Microsoft.FSharp.Text.Lexing

/// Family of children - For one nonTerminal there can be a lot of dirivation trees.
/// int - number of token, if there is an epsilon-tree derivation, -1 otherwise.
type AST<'TokenType> =
    /// Non-terminal expansion: production, family of children
    /// All nodes are stored in array, so there is a correspondence between integer and node.
    | NonTerm of (ResizeArray<int * int []>)
    | Epsilon of int
    | Term of 'TokenType

let inline getFamily node =
    match node with
    | NonTerm list -> list
    | Epsilon _ -> failwith "Attempt to get family of epsilon"
    | Term _ -> failwith "Attempt to get family of terminal"

[<AllowNullLiteral>]
type Tree<'TokenType> (nodes : ResizeArray<AST<'TokenType>>, root : int) =
    let scaleSize = int <| double nodes.Count * 1.2
    let reachable = new ResizeArray<_>(scaleSize)
    let order =
        for i = 0 to nodes.Count-1 do
            reachable.Add false
        let stack = new System.Collections.Generic.Stack<_>()
        stack.Push root
        let res = new ResizeArray<_>(int <| double nodes.Count * 1.2)
        
        while stack.Count > 0 do
            let u = stack.Pop()
            if u < 0 then
                res.Add <| -u-1
            elif not reachable.[u] then
                stack.Push <| -u-1
                reachable.[u] <- true
                match nodes.[u] with
                | NonTerm children ->
                    children |> ResizeArray.iter (
                        fun (_,family) ->
                            for j = 0 to family.Length - 1 do
                                if not reachable.[family.[j]] then
                                    match nodes.[family.[j]] with
                                    | Epsilon eps ->
                                        family.[j] <- nodes.Count
                                        nodes.Add <| Epsilon eps
                                        reachable.Add true
                                        res.Add family.[j]
                                    | Term _ ->
                                        reachable.[family.[j]] <- true
                                        res.Add family.[j]
                                    | _ -> stack.Push family.[j])
                | _ -> ()
        res.ToArray()

    let pos =
        let ret = Array.zeroCreate nodes.Count
        for i = 0 to order.Length-1 do
            ret.[order.[i]] <- i
        ret
    
    member this.Nodes = nodes
    member this.Order = order

    member this.EliminateCycles() =
        let proper = Array.create nodes.Count true
        for x in order do
            match nodes.[x] with
            | NonTerm children ->
                children.RemoveAll (
                    fun (_, children) ->
                        children
                        |> Array.forall (fun j -> pos.[j] < pos.[x] && reachable.[j])
                        |> not)
                |> ignore
                if children.Count = 0 then
                    reachable.[x] <- false
            | _ -> ()
                
    member this.ChooseSingleAst () = 
        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | NonTerm children ->
                    match
                        children |> ResizeArray.tryFind
                            (fun (_, children) ->
                                children
                                |> Array.forall (fun j -> pos.[j] < pos.[x] && reachable.[j]))
                        with
                    | Some v ->
                        if children.Count > 1 then
                            children.Clear()
                            children.Add v
                    | None ->
                        reachable.[x] <- false
                        children.Clear()
                | _ -> ()
        for x in order do
            reachable.[x] <- false
        reachable.[root] <- true
        for i = order.Length-1 downto 0 do
            let x = order.[i]
            if reachable.[x] then
                match nodes.[x] with
                | NonTerm x ->
                    for j in snd x.[0] do
                        reachable.[j] <- true
                | _ -> ()

    member private this.TranslateEpsilon (funs : array<_>) (leftSides : array<_>) (concat : array<_>) (range : 'Position * 'Position) : obj =
        let result = Array.zeroCreate nodes.Count
        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | NonTerm children ->
                    result.[x] <- 
                        if children.Count = 1 then
                            funs.[fst children.[0]] (snd children.[0] |> Array.map (fun i -> result.[i])) range
                        else
                            children
                            |> ResizeArray.map (
                                fun (prod, children) ->
                                    funs.[prod] (children |> Array.map (fun i -> result.[i])) range
                                )
                            |> ResizeArray.toList
                            |> concat.[leftSides.[fst children.[0]]]
                | x -> failwith "%A was not expected in epsilon-translation" x
        result.[root]
            
    member this.Translate (funs : array<obj[] -> 'Position * 'Position -> obj>) (leftSides : array<_>)
                            (concat : array<_>) (epsilons : array<Tree<_>>) (tokenToRange) =
        let result = Array.zeroCreate nodes.Count
        let ranges : ('Position * 'Position)[] = Array.zeroCreate nodes.Count
        // Set Positions
        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | Term t -> ranges.[x] <- tokenToRange t
                | Epsilon _ -> ()
                | NonTerm children ->
                    let family = snd children.[0]
                    let mutable j, k = 0, family.Length-1
                    let inline isEpsilon x = match nodes.[x] with Epsilon _ -> true | _ -> false
                    while isEpsilon family.[j] do
                        j <- j + 1
                    while isEpsilon family.[k] do
                        k <- k - 1
                    let beginPos = fst ranges.[family.[j]]
                    ranges.[x] <- beginPos, (snd ranges.[family.[k]])
                    //let beginRange = beginPos, beginPos
                    children |> ResizeArray.iter
                        (fun (_,family) ->
                            if isEpsilon family.[0] then
                                ranges.[family.[0]] <- beginPos, beginPos
                            for i = 1 to family.Length-1 do
                                if isEpsilon family.[i] then
                                    let end' = snd ranges.[family.[i-1]]
                                    ranges.[family.[i]] <- end', end'
                        )

        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | Term t ->
                    result.[x] <- box t
                | Epsilon eps ->
                    result.[x] <- epsilons.[eps].TranslateEpsilon funs leftSides concat ranges.[x]//epsilons
                | NonTerm children ->
                    result.[x] <- 
                        if children.Count = 1 then
                            funs.[fst children.[0]] (snd children.[0] |> Array.map (fun i -> result.[i])) ranges.[x]
                        else
                            children
                            |> ResizeArray.map (
                                fun (prod, children) ->
                                    funs.[prod] (children |> Array.map (fun i -> result.[i])) ranges.[x]
                                )
                            |> ResizeArray.toList
                            |> concat.[leftSides.[fst children.[0]]]
        result.[root]
            
    member this.PrintAst() =            
        let rec printAst ind ast =
            let printInd num (x : 'a) =
                printf "%s" (String.replicate (num * 4) " ")
                printfn x
            match nodes.[ast] with
            | Term t -> printInd ind "t: %A" t
            | Epsilon _ -> printInd ind "e"
            | NonTerm children ->
                if children.Count > 0 then
                    let needGroup = children.Count > 1
                    if needGroup then printInd ind "^^^^"
                    children |> ResizeArray.iteri 
                        (fun i (num, children) ->
                                    if i > 0 then
                                        printInd ind "----"
                                    printInd ind "prod %d" num
                                    children
                                    |> Array.iter (printAst <| ind+1))
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
                | Epsilon eps ->
                    createNode res (ast :> Object) ("n " + indToString ind)
                    let u = next()
                    createNode u null "eps"
                    createEdge res u true ""
                | NonTerm children ->
                    createNode res (ast :> Object) ("n " + indToString ind)
                    children |> ResizeArray.iter
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
                box res
        inner nodes.[root] startInd |> ignore
        out.WriteLine("}")
        out.Close()
    