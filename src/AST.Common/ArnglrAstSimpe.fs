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

module Yard.Generators.Common.ARNGLR.SimpleAST
open System
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
open Microsoft.FSharp.Collections

/// Arguments for tanslation calling, seen by user
type TranslateArguments<'Token, 'Position> = {
    tokenToRange : 'Token -> 'Position * 'Position
    zeroPosition : 'Position
    clearAST : bool
    filterEpsilons : bool
}

/// Family of children - For one nonTerminal there can be a lot of dirivation trees.
/// int - number of token, if there is an epsilon-tree derivation, -1 otherwise.
type AST<'TokenType> =
    /// Non-terminal expansion: production, family of children
    /// All nodes are stored in array, so there is a correspondence between integer and node.
    | NonTerm of (ResizeArray<int * int []>)
    | Term of 'TokenType

let inline getFamily node =
    match node with
    | NonTerm list -> list
    | Term _ -> failwith "Attempt to get family of terminal"

[<AllowNullLiteral>]
type Tree<'TokenType> (nodes : array<AST<'TokenType>>, root : int) =
    let scaleSize = int <| double nodes.Length * 1.2
    let reachable = Array.zeroCreate nodes.Length
    let isEpsilon = root < 0
    let order =
        let stack = new System.Collections.Generic.Stack<_>()
        stack.Push root
        let res = new ResizeArray<_>(scaleSize)
        if not isEpsilon then
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
                                    if family.[j] >= 0 && not reachable.[family.[j]] then
                                        match nodes.[family.[j]] with
                                        | Term _ ->
                                            reachable.[family.[j]] <- true
                                            res.Add family.[j]
                                        | _ -> stack.Push family.[j])
                    | _ -> ()
        res.ToArray()

    let pos =
        let ret = Array.zeroCreate nodes.Length
        for i = 0 to order.Length-1 do
            ret.[order.[i]] <- i
        ret
    
    member this.Nodes = nodes
    member this.Order = order

    member this.EliminateCycles() =
        if not isEpsilon then
            let proper = Array.create nodes.Length true
            for x in order do
                match nodes.[x] with
                | NonTerm children ->
                    children.RemoveAll (
                        fun (_, children) ->
                            children
                            |> Array.forall (fun j -> j < 0 || pos.[j] < pos.[x] && reachable.[j])
                            |> not)
                    |> ignore
                    if children.Count = 0 then
                        reachable.[x] <- false
                | _ -> ()
                
    member this.ChooseSingleAst () = 
        if not isEpsilon then
            for x in order do
                if reachable.[x] then
                    match nodes.[x] with
                    | NonTerm children ->
                        match
                            children |> ResizeArray.tryFind
                                (fun (_, children) ->
                                    children
                                    |> Array.forall (fun j -> j < 0 || pos.[j] < pos.[x] && reachable.[j]))
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
                            if j >= 0 then
                                reachable.[j] <- true
                    | _ -> ()

    member private this.TranslateEpsilon (funs : array<_>) (leftSides : array<_>) (concat : array<_>) (range : 'Position * 'Position) : obj =
        let result = Array.zeroCreate nodes.Length
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
    
    member this.SetRanges tokenToRange =
        let ranges : ('Position * 'Position)[] = Array.zeroCreate nodes.Length
        // Set Positions
        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | Term t -> ranges.[x] <- tokenToRange t
                | NonTerm children ->
                    let family = snd children.[0]
                    let mutable j, k = 0, family.Length-1
                    let inline isEpsilon x = x < 0
                    while isEpsilon family.[j] do
                        j <- j + 1
                    while isEpsilon family.[k] do
                        k <- k - 1
                    ranges.[x] <- (fst ranges.[family.[j]]), (snd ranges.[family.[k]])
        ranges

    member this.collectWarnings tokenToRange =
        let ranges = this.SetRanges tokenToRange
        let res = new ResizeArray<_>()
        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | Term _ -> ()
                | NonTerm children ->
                    if children.Count > 1 then
                        res.Add (ranges.[x], ResizeArray.map fst children)
        res

    member this.Translate (funs : array<obj[] -> 'Position * 'Position -> obj>) (leftSides : array<_>)
                            (concat : array<_>) (epsilons : array<Tree<_>>) (tokenToRange) (zeroPosition :'Position) =
        if isEpsilon then epsilons.[-root-1].TranslateEpsilon funs leftSides concat (zeroPosition, zeroPosition)
        else
            let result = Array.zeroCreate nodes.Length
            let ranges = this.SetRanges tokenToRange
            let inline getRes prevRange i = 
                if i < 0 then epsilons.[-i-1].TranslateEpsilon funs leftSides concat (!prevRange, !prevRange)
                else
                    prevRange := snd ranges.[i]
                    result.[i]
            for x in order do
                if reachable.[x] then
                    match nodes.[x] with
                    | Term t ->
                        result.[x] <- box t
                    | NonTerm children ->
                        result.[x] <-
                            if children.Count = 1 then
                                let prevRange = fst ranges.[x] |> ref
                                funs.[fst children.[0]] (snd children.[0] |> Array.map (getRes prevRange)) ranges.[x]
                            else
                                children
                                |> ResizeArray.map (
                                    fun (prod, children) ->
                                        let prevRange = fst ranges.[x] |> ref
                                        funs.[prod] (children |> Array.map (getRes prevRange)) ranges.[x]
                                    )
                                |> ResizeArray.toList
                                |> concat.[leftSides.[fst children.[0]]]
            result.[root]
            
    member this.PrintAst() =            
        let rec printAst ind ast =
            let printInd num (x : 'a) =
                printf "%s" (String.replicate (num * 4) " ")
                printfn x
            if ast < 0 then printInd ind "e"
            else
                match nodes.[ast] with
                | Term t -> printInd ind "t: %A" t
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

    member this.AstToDot (indToString : int -> string) tokenToNumber (leftSide : array<int>) (path : string) =
        let next =
            let cur = ref nodes.Length
            fun () ->
                incr cur
                !cur

        let nodeToNumber = new System.Collections.Hashtable({new Collections.IEqualityComparer with
                                                                    override this.Equals (x1, x2) = Object.ReferenceEquals (x1, x2)
                                                                    override this.GetHashCode x = x.GetHashCode()})
        use out = new System.IO.StreamWriter (path : string)
        out.WriteLine("digraph AST {")
        let createNode num isAmbiguous (str : string) =
            let label =
                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
                if not isAmbiguous then cur
                else cur + " !"
            let color =
                if not isAmbiguous then ""
                else ",style=\"filled\",fillcolor=red"
            out.WriteLine ("    " + num.ToString() + " [label=\"" + label + "\"" + color + "]")
        let createEdge (b : int) (e : int) isBold (str : string) =
            let label = str.Replace("\n", "\\n").Replace ("\r", "")
            let bold = 
                if not isBold then ""
                else "style=bold,width=10,"
            out.WriteLine ("    " + b.ToString() + " -> " + e.ToString() + " [" + bold + "label=\"" + label + "\"" + "]")
        let createEpsilon ind = 
            let res = next()
            createNode res false ("n " + indToString (-1-ind))
            let u = next()
            createNode u false "eps"
            createEdge res u true ""
            res
        if not isEpsilon then
            for i in order do
                if reachable.[i] then
                    let ast = nodes.[i]
                    match ast with
                    | Term t ->
                        createNode i false  ("t " + indToString (tokenToNumber t))
                    | NonTerm children ->
                        createNode i (children.Count > 1) ("n " + indToString leftSide.[fst children.[0]])
                        children |> ResizeArray.iter
                                (fun (num, children) ->
                                    let u = next()
                                    createNode u false ("prod " + num.ToString())
                                    createEdge i u true ""
                                    for child in children do
                                        let v =
                                            if child >= 0 then child
                                            else createEpsilon child
                                        createEdge u v false ""
                                )
        else createEpsilon root |> ignore
        
        out.WriteLine("}")
        out.Close()
    