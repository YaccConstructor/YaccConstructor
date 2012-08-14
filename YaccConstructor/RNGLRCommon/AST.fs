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

[<Struct>]
type Family =
    val prod : int
    val nodes : int[]
    new (p,n) = {prod = p; nodes = n}


/// Family of children - For one nonTerminal there can be a lot of dirivation trees.
/// int - number of token, if there is an epsilon-tree derivation, -1 otherwise.
type AST =
    /// Non-terminal expansion: production, family of children
    /// All nodes are stored in array, so there is a correspondence between integer and node.
    | NonTerm of array<Family>
    | Term of int

let inline getFamily node =
    match node with
    | NonTerm list -> list
    | Term _ -> failwith "Attempt to get family of terminal"

[<AllowNullLiteral>]
type Tree<'TokenType> (nodes : array<AST>, tokens : array<'TokenType>, root : int) =
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
                        for family in children do
                            let family = family.nodes
                            for j = 0 to family.Length - 1 do
                                if family.[j] >= 0 && not reachable.[family.[j]] then
                                    match nodes.[family.[j]] with
                                    | Term _ ->
                                        reachable.[family.[j]] <- true
                                        res.Add family.[j]
                                    | _ -> stack.Push family.[j]
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
                    let arr =
                        children |> Array.filter (fun family ->
                            family.nodes
                            |> Array.forall (fun j -> j < 0 || pos.[j] < pos.[x] && reachable.[j]))
                    if arr.Length = 0 then
                        reachable.[x] <- false
                    else nodes.[x] <- NonTerm arr
                | _ -> ()
                
    member this.ChooseSingleAst () = 
        if not isEpsilon then
            for x in order do
                if reachable.[x] then
                    match nodes.[x] with
                    | NonTerm children ->
                        match
                            children |> Array.tryFind
                                (fun family ->
                                    family.nodes
                                    |> Array.forall (fun j -> j < 0 || pos.[j] < pos.[x] && reachable.[j]))
                            with
                        | Some v ->
                            if children.Length > 1 then
                                nodes.[x] <- NonTerm [|v|]
                        | None ->
                            reachable.[x] <- false
                            nodes.[x] <- Unchecked.defaultof<_>
                    | _ -> ()
            for x in order do
                reachable.[x] <- false
            reachable.[root] <- true
            for i = order.Length-1 downto 0 do
                let x = order.[i]
                if reachable.[x] then
                    match nodes.[x] with
                    | NonTerm x ->
                        for j in x.[0].nodes do
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
                        if children.Length = 1 then
                            funs.[children.[0].prod] (children.[0].nodes |> Array.map (fun i -> result.[i])) range
                        else
                            children
                            |> Array.map (
                                fun family ->
                                    funs.[family.prod] (family.nodes |> Array.map (fun i -> result.[i])) range
                                )
                            |> Array.toList
                            |> concat.[leftSides.[children.[0].prod]]
                | x -> failwith "%A was not expected in epsilon-translation" x
        result.[root]
    
    member this.SetRanges tokenToRange =
        let ranges : ('Position * 'Position)[] = Array.zeroCreate nodes.Length
        // Set Positions
        for x in order do
            if reachable.[x] then
                match nodes.[x] with
                | Term t -> ranges.[x] <- tokenToRange tokens.[t]
                | NonTerm children ->
                    let family = children.[0].nodes
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
                    if children.Length > 1 then
                        res.Add (ranges.[x], children |> Array.map (fun family -> family.prod))
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
                    | Term t -> result.[x] <- box tokens.[t]
                    | NonTerm children ->
                        result.[x] <-
                            if children.Length = 1 then
                                let prevRange = fst ranges.[x] |> ref
                                funs.[children.[0].prod] (children.[0].nodes |> Array.map (getRes prevRange)) ranges.[x]
                            else
                                children
                                |> Array.map (
                                    fun family ->
                                        let prevRange = fst ranges.[x] |> ref
                                        funs.[family.prod] (family.nodes |> Array.map (getRes prevRange)) ranges.[x]
                                    )
                                |> Array.toList
                                |> concat.[leftSides.[children.[0].prod]]
            result.[root]
            
    member this.PrintAst() =            
        let rec printAst ind ast =
            let printInd num (x : 'a) =
                printf "%s" (String.replicate (num * 4) " ")
                printfn x
            if ast < 0 then printInd ind "e"
            else
                match nodes.[ast] with
                | Term t -> printInd ind "t: %A" tokens.[t]
                | NonTerm children ->
                    if children.Length > 0 then
                        let needGroup = children.Length > 1
                        if needGroup then printInd ind "^^^^"
                        children |> Array.iteri 
                            (fun i family ->
                                        if i > 0 then
                                            printInd ind "----"
                                        printInd ind "prod %d" family.prod
                                        family.nodes
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
            //for i in order do
            for i = 0 to nodes.Length-1 do
                if reachable.[i] then
                    let ast = nodes.[i]
                    match ast with
                    | Term t ->
                        createNode i false  ("t " + indToString (tokenToNumber tokens.[t]))
                    | NonTerm children ->
                        createNode i (children.Length > 1) ("n " + indToString leftSide.[children.[0].prod])
                        children |> Array.iter
                                (fun family ->
                                    let u = next()
                                    createNode u false ("prod " + family.prod.ToString())
                                    createEdge i u true ""
                                    for child in family.nodes do
                                        let v =
                                            if child >= 0 then child
                                            else createEpsilon child
                                        createEdge u v false ""
                                )
        else createEpsilon root |> ignore
        
        out.WriteLine("}")
        out.Close()
    