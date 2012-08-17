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
type UsualOne<'T> =
    val mutable first : 'T
    val mutable other : 'T[]
    new (f,o) = {first = f; other = o}

/// Non-terminal expansion: production, family of children
/// All nodes are stored in array, so there is a correspondence between integer and node.
/// Family of children - For one nonTerminal there can be a lot of dirivation trees.
type AST =
    val mutable first : Family
    val mutable other : Family[]
    val mutable pos : int
    new (f, o) = {pos = -1; first = f; other = o}

and Family =
    struct
        val prod : int
        val nodes : obj[]
        new (p,n) = {prod = p; nodes = n}
    end

let inline getFamily (node : obj) =
    match node with
    | :? AST as ast -> ast
    | _ -> failwith "Attempt to get family of not-AST"

let inline getSingleNode (node : obj) =
    match node with
    | :? int as i  -> i
    | _ -> failwith "Attempt to get singleNode of NonTerm"

let inline private getPos (x : obj) = match x with :? AST as n -> n.pos | _ -> failwith "Attempt to get num of single node"
//let inline private setPos p (x : AST) = match x with NonTerm n -> n.pos <- p | SingleNode _ -> failwith "Attempt to get num of single node"

[<AllowNullLiteral>]
type Tree<'TokenType> (tokens : array<'TokenType>, root : obj) =
    let rootFamily, isEpsilon =
        match root with
        | :? AST as ast -> ast, false
        | :? int as x when x < 0 -> Unchecked.defaultof<_>, true
        | _ -> failwith "Strange tree - singleNode with non-negative value"
    let order =
        let stack = new System.Collections.Generic.Stack<_>()
        match root with
        | :? AST as ast -> stack.Push ast
        | _ -> ()
        let res = new ResizeArray<_>(if not <| Object.ReferenceEquals(tokens, null) then tokens.Length else 0)
        if not isEpsilon then
            while stack.Count > 0 do
                let u = stack.Pop()
                let children = u
                if children.pos = -2 then
                    children.pos <- res.Count 
                    res.Add <| u
                elif children.pos = -1 then
                    children.pos <- -2
                    stack.Push u
                    let inline handle (family : Family) = 
                        let family = family.nodes
                        for j = 0 to family.Length - 1 do
                            match family.[j] with
                            | :? AST as ast ->
                                if ast.pos = -1 then
                                    stack.Push ast
                            | _ -> ()
                    handle children.first
                    if children.other <> null then
                        for family in children.other do
                            handle family
        res.ToArray()

    member this.Order = order
    member this.Root = root

    (*member this.EliminateCycles() =
        if not isEpsilon then
            let proper = Array.create nodes.Length true
            for x in order do
                match nodes.[x] with
                | NonTerm children ->
                    let arr =
                        children.other |> Array.filter (fun family ->
                            family.nodes
                            |> Array.forall (fun j -> j < 0 || pos.[j] < pos.[x] && reachable.[j]))
                    if children.first.nodes |> Array.forall (fun j -> j < 0 || pos.[j] < pos.[x] && reachable.[j]) then
                        if arr.Length = 0 then
                            children.other <- null
                        else
                            children.other <- arr
                    else
                        if arr.Length = 0 then
                            children.other <- null
                            reachable.[x] <- false
                        else nodes.[x] <- NonTerm arr
                | _ -> ()*)
                
    member this.ChooseSingleAst () = 
        let inline smaller pos : (obj -> _)= function
            | :? int -> true
            | :? AST as n -> n.pos < pos && n.pos <> -1
            | _ -> failwith ""
        if not isEpsilon then
            for children in order do
                if children.pos <> -1 then
                    if children.first.nodes |> Array.forall (smaller children.pos) then
                        if children.other <> null then
                            children.other <- null
                    else
                        if children.other = null then
                            children.pos <- -1
                        else
                            match
                                children.other |> Array.tryFind
                                    (fun family ->
                                        family.nodes |> Array.forall (smaller children.pos))
                                with
                            | Some v ->
                                children.first <- v
                                children.other <- null
                            | None ->
                                children.pos <- -1
            for x in order do
                x.pos <- -1
            rootFamily.pos <- -2
            for i = order.Length-1 downto 0 do
                let x = order.[i]
                if x.pos <> -1 then
                    x.pos <- i
                    for j in x.first.nodes do
                        match j with
                        | :? AST as ch -> ch.pos <- -2
                        | _ -> ()

    member this.SetRanges tokenToRange =
        let ranges : ('Position * 'Position)[] = Array.zeroCreate order.Length
        // Set Positions
        let inline getRanges x =
            match x : obj with
            | :? int as t -> tokenToRange tokens.[t]
            | :? AST as ch -> ranges.[ch.pos]
            | _ -> failwith ""
        for i = 0 to order.Length - 1 do
            let x = order.[i]
            if x.pos <> -1 then
                let family = x.first.nodes
                let mutable j, k = 0, family.Length-1
                let inline isEpsilon x = match x : obj with | :? int as x when x < 0 -> true | _ -> false
                while isEpsilon family.[j] do
                    j <- j + 1
                while isEpsilon family.[k] do
                    k <- k - 1
                ranges.[i] <- fst (getRanges family.[j]), snd (getRanges family.[k])
        ranges

    member this.collectWarnings tokenToRange =
        let ranges = this.SetRanges tokenToRange
        let res = new ResizeArray<_>()
        for i = 0 to order.Length - 1 do
            let x = order.[i]
            if x.pos <> -1 then
                let children = x
                if children.other <> null then
                    res.Add (ranges.[i], Array.append [|children.first.prod|] (children.other |> Array.map (fun family -> family.prod)))
        res

    member private this.TranslateEpsilon (funs : array<_>) (leftSides : array<_>) (concat : array<_>) (range : 'Position * 'Position) : obj =
        let result = Array.zeroCreate order.Length
        for i = 0 to order.Length-1 do
            let x = order.[i]
            let children = x
            if x.pos <> -1 then
                result.[i] <- 
                    let firstRes = funs.[children.first.prod]
                                        (children.first.nodes |> Array.map (fun j -> result.[getPos j])) range
                    if children.other = null then
                        firstRes
                    else
                        children.other
                        |> Array.map (
                            fun family ->
                                funs.[family.prod] (family.nodes |> Array.map (fun j -> result.[getPos j])) range
                            )
                        |> Array.toList
                        |> (fun x -> firstRes::x)
                        |> concat.[leftSides.[children.first.prod]]
        result.[rootFamily.pos]
    
    member this.Translate (funs : array<obj[] -> 'Position * 'Position -> obj>) (leftSides : array<_>)
                            (concat : array<_>) (epsilons : array<Tree<_>>) (tokenToRange) (zeroPosition :'Position) =

        if isEpsilon then epsilons.[-(getSingleNode root)-1].TranslateEpsilon funs leftSides concat (zeroPosition, zeroPosition)
        else
            let result = Array.zeroCreate order.Length
            let ranges = this.SetRanges tokenToRange
            let tokenRanges = tokens |> Array.map tokenToRange
            let inline getRes prevRange : (obj -> _) = function
                | :? int as i when i < 0 -> epsilons.[-i-1].TranslateEpsilon funs leftSides concat (!prevRange, !prevRange)
                | :? int as t ->
                    prevRange := snd tokenRanges.[t]
                    box tokens.[t]
                | :? AST as ch ->
                    prevRange := snd ranges.[ch.pos]
                    result.[ch.pos]
                | _ -> failwith ""
            for i = 0 to order.Length - 1 do
                let x = order.[i]
                if x.pos <> -1 then
                    let children = x
                    result.[i] <-
                        let firstRes = 
                            let prevRange = fst ranges.[i] |> ref
                            funs.[children.first.prod] (children.first.nodes |> Array.map (getRes prevRange)) ranges.[i]
                        if children.other = null then
                            firstRes
                        else
                            children.other
                            |> Array.map (
                                fun family ->
                                    let prevRange = fst ranges.[i] |> ref
                                    funs.[family.prod] (family.nodes |> Array.map (getRes prevRange)) ranges.[i]
                                )
                            |> Array.toList
                            |> (fun x -> firstRes::x)
                            |> concat.[leftSides.[children.first.prod]]
            result.[rootFamily.pos]
            
    member this.PrintAst() =            
        let rec printAst ind ast =
            let printInd num (x : 'a) =
                printf "%s" (String.replicate (num * 4) " ")
                printfn x
            match (ast : obj) with
            | :? int as i when i < 0 -> printInd ind "e"
            | :? int as t -> printInd ind "t: %A" tokens.[t]
            | :? AST as fam ->
                let children = fam
                let needGroup = children.other <> null
                if needGroup then printInd ind "^^^^"
                let inline handle separate (family : Family) =
                    if separate then
                        printInd ind "----"
                    printInd ind "prod %d" family.prod
                    family.nodes
                    |> Array.iter (printAst <| ind+1)
                children.first |> handle false
                if needGroup then
                    children.other |> Array.iter (handle true)
                       
                if needGroup then printInd ind "vvvv"
            | _ -> failwith ""
        printAst 0 root

    member this.AstToDot (indToString : int -> string) tokenToNumber (leftSide : array<int>) (path : string) =
        let next =
            let cur = ref order.Length
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
        let createTerm t =
            let res = next()
            createNode res false  ("t " + indToString (tokenToNumber tokens.[t]))
            res
        if not isEpsilon then
            //for i in order do
            for i = 0 to order.Length-1 do
                let x = order.[i]
                if x.pos <> -1 then
                    let children = x
                    createNode i (children.other <> null) ("n " + indToString leftSide.[children.first.prod])
                    let inline handle (family : Family) =
                        let u = next()
                        createNode u false ("prod " + family.prod.ToString())
                        createEdge i u true ""
                        for child in family.nodes do
                            let v = 
                                match child with
                                | :? AST as v -> v.pos
                                | :? int as e when e < 0 -> createEpsilon e
                                | :? int as t -> createTerm t
                                | _ -> failwith ""
                            createEdge u v false ""
                    children.first |> handle
                    if children.other <> null then 
                        children.other |> Array.iter handle
        else createEpsilon (getSingleNode root) |> ignore
        
        out.WriteLine("}")
        out.Close()
    