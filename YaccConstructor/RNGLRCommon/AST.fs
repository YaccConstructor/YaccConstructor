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
open Yard.Generators.RNGLR.DataStructures


[<Struct>]
type UsualOne<'T> =
    val mutable first : 'T
    val mutable other : 'T[]
    new (f,o) = {first = f; other = o}

/// Non-terminal expansion: production, family of children
/// All nodes are stored in array, so there is a correspondence between integer and node.
/// Family of children - For one nonTerminal there can be a lot of dirivation trees.
[<AllowNullLiteral>]
type AST =
    val mutable first : Family
    val mutable other : Family[]
    val mutable pos : int
    new (f, o) = {pos = -1; first = f; other = o}

and Family =
    struct
        val prod : int
        val nodes : Nodes
        new (p,n) = {prod = p; nodes = n}
    end

and Nodes =
    struct
        val mutable fst : obj
        val mutable snd : obj
        val mutable other : obj[]
        new (f,s,o) = {fst = f; snd = s; other = o}

        new (arr : array<_>) =
            let mutable res = new Nodes()
            if arr <> null then
                if arr.Length > 0 then
                    res.fst <- arr.[0]
                    if arr.Length > 1 then
                        res.snd <- arr.[1]
                        if arr.Length > 2 then
                            res.other <- arr.[2..]
            {fst = res.fst; snd = res.snd; other = res.other}
            //match arr with


        member inline nodes.doForAll f =
            if nodes.fst <> null then
                f nodes.fst
                if nodes.snd <> null then
                    f nodes.snd
                    if nodes.other <> null then
                        for x in nodes.other do
                            f x

        member inline nodes.isForAll f =
            if nodes.fst <> null then
                if not <| f nodes.fst then false
                elif nodes.snd <> null then
                        if not <| f nodes.snd then false
                        elif nodes.other <> null then
                            nodes.other |> Array.forall f
                        else true
                else true
            else true

        member inline nodes.Length = 
            if nodes.fst <> null then
                if nodes.snd <> null then
                    if nodes.other <> null then
                        2 + nodes.other.Length
                    else 2
                else 1
            else 0

        member inline nodes.Item
            with get i =
                match i with
                | 0 -> nodes.fst
                | 1 -> nodes.snd
                | i -> nodes.other.[i-2]

        member inline nodes.map f =
            let length = nodes.Length
            let res = Array.zeroCreate length
            if nodes.fst <> null then
                res.[0] <- f nodes.fst
                if nodes.snd <> null then
                    res.[1] <- f nodes.snd
                    if nodes.other <> null then
                        for i = 0 to nodes.other.Length-1 do
                            res.[i+2] <- f nodes.other.[i]
            res
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
type Tree<'TokenType> (tokens : array<'TokenType>, root : obj, rules : int[][]) =
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
        let res = new BlockResizeArray<_>()
        if not isEpsilon then
            while stack.Count > 0 do
                let u = stack.Pop()
                let children = u
                if children.pos = -2 then
                    children.pos <- res.Count 
                    res.Add u
                elif children.pos = -1 then
                    children.pos <- -2
                    stack.Push u
                    let inline handle (family : Family) = 
                        let inline handleAst (ast : obj) =
                            match ast with
                            | :? AST as ast ->
                                if ast.pos = -1 then
                                    stack.Push ast
                            | _ -> ()
                        family.nodes.doForAll handleAst
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
                    if children.first.nodes.isForAll (smaller children.pos) then
                        if children.other <> null then
                            children.other <- null
                    else
                        if children.other = null then
                            children.pos <- -1
                        else
                            match
                                children.other |> Array.tryFind
                                    (fun family ->
                                        family.nodes.isForAll (smaller children.pos))
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
                    x.first.nodes.doForAll <| function
                        | :? AST as ch -> ch.pos <- -2
                        | _ -> ()

    member this.TraverseWithRanges tokenToRange dispose f =
        let ranges = new BlockResizeArray<'Position * 'Position>()
        let count = Array.zeroCreate <| (order.Length >>> ranges.Shift) + 1
        for i = 0 to order.Length-1 do
            let x = order.[i]
            if x.pos <> -1 then
                let inline incr (x : obj) =
                    match x with
                    | :? AST as ast ->
                        let num = ast.pos >>> ranges.Shift
                        count.[num] <- count.[num] + 1
                    | _ -> ()
                x.first.nodes.doForAll incr
                if x.other <> null then
                    for e in x.other do
                        e.nodes.doForAll incr
        count.[rootFamily.pos >>> ranges.Shift] <- count.[rootFamily.pos >>> ranges.Shift] + 1

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
                (*let left = 
                    if isEpsilon family.fst then
                        if isEpsilon family.snd then
                            family.[2]
                        else family.snd
                    else family.fst*)
                while isEpsilon family.[j] do
                    j <- j + 1
                while isEpsilon family.[k] do
                    k <- k - 1
                ranges.Add (fst (getRanges family.[j]), snd (getRanges family.[k]))
                f i ranges
                let inline clear (x : obj) =
                    match x with
                    | :? AST as ast ->
                        let num = ast.pos >>> ranges.Shift
                        count.[num] <- count.[num] - 1
                        if count.[num] = 0 then
                            dispose num
                            ranges.DeleteBlock num
                    | _ -> ()
                family.doForAll clear
                if x.other <> null then
                    for e in x.other do
                        e.nodes.doForAll clear
        //printfn "Memory: %d" ((GC.GetTotalMemory true) >>> 20)

    member this.collectWarnings tokenToRange =
        let res = new ResizeArray<_>()
        this.TraverseWithRanges tokenToRange ignore <| fun i ranges ->
            let children = order.[i]
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
                                        (children.first.nodes.map (fun j -> result.[getPos j])) range
                    if children.other = null then
                        firstRes
                    else
                        children.other
                        |> Array.map (
                            fun family ->
                                funs.[family.prod] (family.nodes.map (fun j -> result.[getPos j])) range
                            )
                        |> Array.toList
                        |> (fun x -> firstRes::x)
                        |> concat.[leftSides.[children.first.prod]]
        result.[rootFamily.pos]
    
    member this.Translate (funs : array<obj[] -> 'Position * 'Position -> obj>) (leftSides : array<_>)
                            (concat : array<_>) (epsilons : array<Tree<_>>) (tokenToRange) (zeroPosition :'Position) clearAST =

        if isEpsilon then epsilons.[-(getSingleNode root)-1].TranslateEpsilon funs leftSides concat (zeroPosition, zeroPosition)
        else
            let result = new BlockResizeArray<_>()
            let inline dispose i =
                result.DeleteBlock i
                if clearAST then
                    for k = i <<< result.Shift to ((i+1) <<< result.Shift) - 1 do
                        order.[k].first <- Unchecked.defaultof<_>
                        order.[k].other <- null
                        order.[k] <- null
            this.TraverseWithRanges tokenToRange dispose <| fun i ranges ->
                let inline getRes prevRange : (obj -> _) = function
                    | :? int as i when i < 0 -> epsilons.[-i-1].TranslateEpsilon funs leftSides concat (!prevRange, !prevRange)
                    | :? int as t ->
                        prevRange := snd <| tokenToRange tokens.[t]
                        box tokens.[t]
                    | :? AST as ch ->
                        prevRange := snd ranges.[ch.pos]
                        result.[ch.pos]
                    | _ -> failwith ""
                let x = order.[i]
                if x.pos <> -1 then
                    let children = x
                    result.Add <|
                        let inline translateFamily (fam : Family) =
                            let prevRange = fst ranges.[i] |> ref
                            let res = Array.zeroCreate rules.[fam.prod].Length
                            let k = ref 0
                            fam.nodes.doForAll <| fun x ->
                                res.[!k] <- getRes prevRange x
                                incr k
                            for i = !k to rules.[fam.prod].Length-1 do
                                res.[i] <- epsilons.[rules.[fam.prod].[i]].TranslateEpsilon funs leftSides concat (!prevRange, !prevRange)
                            funs.[fam.prod] res ranges.[i]
                        let firstRes = translateFamily children.first
                        if children.other = null then
                            firstRes
                        else
                            children.other
                            |> Array.map translateFamily
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
                    family.nodes.doForAll (printAst <| ind+1)
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
                        family.nodes.doForAll <| fun child ->
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
    