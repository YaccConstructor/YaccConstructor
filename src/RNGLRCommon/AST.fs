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

module Yard.Generators.RNGLR.AST
open System
open System.Collections.Generic
open Yard.Generators.RNGLR.DataStructures
open Yard.Generators.RNGLR.AstNode

/// Arguments for tanslation calling, seen by user
type TranslateArguments<'Token, 'Position> = {
    tokenToRange : 'Token -> 'Position * 'Position
    zeroPosition : 'Position
    clearAST : bool
    filterEpsilons : bool
}

let inline private getPos (ast : AstNode) =
    match ast with
    | :? AST as n -> n.pos
    | _ -> failwith "Attempt to get num of single node"
//let inline private setPos p (x : AST) = match x with NonTerm n -> n.pos <- p | SingleNode _ -> failwith "Attempt to get num of single node"

let private emptyArr = [||]
type private DotNodeType = Prod | AstNodeType

type ErrorNode<'TokenType> = 
    val errorOn : 'TokenType     // token on which error occurs
    val production : int  // now it doesn't work. Production number where error occured
    val expected : array<string>  // parser was expecting one of the these tokens

    val mutable tokens : array<'TokenType>  //skipped tokens during error recovery
    val recTokens : array<string> // parser was look for one of these tokens during recovery

    new (errOn, prod, exp, (*skip,*) recToks) = 
        {
            errorOn = errOn; 
            production = prod; 
            expected = exp; 
            tokens = Unchecked.defaultof<_>;(*skip;*)
            recTokens = recToks
        }

type ErrorDictionary<'TokenType> = Dictionary<Family, ErrorNode<'TokenType>>

[<AllowNullLiteral>]
type Tree<'TokenType> (tokens : array<'TokenType>, root : AstNode, rules : int[][]) =
    let rootFamily, isEpsilonTree =
        match root with
        | :? AST as ast -> ast, false
        | :? Epsilon as e -> Unchecked.defaultof<_>, true
        | _ -> failwith "Strange tree - consists of terminal"
    let order =
        let stack = new System.Collections.Generic.Stack<_>()
        match root with
        | :? AST as ast -> stack.Push ast
        | _ -> ()
        let res = new BlockResizeArray<_>()
        if not isEpsilonTree then
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
                        let inline handleAst (ast : AstNode) =
                            match ast with
                            | :? AST as ast ->
                                if ast.pos = -1 then
                                    stack.Push ast
                            | _ -> ()
                        family.nodes.doForAllRev handleAst
                    handle children.first
                    if children.other <> null then
                        for family in children.other do
                            handle family
        res.ToArray()

    member this.Order = order
    member this.Root = root
    member this.RulesCount = rules.GetLength(0)

    static member inline private smaller pos : (AstNode -> _) = function
        | :? Epsilon | :? Terminal -> true
        | :? AST as n -> n.pos < pos && n.pos <> -1
        | _ -> failwith ""

    member private this.FilterChildren childrenHandler = 
        if not isEpsilonTree then
            for children in order do
                if children.pos <> -1 then
                    childrenHandler children
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

    member this.EliminateCycles() =
        let handleChildren (children : AST) =
            let inline isCorrectFamily (family : Family) =
                family.nodes.isForAll (Tree<_>.smaller children.pos)
            let arr =
                if children.other <> null then
                    children.other |> Array.filter isCorrectFamily
                else emptyArr
            if isCorrectFamily children.first then
                if arr.Length = 0 then
                    children.other <- null
                else
                    children.other <- arr
            elif arr.Length = 0 then
                children.other <- null
                children.pos <- -1
            elif arr.Length = 1 then
                children.other <- null
                children.first <- arr.[0]
            else
                children.first <- arr.[0]
                children.other <- arr.[1..arr.Length-1]
                    
        this.FilterChildren handleChildren
                
    /// Choose first correct subtree without cycles.
    member this.ChooseSingleAst () =
        let handleChildren (children : AST) =
            if children.first.nodes.isForAll (Tree<_>.smaller children.pos) then
                if children.other <> null then
                    children.other <- null
            elif children.other = null then
                children.pos <- -1
            else
                match
                    children.other |> Array.tryFind
                        (fun family -> family.nodes.isForAll (Tree<_>.smaller children.pos))
                    with
                | Some v ->
                    children.first <- v
                | None ->
                    children.pos <- -1
                children.other <- null
        this.FilterChildren handleChildren

    /// Select children, where the first subnode ends first.
    /// In case of ambiguity look at second one.
    /// If ranges are equal, then select one, having the smallest rule number.
    /// Eliminate cycles.
    member this.ChooseLongestMatch () =
        if not isEpsilonTree then
            let rangeEnds = Array.zeroCreate order.Length
            let inline getEnd (x : AstNode) =
                match x with
                | :? Terminal as t -> t.TokenNumber
                | :? AST as ch -> rangeEnds.[ch.pos]
                | _ -> failwith ""
            let getRangeEnd (family : Nodes) =
                let mutable k = family.Length-1
                while isEpsilon family.[k] do
                    k <- k - 1
                getEnd family.[k]

            /// Compare arrays of nodes using longest match.
            /// The longest is where the first element with different position is longest
            ///    or where the total length of array is larger.
            let compareNodesArrs (a1 : _[]) (a2 : _[]) =
                let lim = min a1.Length a2.Length
                let rec inner i = 
                    if i = lim then a1.Length - a2.Length
                    else
                        match getEnd a1.[i] - getEnd a2.[i] with
                        | 0 -> inner (i + 1)
                        | x -> x
                inner 0
            let selectChild (f1 : Family) (f2 : Family) =
                let inline getEnd (x : AstNode) =
                    match x with
                    | null -> -1
                    | :? Epsilon -> -1
                    | :? Terminal as t -> t.TokenNumber
                    | :? AST as ch -> rangeEnds.[ch.pos]
                    | _ -> failwith ""
                let inline compareNodes (n1 : Nodes) (n2 : Nodes) =
                    match getEnd n1.fst - getEnd n2.fst with
                    | 0 -> 
                        match getEnd n1.snd - getEnd n2.snd with
                        | 0 -> 
                            match n1.other, n2.other with
                            | null, null -> 0
                            | null, _ -> -1
                            | _, null -> 1
                            | arr1, arr2 -> compareNodesArrs arr1 arr2
                        | x -> x
                    | x -> x
                match compareNodes f1.nodes f2.nodes with
                    | 0 -> if f1.prod < f2.prod then f1 else f2
                    | x when x > 0 -> f1
                    | _ -> f2
            let handleChildren (children : AST) =
                if children.other = null then
                    if children.first.nodes.isForAll (Tree<_>.smaller children.pos) then
                        rangeEnds.[children.pos] <- getRangeEnd children.first.nodes
                    else children.pos <- -1
                else
                    match
                        children.other |> Array.fold
                            (fun res family ->
                                if family.nodes.isForAll (Tree<_>.smaller children.pos) then
                                    match res with
                                    | None -> Some family
                                    | Some cur -> Some <| selectChild cur family
                                else res)
                            (if children.first.nodes.isForAll (Tree<_>.smaller children.pos) then
                                 Some children.first
                             else None)
                        with
                    | Some v ->
                        children.first <- v
                    | None ->
                        children.pos <- -1
                children.other <- null

            this.FilterChildren handleChildren

    /// handleCycleNode is used for handling nodes, contained in cycles
    ///   and having no children family, where each node has smaller position.
    member this.TraverseWithRanges tokenToRange dispose handleCycleNode f =
        let ranges = new BlockResizeArray<'Position * 'Position>()
        let processed = Array.zeroCreate order.Length
        let count = Array.zeroCreate <| (order.Length >>> ranges.Shift) + 1
        for i = 0 to order.Length-1 do
            let x = order.[i]
            if x.pos <> -1 then
                let inline incr (x : AstNode) =
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

        let inline getRanges (x : AstNode) =
            match x with
            | :? Terminal as t -> tokenToRange tokens.[t.TokenNumber]
            | :? AST as ch -> ranges.[ch.pos]
            | _ -> failwith ""
        for i = 0 to order.Length - 1 do
            let x = order.[i]
            if x.pos = -1 then
                f i ranges
                ranges.Add Unchecked.defaultof<_>
            else
                let inline goodNodes (family : Family) =
                    family.nodes.isForAll (function
                        | :? AST as ast -> processed.[ast.pos]
                        | _ -> true)
                match x.findFamily goodNodes with
                | None ->
                    ranges.Add Unchecked.defaultof<_>
                    handleCycleNode x
                | Some family -> 
                    processed.[x.pos] <- true
                    let nodes = family.nodes
                    let mutable j, k = 0, nodes.Length-1
                    (*let left = 
                        if isEpsilon family.fst then
                            if isEpsilon family.snd then
                                family.[2]
                            else family.snd
                        else family.fst*)
                    while isEpsilon nodes.[j] do
                        j <- j + 1
                    while k > 0 && isEpsilon nodes.[k] do
                        k <- k - 1
                    if j <= k 
                    then
                        let leftRange = getRanges nodes.[j]
                        let rightRange = getRanges nodes.[k]
                        ranges.Add (fst leftRange, snd rightRange)
                    else //may occurs because of error recovery
                        let rang = ranges.[ranges.Count-1]
                        ranges.Add (snd rang, snd rang)
                    f i ranges
                    let inline clear (x : AstNode) =
                        match x with
                        | :? AST as ast ->
                            let num = ast.pos >>> ranges.Shift
                            count.[num] <- count.[num] - 1
                            if count.[num] = 0 then
                                dispose num
                                ranges.DeleteBlock num
                        | _ -> ()
                    nodes.doForAll clear
                    if x.other <> null then
                        for e in x.other do
                            e.nodes.doForAll clear
        //printfn "Memory: %d" ((GC.GetTotalMemory true) >>> 20)

    member this.collectWarnings tokenToRange =
        let res = new ResizeArray<_>()
        if not isEpsilonTree then
            this.TraverseWithRanges tokenToRange ignore ignore <| fun i ranges ->
                let children = order.[i]
                if children.other <> null then
                    res.Add (ranges.[i], Array.append [|children.first.prod|] (children.other |> Array.map (fun family -> family.prod)))
        res

    member private this.getTokensFromFamily (family : Family) =
        let rec func (fam : Family) = 
            let mutable res = []
            for j = 0 to fam.nodes.Length-1 do
                match fam.nodes.[j] with
                | :? Terminal as t -> 
                    res <- tokens.[t.TokenNumber] :: res
                | :? AST as ast -> 
                    res <- func ast.first @ res
                    if ast.other <> null 
                    then
                        for other in ast.other do
                            res <- func other @ res
                | _ -> ()
            res
        List.rev <| func family

    member this.collectErrors tokenToRange = 
        let res = new ResizeArray<'Position * 'Position * array<'TokenType>>()
        if not isEpsilonTree 
        then
            this.TraverseWithRanges tokenToRange ignore ignore <| fun i ranges ->
                let children = order.[i]

                if children.first.prod = this.RulesCount //i.e. error production
                then 
                    let result = this.getTokensFromFamily children.first
                    res.Add(fst ranges.[i], snd ranges.[i], List.toArray result)

                if children.other <> null
                then
                    for other in children.other do
                        if other.prod = this.RulesCount //i.e. error production
                        then
                            let result = this.getTokensFromFamily other
                            res.Add(fst ranges.[i], snd ranges.[i], List.toArray result)
        res
        
    member private this.TranslateEpsilon (funs : array<_>) (leftSides : array<_>) (concat : array<_>) (range : 'Position * 'Position) : obj =
        let result = Array.zeroCreate order.Length
        for i = 0 to order.Length-1 do
            let x = order.[i]
            let children = x
            if x.pos <> -1 
            then
                result.[i] <- 
                    let arr = children.first.nodes.map (fun j -> result.[getPos j])
                    let firstRes = funs.[children.first.prod] arr range
                    if children.other = null 
                    then
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
                            (concat : array<_>) (epsilons : array<Tree<_>>) (tokenToRange) (zeroPosition :'Position) 
                            clearAST (errDict : ErrorDictionary<'TokenType>) =

        if isEpsilonTree 
        then epsilons.[(root :?> Epsilon).EpsilonNonTerm].TranslateEpsilon funs leftSides concat (zeroPosition, zeroPosition)
        else
            let result = new BlockResizeArray<_>()
            let inline dispose i =
                result.DeleteBlock i
                if clearAST then
                    for k = i <<< result.Shift to ((i+1) <<< result.Shift) - 1 do
                        order.[k].first <- Unchecked.defaultof<_>
                        order.[k].other <- null
                        order.[k] <- null
            let reportCycleError (x : AST) =
                printfn "Rule %d" x.first.prod
                printf "Subnodes positions: "
                x.first.nodes.map (function
                    | :? AST as ast -> sprintf "(%d)" ast.pos
                    | x -> sprintf "%A" x
                ) |> Array.iter (printf "%s ")
                failwith "Please, delete cycles from tree you are going to translate"

            this.TraverseWithRanges tokenToRange dispose reportCycleError <| fun i ranges ->
                let inline getRes prevRange : (AstNode -> _) = function
                    | :? Epsilon as e -> 
                        epsilons.[e.EpsilonNonTerm].TranslateEpsilon funs leftSides concat (!prevRange, !prevRange)
                    | :? Terminal as t ->
                        prevRange := snd <| tokenToRange tokens.[t.TokenNumber]
                        box tokens.[t.TokenNumber]
                    | :? AST as ch ->
                        prevRange := snd ranges.[ch.pos]    
                        result.[ch.pos]
                    | _ -> failwith ""

                let inline getFamilyWithError (fam : Family) = 
                    let res = ref None
                    fam.nodes.doForAll <| fun x ->
                        if x :? AST 
                        then 
                            let ast = (unbox x) : AST
                            if res.Value.IsNone
                            then res := ast.findFamily(fun x -> x.prod = this.RulesCount)
                    !res

                let x = order.[i]
                if x.pos = -1 then result.Add Unchecked.defaultof<_>
                else
                    let children = x
                    result.Add <|
                        let  translateFamily (fam : Family) =
                            let prevRange = fst ranges.[i] |> ref
                            let length = 
                                if fam.prod < this.RulesCount
                                then rules.[fam.prod].Length
                                else fam.nodes.Length
                            
                            let res = Array.zeroCreate length
                            let k = ref 0
                            fam.nodes.doForAll <| fun x ->
                                res.[!k] <- getRes prevRange x
                                incr k
                            
                            for i = !k to length-1 do
                                res.[i] <- epsilons.[rules.[fam.prod].[i]].TranslateEpsilon funs leftSides concat (!prevRange, !prevRange)
                            
                            let errFamily = getFamilyWithError fam
                            if errFamily.IsSome
                            then 
                                let info = errDict.[errFamily.Value]
                                let arr = List.toArray <| this.getTokensFromFamily errFamily.Value
                                info.tokens <- arr
                                let errNodes = Array.zeroCreate 1
                                let list = info :: []
                                errNodes.[0] <- box list
                                funs.[fam.prod] errNodes ranges.[i]
                            else
                                funs.[fam.prod] res ranges.[i]
                            //funs.[fam.prod] res ranges.[i]
                            
                        let firstRes = translateFamily children.first
                        if children.other = null 
                        then firstRes
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
            match (ast : AstNode) with
            | :? Epsilon -> printInd ind "e"
            | :? Terminal as t -> printInd ind "t: %A" tokens.[t.TokenNumber]
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
        let createNode num isAmbiguous nodeType (str : string) =
            let label =
                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
                if not isAmbiguous then cur
                else cur + " !"
            let shape =
                match nodeType with
                | AstNodeType -> ",shape=box"
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
        let createEpsilon ind = 
            let res = next()
            createNode res false AstNodeType ("n " + indToString (-1-ind))
            let u = next()
            createNode u false AstNodeType "eps"
            createEdge res u true ""
            res
        let createTerm t =
            let res = next()
            createNode res false AstNodeType ("t " + indToString (tokenToNumber tokens.[t]))
            res
        if not isEpsilonTree then
            //for i in order do
            for i = order.Length-1 downto 0 do
                let x = order.[i]
                if x.pos <> -1 then
                    let children = x
                    
                    let label = 
                        if children.first.prod < leftSide.Length then indToString leftSide.[children.first.prod]
                        else "error"
                     
                    createNode i (children.other <> null) AstNodeType ("n " + label)
                     
                    let inline handle (family : Family) =
                        let u = next()
                        createNode u false Prod ("prod " + family.prod.ToString())
                        createEdge i u true ""
                        family.nodes.doForAll <| fun child ->
                            let v = 
                                match child with
                                | :? AST as v -> v.pos
                                | :? Epsilon as e -> createEpsilon e.EpsilonNonTerm
                                | :? Terminal as t -> createTerm t.TokenNumber
                                | _ -> failwith ""
                            createEdge u v false ""
                    children.first |> handle
                    if children.other <> null then 
                        children.other |> Array.iter handle
        else createEpsilon (root :?> Epsilon).EpsilonNonTerm |> ignore
        
        out.WriteLine("}")
        out.Close()