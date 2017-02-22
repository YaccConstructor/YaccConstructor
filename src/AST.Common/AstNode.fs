module Yard.Generators.Common.AstNode

[<AllowNullLiteral>]
type AstNode =
    interface
    end

type Terminal (tokenNumber: int)  =
    interface AstNode
    member this.TokenNumber = tokenNumber 
    
type Epsilon (epsilonNonTerm : int) =
    interface AstNode
    member this.EpsilonNonTerm = epsilonNonTerm

/// Non-terminal expansion: production, family of children
/// All nodes are stored in array, so there is a correspondence between integer and node.
/// Family of children - For one nonTerminal there can be a lot of derivation trees.
[<AllowNullLiteral>]
type AST =
    interface AstNode
    val mutable first : Family
    val mutable other : Family[]
    val mutable pos : int
    new (f, o) = {pos = -1; first = f; other = o}
    new (f, o, p) = {first = f; other = o; pos = p}
    member inline this.findFamily f =
        if f this.first 
        then Some this.first
        elif this.other <> null 
        then Array.tryFind f this.other
        else None

    member this.filterFamilies f =
        let res = new ResizeArray<Family>()
        if f this.first
        then res.Add this.first
        if this.other <> null 
        then res.AddRange(Array.filter f this.other)
        res

    member inline this.doForAllFamilies f =
        f this.first
        if this.other <> null
        then Array.iter f this.other

    member inline this.map f =
        let length = let count = ref 0; 
                     this.doForAllFamilies (fun _ -> count := !count + 1)
                     !count
        let res = Array.zeroCreate length
        res.[0] <- f this.first
        if this.other <> null
        then
            for i = 0 to this.other.Length-1 do
                res.[i+1] <- f this.other.[i]
        res

    new (fs : array<_>) = 
        let other = 
            if fs.Length > 1 
            then Array.init (fs.Length - 1) (fun i -> fs.[i+1]) 
            else null
        {pos = -1; first = fs.[0]; other = other}

and Family =
    struct
        val prod : int
        val nodes : Nodes
        new (p,n) = {prod = p; nodes = n}
    end

and Nodes =
    struct
        val mutable fst : AstNode
        val mutable snd : AstNode
        val mutable other : AstNode[]
        new (f,s,o) = {fst = f; snd = s; other = o}

        new (arr : array<_>) =
            let mutable res = new Nodes()
            if arr <> null 
            then
                if arr.Length > 0
                then
                    res.fst <- arr.[0]
                    if arr.Length > 1
                    then
                        res.snd <- arr.[1]
                        if arr.Length > 2
                        then
                            res.other <- arr.[2..]
            {fst = res.fst; snd = res.snd; other = res.other}
            //match arr with

        member nodes.doForAll f =
            if nodes.fst <> null
            then
                f nodes.fst
                if nodes.snd <> null
                then
                    f nodes.snd
                    if nodes.other <> null
                    then
                        for x in nodes.other do
                            f x

        member nodes.doForAllRev f =
            if nodes.fst <> null
            then
                if nodes.snd <> null
                then
                    if nodes.other <> null 
                    then
                        for i = nodes.other.Length - 1 downto 0 do
                            f nodes.other.[i]
                    f nodes.snd
                f nodes.fst

        member nodes.isForAll f =
            if nodes.fst <> null
            then
                if not <| f nodes.fst 
                then false
                elif nodes.snd <> null 
                then
                    if not <| f nodes.snd 
                    then false
                    elif nodes.other <> null 
                    then nodes.other |> Array.forall f
                    else true
                else true
            else true

        member inline nodes.Length = 
            if nodes.fst <> null
            then
                if nodes.snd <> null
                then
                    if nodes.other <> null
                    then 2 + nodes.other.Length
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

let inline getFamily (node : AstNode) =
    match node with
    | :? AST as ast -> ast
    | _ -> failwith "Attempt to get family of not-AST"

let inline getSingleNode (node : AstNode) =
    match node with
    | :? Epsilon as e  -> e.EpsilonNonTerm
    | :? Terminal as t  -> t.TokenNumber
    | _ -> failwith "Attempt to get singleNode of NonTerm"

let inline isEpsilon (x : AstNode) =
    match x with | :? Epsilon -> true | _ -> false
