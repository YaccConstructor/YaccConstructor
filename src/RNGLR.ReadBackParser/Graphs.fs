module Yard.Generators.RNGLR.ReadBack.Graphs

open Yard.Generators.RNGLR.ReadBack
open Yard.Generators.Common
open Yard.Generators.Common.DataStructures
open System.Collections.Generic

//Sppf is a intersection of a production automaton and Gss, which, in its turn, has edges labelled with Sppf
type Sppf = Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>

and SppfLabel =
    | Terminal of int
    | Reduction of int * Sppf
    | EpsilonReduction of int
    | Epsilon //used only in SPPF, while others may be used in GSS as well

and GssVertex  =
    val mutable OutEdges : UsualOne<GssEdge>
    /// Number of token, processed when the vertex was created
    val Level : int
    /// Usual LALR state
    val State : int
    new (state, level) = {OutEdges = Unchecked.defaultof<_>; State = state; Level = level}

and GssEdge =
    struct
        /// AST on the edge
        val Label : SppfLabel
        /// End of the vertex (begin is not needed)
        val Dest : GssVertex
        new (d,l) = {Dest = d; Label = l}
    end

//for reductions that goes from level being processed
type ReductionTemp(prod : int, numberOfStates : int,
                    rightEnd : Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>) =
    let prod = prod
    let leftEnds = new ResizeArray<Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>>()
    let rightEnds =
        let x = new ResizeArray<Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>>()
        x.Add rightEnd
        x
    let visitedVertices =
    //TODO: PERFORMANCE
        let x =  Array.init numberOfStates (fun i -> new ResizeArray<Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>>())
        x.[(fst rightEnd.label).label].Add(rightEnd)
        x
    
    member this.AddVisited (vertex : Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>) =
        let i = (fst vertex.label).label
        visitedVertices.[i].Add (vertex)
        if i = 0 then
            leftEnds.Add vertex
    
    member this.AddRightEnd rE =
        this.AddVisited(rE)
        rightEnds.Add rE

    member this.TryGetAlreadyVisited (nfaVertex : VertexWithBackTrack<int, int>) (gssVertex : GssVertex) =
        visitedVertices.[nfaVertex.label] |> Seq.tryFind 
                (fun x -> 
                    (snd x.label).Level = gssVertex.Level && (snd x.label).State = gssVertex.State)

let inline isEpsilonReduction x =
    match x with
    | EpsilonReduction _ -> true
    | _ -> false

/// Compare vertex like a pair: (level, state)
let inline vxLess (v' : GssVertex) (v : GssVertex) = v'.Level < v.Level || (v'.Level = v.Level && v'.State < v.State)
let inline vxEq (v' : GssVertex) (v : GssVertex) = v'.Level = v.Level && v'.State = v.State

let inline lblCoincidence s' s =
    match s', s with
    | Terminal v', Terminal v -> v' = v
    | Reduction (v',_), Reduction (v,_) -> v' = v
    | EpsilonReduction v', EpsilonReduction v -> v' = v
    //|Epsilon, Epsilon
    | _ -> false

/// Add edges, what must be unique (after shift or epsilon-edges, Terminal and EpsilonReduction edges respectively).
/// All edges are sorted by destination ascending.
let addSimpleEdge (v : GssVertex) (lbl : SppfLabel) (out : ResizeArray<GssVertex * SppfLabel>) =
    let mutable i = out.Count - 1
    while i >= 0 && vxLess (fst out.[i]) v do
        i <- i - 1
    out.Insert (i+1, (v, lbl))

/// Check if edge with specified destination and AST already exists (both simple and not)
let containsSimpleEdge (v : GssVertex) (lbl : SppfLabel) (out : ResizeArray<GssVertex * SppfLabel>) =
    let mutable i = out.Count - 1
    while i >= 0 && vxLess (fst out.[i]) v do
        i <- i - 1
    while i >= 0 && (let v',lbl' = out.[i] in vxEq v' v && not (lblCoincidence lbl lbl')) do
        i <- i - 1
    i >= 0 && (let v',lbl' = out.[i] in vxEq v' v && (lblCoincidence lbl lbl'))

(*/// Add or extend edge with specified destination and family.
/// All edges are sorted by destination ascending.
let addEdge (v : GssVertex) (family : ReductionTemp) (out : ResizeArray<GssVertex * ReductionTemp>) isError =
    let mutable i = out.Count - 1
    while i >= 0 && vxLess (fst out.[i]) v do
        i <- i - 1

    let isCreated = not (i >= 0 && vxEq (fst out.[i]) v)

    let ast = 
        if isError
        then new AST(family, null)
        else 
            if not isCreated 
            then let _,_,n = out.[i] in n
            else new AST (Unchecked.defaultof<_>, null)
    let newVal = v, family, ast
    if isCreated || family.prod = (snd3 out.[i]).prod then
        out.Insert (i+1, newVal)
    elif family.prod < (snd3 out.[i]).prod then
        out.[i] <- newVal
        let mutable j = i-1
        while j >= 0 && eq (fst3 out.[j])  (fst3 out.[i]) do
            j <- j-1

        out.RemoveRange(j+1, i-j-1)
    isCreated, ast*)