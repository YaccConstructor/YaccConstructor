module Yard.Generators.RNGLR.ReadBack.Graphs

open Yard.Generators.RNGLR.ReadBack
//open Yard.Generators.Common
open System.Collections.Generic
open Microsoft.FSharp.Collections

//Sppf is a intersection of a production automaton and Gss, which, in its turn, has edges labelled with Sppf
type Sppf = SppfVertex * int * int * Set<int>
    //(start vertex, number of nfa states, end level, accepting nfa states)

and SppfLabel =
    | Terminal of int //number in tokens array
    | Reduction of int * Sppf
    | EpsilonReduction of int
    | Epsilon //used only in SPPF, while others may be used in GSS as well
    | TemporaryReduction of ReductionTemp // only for currently processing reductions

and GssVertex  =
    val mutable firstOutEdge : GssEdge option
    val mutable otherOutEdges : GssEdge[]
    /// Number of token, processed when the vertex was created
    val Level : int
    /// Usual LALR state
    val State : int
    new (state, level) = {firstOutEdge = None; otherOutEdges = null; State = state; Level = level}

and GssEdge =
    struct
        /// AST on the edge
        val Label : SppfLabel
        /// End of the vertex (begin is not needed)
        val Dest : GssVertex
        new (d,l) = {Dest = d; Label = l}
    end

and SppfVertex =
    val nfaVertex : VertexWithBackTrack<int, int>
    val gssVertex : GssVertex

    //val outEdgesCount : int
    
    val mutable outEdges : SppfEdge list
    new (nv, gv) = {nfaVertex = nv; gssVertex = gv; (*outEdgesCount = 0;*) outEdges = []}
    member this.addEdge edge =
        this.outEdges <- edge :: this.outEdges

and SppfEdge (dest : SppfVertex, label : SppfLabel) =
        let dest = dest
        let mutable label = label
        member this.Dest = dest
        member this.Label = label
        member this.setLabel label' = 
            label <- label'

and SppfSearchDictionary<'ValueType>(numberOfNfaStates : int) =
    //three levels of indexing - nfa state, gss level, lr state
    let firstLevelArray = Array.init numberOfNfaStates (fun _ ->  new Dictionary<int, (int * 'ValueType) list ref>())

    //DEBUG
    let count = ref 0

    member this.Add (key : SppfVertex) (value : 'ValueType) =
        let nfaState = key.nfaVertex.label
        let gssLevel, lrState = key.gssVertex.Level, key.gssVertex.State
        let levelDict = firstLevelArray.[nfaState]
        let containsLrStateDict, lrStateDict = levelDict.TryGetValue(gssLevel)
        if containsLrStateDict then
            lrStateDict := (lrState, value) :: !lrStateDict
        else
            levelDict.[gssLevel] <- ref [(lrState, value)] 
        incr count               

    member this.TryGet nfaState gssLevel lrState =
        let levelDict = firstLevelArray.[nfaState]
        let containsLrStateDict, lrStateDict = levelDict.TryGetValue(gssLevel)
        if containsLrStateDict then
            match List.tryFind (fun (x, _) -> x = lrState) !lrStateDict with
            | Some (_, y) -> Some y
            | None -> None
        else
            None
    
    member this.Count = !count
 
 //for reductions that goes from level being processed
and ReductionTemp(prod : int, numberOfStates : int, endLevel : int) =
    let prod = prod
    let notHandledLeftEnds = new Queue<SppfVertex>()
    let leftEndsDict = new Dictionary<int * int, SppfVertex>()
    let acceptingNfaStates = ref Set.empty
    let visitedVertices =
    //TODO: PERFORMANCE
        new SppfSearchDictionary<SppfVertex>(numberOfStates)
            
    member this.AcceptingNfaStates = !acceptingNfaStates

    member this.AddVisited (vertex : SppfVertex) =
        //let leftEndsRef = record.leftEndsRef
        visitedVertices.Add vertex vertex
        if vertex.nfaVertex.label = 0 then
            let gssVertex = vertex.gssVertex
            leftEndsDict.[(gssVertex.Level, gssVertex.State)] <- vertex
            //leftEndsRef := [vertex]
            notHandledLeftEnds.Enqueue vertex
    
    member this.AddRightEnd rE =
        this.AddVisited rE
        acceptingNfaStates := Set.add rE.nfaVertex.label !acceptingNfaStates
    
    member this.EndLevel = endLevel

    member this.getLeftEnd level state = leftEndsDict.[(level, state)]

    member this.NotHandledLeftEnds = notHandledLeftEnds

    member this.Production = prod

    member this.TryGetAlreadyVisited' nfaNum gssLevel lrState =
        visitedVertices.TryGet nfaNum gssLevel lrState

    member this.TryGetAlreadyVisited (nfaVertex : VertexWithBackTrack<int, int>) (gssVertex : GssVertex) =
        visitedVertices.TryGet nfaVertex.label gssVertex.Level gssVertex.State

    member this.VisitedVerticesCount = visitedVertices.Count

let inline isEpsilonReduction x =
    match x with
    | EpsilonReduction _ -> true
    | _ -> false

/// Compare vertex like a pair: (level, state)
let inline vxLess (v' : GssVertex) (v : GssVertex) = v'.Level < v.Level || (v'.Level = v.Level && v'.State < v.State)
let inline vxEq (v' : GssVertex) (v : GssVertex) = v'.Level = v.Level && v'.State = v.State

let inline sppfVertexEq (v' : SppfVertex) (v : SppfVertex) =
    v'.nfaVertex = v.nfaVertex && vxEq v'.gssVertex v.gssVertex

let inline lblCoincidence s' s =
    match s', s with
    | Terminal v', Terminal v -> v' = v
    | Reduction (v',_), Reduction (v,_) -> v' = v
    | EpsilonReduction v', EpsilonReduction v -> v' = v
    | TemporaryReduction tr', TemporaryReduction tr ->
        tr'.Production = tr.Production
    //|Epsilon, Epsilon
    | _ -> false

/// Add edges, what must be unique (after shift or epsilon-edges, Terminal and EpsilonReduction edges respectively).
/// All edges are sorted by destination ascending.
let addEdge (v : GssVertex) (lbl : SppfLabel) (out : ResizeArray<GssVertex * SppfLabel>) =
    let mutable i = out.Count - 1
    while i >= 0 && vxLess (fst out.[i]) v do
        i <- i - 1
    out.Insert (i+1, (v, lbl))

/// Check if edge with specified destination and AST already exists (both simple and not)
let containsEdge (v : GssVertex) (lbl : SppfLabel) (out : ResizeArray<GssVertex * SppfLabel>) =
    let mutable i = out.Count - 1
    while i >= 0 && vxLess (fst out.[i]) v do
        i <- i - 1
    while i >= 0 && (let v',lbl' = out.[i] in vxEq v' v && not (lblCoincidence lbl lbl')) do
        i <- i - 1
    i >= 0 && (let v',lbl' = out.[i] in vxEq v' v && (lblCoincidence lbl lbl'))