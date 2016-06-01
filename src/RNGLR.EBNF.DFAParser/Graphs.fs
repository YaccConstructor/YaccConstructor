module Yard.Generators.RNGLR.ReadBack.Graphs

//open Yard.Generators.RNGLR.ReadBack
open Yard.Generators.Common
open System.Collections.Generic
open Microsoft.FSharp.Collections
//open Yard.Generators.Common.DataStructures
open Yard.Generators.RNGLR.ReadBack.Compressor
//open Yard.Generators.RNGLR.ReadBack.Tree

open Yard.Generators.Common

//Sppf is a intersection of a production automaton and Gss, which, in its turn, has edges labelled with Sppf
type Sppf = SppfVertex * int * int * (Set<int> ref)
    //(start vertex, number of nfa states, end level, accepting nfa states)

and SppfLabel =
    | Terminal of int //number in tokens array
    | Reduction of int * Sppf
    | EpsilonReduction of int
    //| Epsilon //used only in SPPF, while others may be used in GSS as well
    //| TemporaryReduction of ReductionTemp // only for currently processing reductions

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
    val dfaState : int
    val gssVertex : GssVertex

    //val outEdgesCount : int
    
    val mutable outEdges : SppfEdge list
    new (ds, gv) = {dfaState = ds; gssVertex = gv; (*outEdgesCount = 0;*) outEdges = []}
    member this.addEdge edge =
        this.outEdges <- edge :: this.outEdges

and SppfEdge (dest : SppfVertex, label : SppfLabel) =
        let dest = dest
        let mutable label = label
        member this.Dest = dest
        member this.Label = label
        member this.setLabel label' = 
            label <- label'

and SppfSearchDictionary<'ValueType>(numberOfDfaStates : int) =
    //three levels of indexing - nfa state, gss level, lr state
    let firstLevelArray = Array.init numberOfDfaStates (fun _ ->  new Dictionary<int, (int * 'ValueType) list ref>())

    //DEBUG
    let count = ref 0

    member this.Add (key : SppfVertex) (value : 'ValueType) =
        let dfaState = key.dfaState
        let gssLevel, lrState = key.gssVertex.Level, key.gssVertex.State
        let levelDict = firstLevelArray.[dfaState]
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
and ReductionTemp(prod : int, numberOfStates : int, leftEndState : int, endLevel : int) =
    let prod = prod
    let notHandledLeftEnds = new Queue<SppfVertex>()
    //let leftEndsDict = new Dictionary<int64, SppfVertex>()
    let acceptingDfaStates = ref Set.empty
    let visitedVertices =
    //TODO: PERFORMANCE
        new SppfSearchDictionary<SppfVertex>(numberOfStates)
            
    member this.AcceptingDfaStates = acceptingDfaStates

    member this.AddVisited (vertex : SppfVertex) =
        visitedVertices.Add vertex vertex

    member this.AddLeftEnd lE =
        this.AddVisited lE
        //let gssVertex = lE.gssVertex
        //let compressedVertex = pairToOne gssVertex.Level gssVertex.State
        //leftEndsDict.[compressedVertex] <- lE
        notHandledLeftEnds.Enqueue lE
    
    member this.AddRightEnd rE =
        this.AddVisited rE
        acceptingDfaStates := Set.add rE.dfaState !acceptingDfaStates
    
    member this.EndLevel = endLevel

    //member this.GetLeftEnd level state = leftEndsDict.[pairToOne level state]

    member this.NotHandledLeftEnds = notHandledLeftEnds

    member this.Production = prod

    member this.TryGetAlreadyVisited' nfaNum gssLevel lrState =
        visitedVertices.TryGet nfaNum gssLevel lrState

    member this.TryGetAlreadyVisited dfaState (gssVertex : GssVertex) =
        visitedVertices.TryGet dfaState gssVertex.Level gssVertex.State

    (*member this.TryGetLeftEnd (gssVertex : GssVertex) =
        visitedVertices.TryGet 0 gssVertex.Level gssVertex.State*)

    member this.VisitedVerticesCount = visitedVertices.Count

let inline isEpsilonReduction x =
    match x with
    | EpsilonReduction _ -> true
    | _ -> false

/// Compare vertex like a pair: (level, state)
let inline vxLess (v' : GssVertex) (v : GssVertex) = v'.Level < v.Level || (v'.Level = v.Level && v'.State < v.State)
let inline vxEq (v' : GssVertex) (v : GssVertex) = v'.Level = v.Level && v'.State = v.State

let inline sppfVertexEq (v' : SppfVertex) (v : SppfVertex) =
    v'.dfaState = v.dfaState && vxEq v'.gssVertex v.gssVertex

let inline lblCoincidence s' s =
    match s', s with
    | Terminal v', Terminal v -> v' = v
    | Reduction (v',_), Reduction (v,_) -> v' = v
    | EpsilonReduction v', EpsilonReduction v -> v' = v
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


//kinda greedy algorithm to get simple (non-ambiguous) tree from sppf
(*let sppfToTree<'TokenType> startRule (sppf : Sppf)
    (tokens : 'TokenType[])
    leftSide
    (nonTermToRule : int[]) 
    (rightParts : VertexWithBackTrack<int,int>[][]) epsilonIndex (canInferEpsilon : bool[]) 
    : Tree<'TokenType> =
    
    let epsilonReductionDict = new Dictionary<int, Ast<'TokenType>>()
    let rec buildEpsilonTail rule (nfaVertex : VertexWithBackTrack<int, int>) =
        if nfaVertex.label = rightParts.[rule].Length - 1 then
            Some []
        else
            let edges = 
                nfaVertex.outEdges 
                |> List.ofSeq 
                |> List.filter (fun (edge : Edge<int,int>) -> (edge.label = epsilonIndex || canInferEpsilon.[edge.label]) && edge.dest.label > nfaVertex.label)
                |> List.sortBy (fun (edge : Edge<int,int>) -> -edge.label)
        
            let rec f = function
            | [] -> None
            | (edge : Edge<int,int>) :: edges ->
                match buildEpsilonTail rule (edge.dest :?> VertexWithBackTrack<int,int>) with
                | Some tail -> 
                    let subTree = 
                        if edge.label = epsilonIndex then
                            Tree.Leaf None
                        else
                            let childRule = nonTermToRule.[edge.label]
                            buildEpsilonReductionTree childRule
                    Some ((new AstEdge<'TokenType>(edge.dest.label, subTree)) :: tail)
                | None -> f edges
            f edges
                                                        

    and buildEpsilonReductionTree rule =
        if epsilonReductionDict.ContainsKey rule then
            epsilonReductionDict.[rule]
        else
            let edges = (buildEpsilonTail rule rightParts.[rule].[0]).Value |> List.toArray
            let tree = Tree.Node(rule, edges)
            epsilonReductionDict.Add(rule, tree)
            tree
    
    let rec f rule sppf =
        let rec g (vertex : SppfVertex) edgeAcc =
            if vertex.outEdges.Length > 0 then
                //take that edge, that has the least nfa state and the most gss level dest
                let firstEdge, tail = vertex.outEdges.Head, vertex.outEdges.Tail
                let edge = List.fold (fun (edge : SppfEdge) (leastNfaDestEdge : SppfEdge) -> 
                            if edge.Dest.nfaVertex.label < leastNfaDestEdge.Dest.nfaVertex.label
                                    || (edge.Dest.nfaVertex.label = leastNfaDestEdge.Dest.nfaVertex.label) && edge.Dest.gssVertex.Level > leastNfaDestEdge.Dest.gssVertex.Level then 
                                edge
                            else leastNfaDestEdge) firstEdge tail
                let sppfLabel = edge.Label
                let nextVertex = edge.Dest
                let nfaDest = nextVertex.nfaVertex.label
                let subTree =
                    match sppfLabel with
                    | SppfLabel.Epsilon -> Tree.Leaf None
                    | SppfLabel.EpsilonReduction rule -> buildEpsilonReductionTree rule
                    | SppfLabel.Reduction (rule, subSppf) -> f rule subSppf
                    | SppfLabel.Terminal i ->  Tree.Leaf <| Some tokens.[i]
                    | _ -> (* TODO raise error *) Tree.Leaf None
                let astEdge = new AstEdge<'TokenType>(nfaDest, subTree)
                g nextVertex (astEdge :: edgeAcc)
            else
                let edges = edgeAcc |> List.rev
                let edgesWithTail = 
                    if vertex.nfaVertex.label <> rightParts.[rule].Length - 1 then
                        (buildEpsilonTail rule vertex.nfaVertex).Value
                        |> List.append edges
                    else
                        edges
                Array.ofList edgesWithTail
        let startVertex, _, _, _ = sppf 
        let edges = g startVertex []
        Ast.Node (rule, edges)
    new Tree<'TokenType>(f startRule sppf, leftSide)*)