module FsaHelper

open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Collections

open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Algorithms.Search

open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open Utils

type FsaState = char * Position<int>
type CharFSA = FSA<FsaState>

type MarkedVal<'a when 'a: comparison> = {
    Value: 'a
    Marked: bool }
module MarkedValFuns =
    let marked v = { Value = v; Marked = true }
    let notMarked v = { Value = v; Marked = false }

type TwoSets<'a when 'a: comparison> = {
    Marked: Set<'a>;
    NotMarked: Set<'a> }
module TwoSetsFuns =
    let empty = { Marked = Set.empty; NotMarked = Set.empty }
    let add (mv: MarkedVal<_>) (ts: TwoSets<_>) = 
        if mv.Marked
        then { ts with Marked = Set.add mv.Value ts.Marked }
        else { ts with NotMarked = Set.add mv.Value ts.NotMarked }
    let contains v (ts: TwoSets<_>) = 
        Set.contains v ts.Marked || Set.contains v ts.NotMarked

type StateFromFsa = StateFromFsa of MarkedVal<int> 
module StateFromFsaFuns = 
    let fromFsa1 (id: int) = StateFromFsa (MarkedValFuns.notMarked id)
    let fromFsa2 (id: int) = StateFromFsa (MarkedValFuns.marked id)
    let isFromFsa1 (StateFromFsa mv) = not mv.Marked
    let isFromFsa2 (StateFromFsa mv) = mv.Marked

type EqClass = EqClass of TwoSets<int>
module EqClassFuns = 
    let empty = EqClass (TwoSetsFuns.empty)
    let fsa1States (EqClass ts) = ts.NotMarked
    let fsa2States (EqClass ts) = ts.Marked
    let add (StateFromFsa mv) (EqClass ts) = 
        EqClass (TwoSetsFuns.add mv ts)
    let contains v (EqClass ts) = TwoSetsFuns.contains v ts

let alphabet = 
    let lst = 
        [
            'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n';
            'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
        ]
    HashSet(lst)

// General wrap/unwrap functions
let private getCharMetEpsMsg = "getChar met Eps symbol"
let private symbolsAreEqual (s1: FsaState) (s2: FsaState) = fst s1 = fst s2
let private getChar = function Smbl(ch, p) -> ch | Eps -> failwith getCharMetEpsMsg
let private newSymbol x =  Smbl(x, Unchecked.defaultof<_>)

let union fsa1 fsa2 = CharFSA.Union (fsa1, fsa2)

let replace origFsa matchFsa replaceFsa =
    CharFSA.Replace (origFsa, matchFsa, replaceFsa, '~', '^', getChar, newSymbol, symbolsAreEqual)

let toDot (fsa: CharFSA) path =
    fsa.PrintToDOT (path, (fun p -> sprintf "%c" (fst p)))

let toDebugDot fsa name =
    let path = Path.Combine (myDebugFolderPath, name + ".dot")
    toDot fsa path

/// Checks if the language accepted by FSA a1 is a sublanguage 
/// of the language accepted by FSA a2. 
/// Expects any fsa
let isSubFsa (a1: CharFSA) (a2: CharFSA) = 
    if a1.IsEmpty
    then true
    elif not <| a2.IsEmpty
    then
        let a2Complement = a2.Complementation (alphabet, newSymbol, getChar)
        let intersFsa = CharFSA.Intersection (a1, a2Complement, symbolsAreEqual)
        intersFsa.IsEmpty
    else false

// Widening operator implementation
let private initsOrFinalsProblemMsg = 
    "Can't define initial or final states for widened FSA, states may be computed incorrectly"
let private stateMultipleTransitionsMsg =
    "Current automaton is NFA, is must be converted to DFA before widening"
let private eqClassMultipleTransitionsMsg =
    "Widened FSA under construction is NFA"

let private dfsCollectingEdges (state: int) (getNextEdges: int -> list<EdgeFSA<FsaState>>) getNextState =
    let rec dfs state visited (edges: list<EdgeFSA<FsaState>>) =
        if not <| Set.contains state visited
        then
            let visited = Set.add state visited
            let nextEdges = getNextEdges state
            let edges = nextEdges @ edges
            nextEdges
            |> List.map getNextState
            |> List.fold (fun (v, e) succ -> dfs succ v e) (visited, edges)
        else visited, edges
    dfs state Set.empty []

let private buildFsaParts state vertices (edges: list<EdgeFSA<FsaState>>) filterStates =
    let states1 = ResizeArray.singleton(state)
    let filterSet = Set.ofSeq filterStates
    let states2 = 
        vertices
        |> Set.filter (fun s -> Set.contains s filterSet)
        |> ResizeArray.ofSeq
    let transitions = 
        edges 
        |> List.map (fun e -> e.Source, e.Tag, e.Target) 
        |> ResizeArray.ofList
    states1, states2, transitions

/// Builds the sub automaton that generates the language consisting of words 
/// accepted by the original automaton with q being the initial state
let private subFsaFrom (fsa: CharFSA) q =
    let getOutEdges st = List.ofSeq <| fsa.OutEdges(st)
    let getTarget (e: Edge<_>) = e.Target
    let vertices, edges = dfsCollectingEdges q getOutEdges getTarget
    let inits, finals, trans = buildFsaParts q vertices edges fsa.FinalState
    let fsa = 
        let fsa = CharFSA (inits, finals, trans)
        // here we add vertices because 'trans' list can be empty
        vertices |> Set.iter (ignore << fsa.AddVertex)
        fsa
    fsa

/// Builds the sub automaton that generates the language consisting of words 
/// accepted by the original automaton with q being the only final state
let private subFsaTo (fsa: CharFSA) q =
    let bidirectionalFsa = fsa.ToBidirectionalGraph()
    let getInEdges st = List.ofSeq <| bidirectionalFsa.InEdges(st)
    let getSource (e: Edge<_>) = e.Source
    let vertices, edges = dfsCollectingEdges q getInEdges getSource
    let finals, inits, trans = buildFsaParts q vertices edges fsa.InitState
    let fsa = 
        let fsa = CharFSA (inits, finals, trans)
        // here we add vertices because 'trans' list can be empty
        vertices |> Set.iter (ignore << fsa.AddVertex)
        fsa
    fsa

/// Checks if q1 from fsa1 is equivalent to q2 from fsa2
/// in the sense of relation assumed by widening operator 
let private isEquivalent q1 (fsa1: CharFSA) q2 (fsa2: CharFSA) =
    let fsaFromQ1 = subFsaFrom fsa1 q1
    let fsaFromQ2 = subFsaFrom fsa2 q2
    if isSubFsa fsaFromQ1 fsaFromQ2 && 
        isSubFsa fsaFromQ2 fsaFromQ1
    then true
    else
        let fsaToQ1 = subFsaTo fsa1 q1
        let fsaToQ2 = subFsaTo fsa2 q2
        let intersFsa = CharFSA.Intersection(fsaToQ1, fsaToQ2, symbolsAreEqual)
        not <| intersFsa.IsEmpty

let private findRelations (fsa1: CharFSA) (fsa2: CharFSA) =
    let fsa1States = fsa1.Vertices
    let fsa2States = fsa2.Vertices
    let relations =
        fsa1States
        |> Seq.map
            (
                fun st1 ->
                    fsa2States
                    |> Seq.choose 
                        (
                            fun st2 -> 
                                if isEquivalent st1 fsa1 st2 fsa2
                                then
                                    let sf1 = StateFromFsaFuns.fromFsa1 st1
                                    let sf2 = StateFromFsaFuns.fromFsa2 st2
                                    Some(Edge(sf1, sf2))
                                else None
                        )
            )
        |> Seq.concat
    let inverseRelations = relations |> Seq.map (fun e -> Edge(e.Target, e.Source))
    relations, inverseRelations

/// Builds equivalence classees using FSA.isEquivalent function
let private buildEquivalenceClasses (fsa1: CharFSA) (fsa2: CharFSA) =
    // find relations
    let relations, inverseRelations = findRelations fsa1 fsa2
    // build relations graph and find connected components
    let relationsGraph = AdjacencyGraph<StateFromFsa,Edge<StateFromFsa>>()
    do relationsGraph.AddVerticesAndEdgeRange relations |> ignore
    do relationsGraph.AddVerticesAndEdgeRange inverseRelations |> ignore
    let _, stateToEqClassIdMap = relationsGraph.StronglyConnectedComponents()
    // create alternative components representation
    stateToEqClassIdMap
    |> Seq.fold
        (
            fun acc pair -> 
                let componentNumber = pair.Value
                let state = pair.Key
                let eqClass = defaultArg (Map.tryFind componentNumber acc) EqClassFuns.empty
                Map.add componentNumber (EqClassFuns.add state eqClass) acc
        )
        Map.empty

let private symbolsToCheck (eqClass: EqClass) (fsa1: CharFSA) (fsa2: CharFSA) =
    let getOutEdges states (fsa: FSA<_>) =
        states
        |> Set.toList
        |> List.map (fun s -> fsa.OutEdges(s) |> List.ofSeq)
        |> List.concat
    let outEdges = 
        let e1 = getOutEdges (EqClassFuns.fsa1States eqClass) fsa1
        let e2 = getOutEdges (EqClassFuns.fsa2States eqClass) fsa2
        List.append e1 e2
    outEdges |> List.map (fun e -> getChar e.Tag) |> Set.ofList |> List.ofSeq

let private filterByContainsWithQuantifier quantifier elems (eqClasses: Map<int, EqClass>) =
    eqClasses
    |> Map.filter 
        (fun _ ec -> quantifier (fun e -> EqClassFuns.contains e ec) elems)

let private createTransition eqClassId (ch: char) (eqClasses: Map<int, EqClass>) fsa1 fsa2 =
    let isSink state (fsa: CharFSA) =
        let isNotFinal = ResizeArray.tryFind ((=) state) fsa.FinalState |> Option.isNone
        isNotFinal && (fsa.OutEdges(state) |> Seq.forall (fun e -> e.Target = e.Source))
    let getDstStates srcStates (fsa: CharFSA) =
        srcStates
        |> Set.toList
        |> List.map
            (fun st ->  fsa.OutEdges(st) |> Seq.filter (fun e -> getChar e.Tag = ch) |> List.ofSeq)
        |> List.choose 
            (
                function 
                    // means that transition from the state by the sym is not definded,
                    // so we have not completely defined FSA and it is assumed that
                    // transition from this state by sym leads to sink state
                    | [] -> None 
                    // transition from the state is defined and deterministic
                    // so we only need to check that it leads to not sink state
                    | x :: [] when not <| isSink x.Target fsa -> Some(x.Target) 
                    | x :: [] -> None
                    // transition is non deterministic, in current implementation
                    // it is assumed that input FSAs must be deterministic
                    | _ -> failwith stateMultipleTransitionsMsg
            )
        |> Set.ofList
    let tryCreateTransition srcStates (fsa: CharFSA) =
        let dstStates = getDstStates srcStates fsa
        if Set.isEmpty dstStates
        then None
        else
            let dstEqClasses = 
                filterByContainsWithQuantifier Set.forall dstStates eqClasses
                |> List.ofSeq
            match dstEqClasses with
            | [] -> None
            | h :: [] -> Some(h.Key)
            | _ -> failwith eqClassMultipleTransitionsMsg

    let srcEqClass = Map.find eqClassId eqClasses
    let dstEqClassId1 = tryCreateTransition (EqClassFuns.fsa1States srcEqClass) fsa1
    let dstEqClassId2 = tryCreateTransition (EqClassFuns.fsa2States srcEqClass) fsa2
    match dstEqClassId1, dstEqClassId2 with
    | Some(id1), Some(id2) when id1 = id2 -> Some(id1)
    | opt1, None -> opt1
    | None, opt2 -> opt2
    | _ -> None

let private createTransitions (eqClasses: Map<int, EqClass>) (fsa1: CharFSA) (fsa2: CharFSA) =
    let charsMap = Map.map (fun _ v -> symbolsToCheck v fsa1 fsa2) eqClasses
    eqClasses
    |> Map.toList
    |> List.map
        (   
            fun (classId, eqClass) ->
                Map.find classId charsMap
                |> List.choose 
                    (
                        fun ch -> 
                            createTransition classId ch eqClasses fsa1 fsa2 
                            |> Option.map (fun dst -> classId, newSymbol ch, dst)
                    )
        )
    |> List.concat
    |> ResizeArray.ofList
        
let widen (fsa1: CharFSA) (fsa2: CharFSA) =
    toDebugDot fsa1 "fsa1"
    toDebugDot fsa2 "fsa2"
    let dfa1 = fsa1.NfaToDfa
    let dfa2 = fsa2.NfaToDfa
    toDebugDot dfa1 "dfa1"
    toDebugDot dfa2 "dfa2"
    let eqClasses = buildEquivalenceClasses dfa1 dfa2
    let wTransitions = createTransitions eqClasses dfa1 dfa2
    let wInits = 
        let unitedInits = ResizeArray.append dfa1.InitState dfa2.InitState
        filterByContainsWithQuantifier ResizeArray.forall unitedInits eqClasses
        |> ResizeArray.ofSeq |> ResizeArray.map (fun kvp -> kvp.Key)
    let wFinals = 
        let unitedFinals = ResizeArray.append dfa1.FinalState dfa2.FinalState
        filterByContainsWithQuantifier ResizeArray.exists unitedFinals eqClasses
        |> ResizeArray.ofSeq |> ResizeArray.map (fun kvp -> kvp.Key)
    if ResizeArray.isEmpty wInits || ResizeArray.isEmpty wFinals
    then failwith initsOrFinalsProblemMsg
    CharFSA (wInits, wFinals, wTransitions)