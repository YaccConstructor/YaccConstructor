module FsaHelper

open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Collections

open QuickGraph
open QuickGraph.Algorithms

open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open Utils

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

type FsaState = char * Position<int>
type CharFSA = FSA<FsaState>
type FullFSA = FullFSA of CharFSA

module FullFsaFuns = 
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

    let private createFromNotFull (notFull: CharFSA) = 
        let full = CharFSA.DfaToFullDfa (notFull, alphabet, newSymbol, getChar)
        FullFSA(full)

    let create initial final transitions =
        createFromNotFull (CharFSA.Create (initial, final, transitions))

    let union (FullFSA(fsa1)) (FullFSA(fsa2)) = 
        createFromNotFull(FSA.Union (fsa1, fsa2))

    let replace (FullFSA(origFsa)) (FullFSA(matchFsa)) (FullFSA(replaceFsa)) =
        let notFull = 
            FSA.Replace (origFsa, matchFsa, replaceFsa, '~', '^', getChar, newSymbol, symbolsAreEqual)
        createFromNotFull notFull

    let toDfa (FullFSA(fsa)) = createFromNotFull fsa.NfaToDfa

    let isSubAutomaton (FullFSA(a1)) (FullFSA(a2)) = 
        FSA.IsSubFsa a1 a2 symbolsAreEqual

    let toDot (FullFSA(fsa)) path =
        fsa.PrintToDOT (path, (fun p -> sprintf "%c" (fst p)))

    let toDebugDot ffsa name =
        let path = Path.Combine (myDebugFolderPath, name + ".dot")
        toDot ffsa path

    // Widening operator implementation
    let private initsOrFinalsProblemMsg = 
        "Can't define initial or final states for widened FSA, states may be computed incorrectly"
    let private stateMultipleTransitionsMsg =
        "Current automaton is NFA, is must be converted to DFA before widening"
    let private eqClassMultipleTransitionsMsg =
        "Widened FSA under construction is NFA"

    let private dfsCollectingEdges (state: int) (getNextEdges: int -> list<EdgeFSA<FsaState>>) =
        let rec dfs state visited (edges: list<EdgeFSA<FsaState>>) =
            if not <| Set.contains state visited
            then
                let visited = Set.add state visited
                let nextEdges = getNextEdges state
                let edges = nextEdges @ edges
                nextEdges
                |> List.map (fun e -> e.Target)
                |> List.fold (fun (v, e) succ -> dfs succ v e) (visited, edges)
            else visited, edges
        let _, edges = dfs state Set.empty []
        edges

    let private buildFsaParts state (edges: list<EdgeFSA<FsaState>>) filterStates =
        let states1 = ResizeArray.singleton(state)
        let filterSet = Set.ofSeq filterStates
        let states2 = 
            edges
            |> List.map (fun e -> [e.Source; e.Target])
            |> List.concat
            |> Set.ofList
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
        let edges = dfsCollectingEdges q getOutEdges
        let inits, finals, trans = buildFsaParts q edges fsa.FinalState
        create inits finals trans

    /// Builds the sub automaton that generates the language consisting of words 
    /// accepted by the original automaton with q being the only final state
    let private subFsaTo (fsa: CharFSA) q =
        let bidirectionalFsa = fsa.ToBidirectionalGraph()
        let getInEdges st = List.ofSeq <| bidirectionalFsa.InEdges(st)
        let edges = dfsCollectingEdges q getInEdges
        let finals, inits, trans = buildFsaParts q edges fsa.InitState
        create inits finals trans

    /// Checks if q1 from fsa1 is equivalent to q2 from fsa2
    /// in the sense of relation assumed by widening operator 
    let private isEquivalent q1 (fsa1: CharFSA) q2 (fsa2: CharFSA) =
        let (FullFSA fsaFromQ1) = subFsaFrom fsa1 q1
        let (FullFSA fsaFromQ2) = subFsaFrom fsa2 q2
        if CharFSA.IsSubFsa fsaFromQ1 fsaFromQ2 symbolsAreEqual && 
           CharFSA.IsSubFsa fsaFromQ2 fsaFromQ1 symbolsAreEqual
        then true
        else
            let (FullFSA fsaToQ1) = subFsaTo fsa1 q1
            let (FullFSA fsaToQ2) = subFsaTo fsa2 q2
            let intersection = CharFSA.Intersection (fsaToQ1, fsaToQ2, symbolsAreEqual)
            not <| FSA<_>.IsEmpty intersection

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
        outEdges |> List.map (fun e -> e.Tag)

    let private filterByContainsWithQuantifier quantifier elems (eqClasses: Map<int, EqClass>) =
        eqClasses
        |> Map.filter 
            (fun _ ec -> quantifier (fun e -> EqClassFuns.contains e ec) elems)

    let private createTransition eqClassId (sym: Symb<FsaState>) (eqClasses: Map<int, EqClass>) fsa1 fsa2 =
        let isSink id (fsa: CharFSA) =
            fsa.OutEdges(id) 
            |> Seq.forall (fun e -> e.Target = e.Source)
        let getDstStates srcStates (fsa: CharFSA) =
            srcStates
            |> Set.toSeq
            |> Seq.map
                (
                    fun st -> 
                        fsa.OutEdges(st) 
                        |> Seq.choose
                            (fun e -> if e.Tag = sym then Some(e.Target) else None)
                )
            |> fun lst -> 
                if Seq.length lst = 1 
                then Seq.head lst 
                else failwith stateMultipleTransitionsMsg
            |> Set.ofSeq
            |> Set.filter (fun s -> not <| isSink s fsa)
        let tryCreateTransition srcStates (fsa: CharFSA) =
            let dstStates = getDstStates srcStates fsa
            if Set.isEmpty dstStates
            then Some(eqClassId)
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
        | _ -> None

    let private createTransitions (eqClasses: Map<int, EqClass>) (fsa1: CharFSA) (fsa2: CharFSA) =
        let symbolsMap = Map.map (fun _ v -> symbolsToCheck v fsa1 fsa2) eqClasses
        eqClasses
        |> Map.toList
        |> List.map
            (   
                fun (classId, eqClass) ->
                    Map.find classId symbolsMap
                    |> List.choose 
                        (
                            fun sym -> 
                                createTransition classId sym eqClasses fsa1 fsa2 
                                |> Option.map (fun dst -> classId, sym, dst)
                        )
            )
        |> List.concat
        |> ResizeArray.ofList
        
    let widen (FullFSA(fsa1)) (FullFSA(fsa2)) =
        let eqClasses = buildEquivalenceClasses fsa1 fsa2
        let wTransitions = createTransitions eqClasses fsa1 fsa2
        let wInits = 
            let unitedInits = ResizeArray.append fsa1.InitState fsa2.InitState
            filterByContainsWithQuantifier ResizeArray.forall unitedInits eqClasses
            |> ResizeArray.ofSeq |> ResizeArray.map (fun kvp -> kvp.Key)
        let wFinals = 
            let unitedFinals = ResizeArray.append fsa1.FinalState fsa2.FinalState
            filterByContainsWithQuantifier ResizeArray.exists unitedFinals eqClasses
            |> ResizeArray.ofSeq |> ResizeArray.map (fun kvp -> kvp.Key)
        if ResizeArray.isEmpty wInits || ResizeArray.isEmpty wFinals
        then failwith initsOrFinalsProblemMsg
        create wInits wFinals wTransitions