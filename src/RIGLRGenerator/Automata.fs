module Yard.Generators.RIGLRGenerator.Automata

open System.Collections.Generic
open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.Algorithms
open QuickGraph
open Yard.Generators.Common.FinalGrammar
open ProductionGraph
open EmbeddedRecursion

type RCAEdge =
    | Sh of int
    | Ri of int
    | Push of int
    override this.ToString() = 
        match this with
        | Sh x -> "Sh" + x.ToString()
        | Ri x  -> "R" + x.ToString()
        | Push x -> "Push" + x.ToString()

let constructIRIA (grammar : FinalGrammar) =
    let statesToItems = new Dictionary<int, int*int>(100)
    let inEdges = new Dictionary<int, ResizeArray<EdgeFSA<RCAEdge>>>(100)
    let primaryEpsEdges = new HashSet<int*int>()
    let reductionStates = new ResizeArray<int>(10)
    let dealtWith = new HashSet<int>()
    let nextState = ref 1

    let IRIA = new FSA<RCAEdge>( new ResizeArray<_>([0]), new ResizeArray<_>(),
                                 new ResizeArray<_>() )
    statesToItems.Add (0, (grammar.startRule, 0))

    let addState state item parent edgeTag =
        let newEdge = new EdgeFSA<_>(parent, state, edgeTag)
        IRIA.AddVerticesAndEdge newEdge |> ignore
        inEdges.Add (state, new ResizeArray<_>([newEdge]))
        statesToItems.Add (state, item)
        if edgeTag = Eps 
        then primaryEpsEdges.Add (parent, state) |> ignore        
        incr nextState
    
    let addEdge source target tag =
        let newEdge = new EdgeFSA<_>(source, target, tag)
        IRIA.AddEdge newEdge |> ignore
        inEdges.[target].Add newEdge
        
    let isPrimaryEdge (edge: EdgeFSA<RCAEdge>) =
        match edge.Tag with
        | Smbl(Sh x) -> true
        | Eps -> primaryEpsEdges.Contains (edge.Source, edge.Target)
        | _ -> failwith "Unexpected edge tag"
    
    let findPrimaryEdge edges = Seq.find (fun e -> isPrimaryEdge e) edges    
    
    let primaryParent state = (findPrimaryEdge inEdges.[state]).Source    
    let primaryDescendant state = (findPrimaryEdge (IRIA.OutEdges state)).Target
     
    let findRecursionStates nonTerm state =
        let targetRules = grammar.rules.rulesWithLeftSide nonTerm
        let rec findStates curState result =
            if curState = 0
            then result
            else
                let item = statesToItems.[curState]
                let nextState = primaryParent curState
                if Array.exists ((=) (fst item)) targetRules && snd item = 0
                then findStates nextState ((curState, fst item) :: result)
                else findStates nextState result
        findStates state []
    
    let rec retraceAlongPath startState path resultSet =
        match path with
        | [] -> startState :: resultSet
        | h :: t -> 
            let predecessors = inEdges.[startState] 
                               |> Seq.where (fun e -> e.Tag = h)
                               |> Seq.map (fun e -> e.Source)
            predecessors
            |> Seq.map (fun s -> retraceAlongPath s t resultSet)
            |> Seq.concat |> Seq.toList
            
    while IRIA.VertexCount > dealtWith.Count do
        let curState = IRIA.Vertices 
                       |> Seq.find (fun x -> not (dealtWith.Contains x))
        let curItem = statesToItems.[curState]       
        let rule, pos = fst curItem, snd curItem
        if grammar.rules.length rule > pos
        then
            let nextSymbol = grammar.rules.symbol rule pos
            addState !nextState (rule, pos + 1) curState (Smbl(Sh(nextSymbol)))
            if grammar.indexator.isNonTerm nextSymbol 
            then
                if nextSymbol <> grammar.rules.leftSide rule || pos <> 0  //they want xor
                then
                    let recursionStates = findRecursionStates nextSymbol curState
                    for rule in grammar.rules.rulesWithLeftSide nextSymbol do
                        match List.tryFind (fun (s, r) -> rule = r) recursionStates with
                        | Some (s, r) -> addEdge curState s Eps
                        | None -> addState !nextState (rule, 0) curState Eps
                elif curState <> 0
                then
                    primaryParent curState
                    |> IRIA.OutEdges
                    |> Seq.filter (fun x -> x.Tag = Eps)
                    |> Seq.iter (fun x -> addEdge curState x.Target Eps)
        else reductionStates.Add curState |> ignore  
        dealtWith.Add curState |> ignore        
    for state in reductionStates |> Seq.toList |> List.tail do
        let rule = fst statesToItems.[state]
        let pathForRetrace = rule
                             |> grammar.rules.rightSide
                             |> Array.map (fun x -> Smbl(Sh(x)))
                             |> Array.toList
        let reductionTargets = retraceAlongPath state (Eps :: pathForRetrace |> List.rev) []
                               |> List.map (fun s -> primaryDescendant s)
        reductionTargets |> List.iter (fun t -> addEdge state t (Smbl(Ri(rule))))
    
    IRIA.FinalState.Add 1
    IRIA

let constructRIA (grammar: FinalGrammar) =
    let IRIA = constructIRIA grammar
    IRIA.RemoveEdgeIf (
                        fun edge ->
                            match edge.Tag with
                            | Smbl(Sh(n)) -> grammar.indexator.isNonTerm n
                            | _ -> false
                      ) |> ignore
    IRIA.NfaToDfa()

let constructRCA (grammar: FinalGrammar) =   
        
    //TODO
    let joinAutomata automata = ()    

    let removeEmbeddedRec = new RemoveEmbeddedRecursion (grammar)
    removeEmbeddedRec.ConvertGrammar()

    let terminalized = removeEmbeddedRec.TerminalizedNonTerms
    let x = constructIRIA grammar
    //x.PrintToDOT ("iria.dot", (fun x -> x.ToString()))
    
    printfn "I <3 embedded recursion"