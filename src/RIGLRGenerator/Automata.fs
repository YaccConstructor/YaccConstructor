module Yard.Generators.RIGLRGenerator.Automata

open System.Collections.Generic
open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.Algorithms
open QuickGraph
open Yard.Generators.Common.FinalGrammar
open ProductionGraph
open EmbeddedRecursion

type RIAEdge =
    | Sh of int
    | Ri of int
    | Push of int
    override this.ToString() = 
        match this with
        | Sh x -> "Sh" + x.ToString()
        | Ri x  -> "R" + x.ToString()
        | Push x -> "Push" + x.ToString()

type IRIA(grammar: FinalGrammar) as this =
    inherit FSA<RIAEdge>()
    do
        let statesToItems = new Dictionary<int, int*int>(100)
        let inEdges = new Dictionary<int, ResizeArray<EdgeFSA<RIAEdge>>>(100)
        let primaryEpsEdges = new HashSet<int*int>()
        let reductionStates = new ResizeArray<int>(10)
        let dealtWith = new HashSet<int>()
        let nextState = ref 1

        this.AddVertex 0 |> ignore
        statesToItems.Add (0, (grammar.startRule, 0))
        this.InitState.Add 0

        let addState state item parent edgeTag =
            let newEdge = new EdgeFSA<_>(parent, state, edgeTag)
            this.AddVerticesAndEdge newEdge |> ignore
            inEdges.Add (state, new ResizeArray<_>([newEdge]))
            statesToItems.Add (state, item)
            if edgeTag = Eps 
            then primaryEpsEdges.Add (parent, state) |> ignore
            incr nextState
        
        let addEdge source target tag =
            let newEdge = new EdgeFSA<_>(source, target, tag)
            this.AddEdge newEdge |> ignore
            inEdges.[target].Add newEdge
            
        let isPrimaryEdge (edge: EdgeFSA<_>) =
            match edge.Tag with
            | Smbl(Sh x) -> true
            | Eps -> primaryEpsEdges.Contains (edge.Source, edge.Target)
            | _ -> failwith "Unexpected edge tag"
        
        let findPrimaryEdge edges = Seq.find (fun e -> isPrimaryEdge e) edges
        let primaryParent state = (findPrimaryEdge inEdges.[state]).Source    
        let primaryDescendant state = (findPrimaryEdge (this.OutEdges state)).Target
         
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
                
        while this.VertexCount > dealtWith.Count do
            let curState = this.Vertices 
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
                        |> this.OutEdges
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
        this.FinalState.Add 1

//they should be combined into one class
type RIA(grammar: FinalGrammar) as this =
    inherit FSA<RIAEdge>()
    do
        let IRIA = IRIA(grammar)
        IRIA.PrintToDOT ("iria.dot", (fun x -> x.ToString()))
        IRIA.RemoveEdgeIf
             (
                 fun edge ->
                     match edge.Tag with
                     | Smbl(Sh(n)) -> grammar.indexator.isNonTerm n
                     | _ -> false
             ) |> ignore
        let temp = IRIA.NfaToDfa()        
        this.AddVerticesAndEdgeRange temp.Edges |> ignore
        this.InitState <- temp.InitState
        this.FinalState <- temp.FinalState

    member val InitNonTerm = (grammar.rules.rightSide grammar.startRule).[0] with get

type RCA(grammar: FinalGrammar) as this =
    inherit FSA<RIAEdge>()

    let mutable popStates = Set.empty
    
    let joinRIAs (terminalized: Dictionary<_, _>) (baseRIA: RIA) (RIAs: list<RIA>) =
        let termToStartState = new Dictionary<int, int>(RIAs.Length)
        let riaStartInRCA = ref baseRIA.VertexCount
        let newLabels = new Dictionary<_, _>()           
        let popStatesSet = new HashSet<_>()
        let i = ref 0

        if terminalized.ContainsKey baseRIA.InitNonTerm
        then termToStartState.Add (terminalized.[baseRIA.InitNonTerm], 0)
        for ria in RIAs do
            termToStartState.Add (terminalized.[ria.InitNonTerm], !riaStartInRCA)
            riaStartInRCA := !riaStartInRCA + ria.VertexCount
        
        let addEdge (newLabels: Dictionary<_, _>) (edge: EdgeFSA<_>) =
            let add src target tag =
                new EdgeFSA<_>(src, target, tag)
                |> this.AddVerticesAndEdge 
                |> ignore

            let newSource, newTarget = newLabels.[edge.Source], newLabels.[edge.Target]
            match edge.Tag with
            | Smbl(Sh(n)) as shift -> 
                if termToStartState.ContainsKey n
                then
                    let pushTag = Smbl(Push(newTarget))
                    add newSource termToStartState.[n] pushTag
                else add newSource newTarget shift
            | _  as tag -> add newSource newTarget tag
        
        let addRIA (ria: RIA) =
            ria.Vertices |> Seq.iter (fun v -> newLabels.Add (v, !i); incr i)
            ria.Edges |> Seq.iter (fun e -> addEdge newLabels e)
            ria.FinalState |> Seq.iter (fun s -> popStatesSet.Add newLabels.[s] |> ignore)
        
        addRIA baseRIA
        this.FinalState <- new ResizeArray<_>(baseRIA.FinalState |> Seq.map (fun s -> newLabels.[s]))
        newLabels.Clear()
        RIAs |> List.iter (fun ria -> addRIA ria; newLabels.Clear())
        popStates <- Set.ofSeq popStatesSet        
    do
        let removeEmbeddedRec = new RemoveEmbeddedRecursion (grammar)
        removeEmbeddedRec.ConvertGrammar()
        
        let terminalized = removeEmbeddedRec.TerminalizedNonTerms
        let baseRIA = new RIA(grammar)
        baseRIA.PrintToDOT ("RIADot.dot", (fun x -> x.ToString()))
        let initNonTerm = baseRIA.InitNonTerm

        if terminalized.Count > 0 
        then
            if terminalized.Count = 1 && terminalized.ContainsKey baseRIA.InitNonTerm
            then joinRIAs terminalized baseRIA []
            else
                let RIAs = terminalized.Keys
                           |> Seq.toList
                           |> List.filter (fun s -> s <> baseRIA.InitNonTerm)
                           |> List.map 
                                   (
                                       fun nonTerm -> 
                                           grammar.rules.setRightSide grammar.startRule [|nonTerm|];
                                           new RIA(grammar)
                                   )
                joinRIAs terminalized baseRIA RIAs          
        else 
            this.AddVerticesAndEdgeRange baseRIA.Edges |> ignore   
            this.FinalState <- baseRIA.FinalState             
        this.InitState <- baseRIA.InitState        
                         
    member val PopStates = popStates with get
                    
    //                 R-actions                 p-actions                  shifts
    // state -> [(rule, stateTo), ...], [(pushState, stateTo), ...], [(term, stateTo), ...]
    member this.ToTable() = 
        let table = Array.zeroCreate this.VertexCount
        for state in this.Vertices do
            let reduce, push, shift = new ResizeArray<_>(), new ResizeArray<_>(), 
                                      new ResizeArray<_>()
            for edge in this.OutEdges state do
                match edge.Tag with
                | Smbl(Sh x) -> shift.Add (x, edge.Target)
                | Smbl(Ri i) -> reduce.Add (i, edge.Target)
                | Smbl(Push s) -> push.Add (s, edge.Target)
            shift.Sort()            
            table.[state] <- [| Seq.toArray(reduce); Seq.toArray(push); Seq.toArray(shift) |]
        table
