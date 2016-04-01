module Yard.Generators.RIGLRGenerator.Automata

open Yard.Generators.Common.FinalGrammar
open QuickGraph.FSA.GraphBasedFsa
open System.Collections.Generic
open ProductionGraph
open QuickGraph.Algorithms
open QuickGraph

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
        printfn "%A" curState
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
    let RIA = new FSA<RCAEdge>()
    IRIA.Edges 
    |> Seq.iter
          (
              fun edge ->
                  match edge.Tag with
                  | Smbl(Sh(n)) -> 
                        if not (grammar.indexator.isNonTerm n)
                        then RIA.AddVerticesAndEdge (edge) |> ignore
                  | _ -> RIA.AddVerticesAndEdge (edge) |> ignore
          )
    RIA.InitState <- IRIA.InitState
    RIA.FinalState <- IRIA.FinalState
    RIA.NfaToDfa()
    
//    let nonTermEdges = IRIA.Edges 
//                       |> Seq.filter 
//                             (
//                                 fun edge ->
//                                     match edge.Tag with
//                                     | Smbl(Sh(n)) -> grammar.indexator.isNonTerm n
//                                     | _ -> false
//                             )
//                       |> Seq.map (fun e -> new EdgeFSA<RCAEdge>(e.Source, e.Target, e.Tag))
//    Seq.iter (fun edge -> IRIA.RemoveEdge edge |> ignore) nonTermEdges   lie
//    IRIA.NfaToDfa()

let constructRCA (grammar: FinalGrammar) =   
    
    let replacedNonTerms = new Dictionary<int, int>()   
    let firstSpecialTerm = grammar.indexator.fullCount + 1

    let getSpecialTerm nonTerm = 
        if replacedNonTerms.ContainsKey nonTerm
        then replacedNonTerms.[nonTerm]
        else 
            let nextSpecialTerm = firstSpecialTerm + replacedNonTerms.Count
            replacedNonTerms.Add (nonTerm, nextSpecialTerm)
            nextSpecialTerm  

    let removeEmbeddedRecursion grammar = 
        let productionGraph = new ProductionGraph(grammar)
        let scc = new Dictionary<int, ResizeArray<int>>()

        let pathType (path: array<_>) =
            let edges = new ResizeArray<_>()
            for i in 0 .. path.Length - 2 do 
                path.[i]
                |> productionGraph.OutEdges
                |> Seq.find (fun e -> e.Target = path.[i + 1])
                |> edges.Add
            if edges |> Seq.exists (fun e -> e.Tag <> edges.[0].Tag) 
            then B
            else edges.[0].Tag
                                
        let replaceNonTerm nonTerm cycleType rules =
            let replaceAt index newValue (array: array<_>) =
                if array.[index] = nonTerm
                then array.[index] <- newValue
            for rule in rules do
                let rightSide = grammar.rules.rightSide rule
                let lastIndex = rightSide.Length - 1
                let specialTerm = getSpecialTerm nonTerm
                match cycleType with
                    | B -> 
                        for i in 0 .. lastIndex do
                            if rightSide.[i] = nonTerm
                            then rightSide.[i] <- specialTerm
                    | L -> replaceAt lastIndex specialTerm rightSide
                    | R -> replaceAt 0 specialTerm rightSide        
        
        let dfsWithCycleHandling (scc: ProductionGraph) start = 
            let visited = new HashSet<_>()
            let prefix = new Stack<_>([start])
            let lCycles = new Stack<array<int>>()
            let rCycles = new Stack<array<int>>()

            let removeBackEdge vertex =
                vertex |> scc.OutEdges
                |> Seq.find (fun e -> e.Target = start)
                |> scc.RemoveEdge |> ignore

            while prefix.Count > 0 do
                let curVertex = prefix.Peek()
                if not (visited.Contains curVertex)
                then
                    let backEdgeOpt = curVertex
                                      |> scc.OutEdges
                                      |> Seq.tryFind (fun e -> e.Target = start) 
                    match backEdgeOpt with
                    | Some e ->
                        let cycle = Array.append (Array.rev (prefix.ToArray())) [|start|]
                        match pathType cycle with
                        | B ->                            
                            replaceNonTerm start B (grammar.rules.rulesWithLeftSide curVertex)
                            scc.RemoveEdge e |> ignore
                        | L -> lCycles.Push cycle
                        | R -> rCycles.Push cycle
                        if lCycles.Count > 0 && rCycles.Count > 0
                        then
                            let rCycle = rCycles.Pop()
                            let lastVertex = rCycle.[rCycle.Length - 2]
                            replaceNonTerm start R (grammar.rules.rulesWithLeftSide lastVertex)
                            removeBackEdge lastVertex
                            lCycles.Pop() |> ignore
                    | None -> ()
                let succesorOpt = curVertex
                                  |> scc.OutEdges
                                  |> Seq.tryFind (fun e -> not (visited.Contains e.Target)) 
                match succesorOpt with
                | Some e -> prefix.Push (e.Target)
                | None -> prefix.Pop() |> ignore
                visited.Add curVertex |> ignore
       
        let removeRecursionFromSCC (scc: ProductionGraph) =
            for v in scc.Vertices do
                dfsWithCycleHandling scc v

        for pair in snd (productionGraph.StronglyConnectedComponents()) do
            let vertex, sccNum = pair.Key, pair.Value
            if scc.ContainsKey sccNum
            then scc.[sccNum].Add vertex
            else scc.Add (sccNum, new ResizeArray<int>([vertex]))

        for pair in scc do
            let sccNum, verticies = pair.Key, pair.Value
            if verticies.Count > 1
            then
                let sccSubgraph = new ProductionGraph()
                for vertex in verticies do
                    vertex
                    |> productionGraph.OutEdges 
                    |> Seq.filter (fun e -> verticies.Contains e.Target)
                    |> Seq.iter (fun e -> sccSubgraph.AddVerticesAndEdge e |> ignore)
                removeRecursionFromSCC sccSubgraph
            else
                let vertex = verticies.[0]
                let selfLoopOpt = vertex
                                  |> productionGraph.OutEdges
                                  |> Seq.tryFind (fun e -> e.Target = vertex && e.Tag = B)
                match selfLoopOpt with
                | Some e -> replaceNonTerm vertex B (grammar.rules.rulesWithLeftSide vertex)
                | None -> ()
        for nonTerm in replacedNonTerms.Keys do
            printf "%A, " (grammar.indexator.indexToNonTerm nonTerm)
        printfn ""
    
    //TODO
    let joinAutomata automata = ()    

    removeEmbeddedRecursion grammar
    
    printfn "I <3 embedded recursion"