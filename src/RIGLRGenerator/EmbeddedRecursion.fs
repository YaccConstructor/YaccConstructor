module EmbeddedRecursion

open Yard.Generators.Common.FinalGrammar
open System.Collections.Generic
open ProductionGraph
open QuickGraph.Algorithms

type RemoveEmbeddedRecursion (grammar: FinalGrammar) =  
    let terminalizedNonTerms = new Dictionary<int, int>()
    let firstSpecialTerm = grammar.indexator.fullCount + 1

    let getSpecialTerm nonTerm = 
        if terminalizedNonTerms.ContainsKey nonTerm
        then terminalizedNonTerms.[nonTerm]
        else 
            let nextSpecialTerm = firstSpecialTerm + terminalizedNonTerms.Count
            terminalizedNonTerms.Add (nonTerm, nextSpecialTerm)
            nextSpecialTerm  
    
    let terminalizeNonTerm nonTerm cycleType rules =
        let replaceAt index newValue (array: array<_>) =
            if array.[index] = nonTerm
            then array.[index] <- newValue
        for rule in rules do
            let rightSide = grammar.rules.rightSide rule
            let lastIndex = rightSide.Length - 1
            let specialTerm = getSpecialTerm nonTerm
            match cycleType with
                | B -> 
                    if rightSide.Length > 1
                    then
                        for i in 1 .. lastIndex do
                            if rightSide.[i] = nonTerm
                            then rightSide.[i] <- specialTerm
                | L -> replaceAt lastIndex specialTerm rightSide
                | R -> replaceAt 0 specialTerm rightSide 

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
                            terminalizeNonTerm start B (grammar.rules.rulesWithLeftSide curVertex)
                            scc.RemoveEdge e |> ignore
                        | L -> lCycles.Push cycle
                        | R -> rCycles.Push cycle
                        if lCycles.Count > 0 && rCycles.Count > 0
                        then
                            let rCycle = rCycles.Pop()
                            let lastVertex = rCycle.[rCycle.Length - 2]
                            terminalizeNonTerm start R (grammar.rules.rulesWithLeftSide lastVertex)
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
                | Some e -> terminalizeNonTerm vertex B (grammar.rules.rulesWithLeftSide vertex)
                | None -> ()
        for nonTerm in terminalizedNonTerms.Keys do
            printf "%A, " (grammar.indexator.indexToNonTerm nonTerm)
        printfn ""  

    member this.TerminalizedNonTerms with get() = terminalizedNonTerms
    member this.ConvertGrammar() = removeEmbeddedRecursion grammar  