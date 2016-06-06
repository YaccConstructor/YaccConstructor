module Yard.EBNF.DFA.StateSets
open Yard.Generators.Common

//Vertices have to have labels as their indices in arrays
let dfaLoopStates (dfas : (Vertex<int,int>[] * _ * _)[]) =
    let result = Array.zeroCreate dfas.Length
    for i = 0 to dfas.Length - 1 do
        let stateToVertex, _, _ = dfas.[i]
        let reachableStates = Array.init stateToVertex.Length (fun j -> stateToVertex.[j].outEdges |> Seq.map (fun x -> x.dest.label) |> Set.ofSeq)
        result.[i] <- Array.init stateToVertex.Length (fun j -> Set.contains j reachableStates.[j])
        let modified = ref true
        while !modified do
            modified := false
            for j = 0 to stateToVertex.Length - 1 do
                if not result.[i].[j] then
                    let nextReachable = stateToVertex.[j].outEdges |> Seq.map (fun x -> reachableStates.[x.dest.label]) |> Set.unionMany
                    let diff = Set.difference nextReachable reachableStates.[j]
                    if not diff.IsEmpty then
                        reachableStates.[j] <- Set.union diff reachableStates.[j]
                        modified := true
                        if Set.contains j diff then
                            result.[i].[j] <- true
    result