module Yard.Core.Conversions.RegularApproximation

open QuickGraph
open Yard.Core
open Yard.Core.IL

let private grammarToGraph (grammar : Grammar<_,_>) =

    let handleOneRule (rule : Rule<_,_>) =
        List.map (fun nonTerm -> SEdge<Source>(rule.name, nonTerm)) <| getAllNonTermOfProd rule.body

    let edges = List.collect handleOneRule grammar.Head.rules
    edges.ToAdjacencyGraph<Source, SEdge<Source>>()

let inline private getStronglyConnectedComps graph =
    QuickGraph.Algorithms.ConnectedComponents.StronglyConnectedComponentsAlgorithm(graph).Graphs
