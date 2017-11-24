module Yard.Core.Conversions.RegularApproximation

open QuickGraph
open Yard.Core
open Yard.Core.IL

let private grammarToGraph (grammar : Grammar.t<_,_>) =

    let handleOneRule (rule : Rule.t<_,_>) =
        List.map (fun nonTerm -> SEdge<Source.t>(rule.name, nonTerm)) <| getAllNonTermOfProd rule.body

    let rules = List.collect (fun (module' : Grammar.Module<_,_>) -> module'.rules) grammar
    let edges = List.collect handleOneRule rules
    edges.ToAdjacencyGraph<Source.t, SEdge<Source.t>>()
