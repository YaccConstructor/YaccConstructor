module ControlFlowGraph.Helper

open System.Collections.Generic

open Yard.Generators.Common.AstNode

open ControlFlowGraph.Common
open ControlFlowGraph.InnerGraph

let createIfGraph condGraph thenGraph (elseGraph : _ option) = 
    let condEdge = new BlockEdge<_>(0, 1, Complicated (Condition, condGraph))
    let thenEdge = new BlockEdge<_>(1, 2, Complicated (ThenStatement, thenGraph))
    let elseEdge = 
        if elseGraph.IsSome
        then new BlockEdge<_>(1, 2, Complicated (ElseStatement, elseGraph.Value))
        else new BlockEdge<_>(1, 2, EmptyEdge)
                        
    let ifGraph = new CfgBlocksGraph<_>()
    ifGraph.AddEdgeForced condEdge
    ifGraph.AddEdgeForced thenEdge
    ifGraph.AddEdgeForced elseEdge

    ifGraph