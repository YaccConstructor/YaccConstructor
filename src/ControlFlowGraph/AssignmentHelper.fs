module ControlFlowGraph.AssignmentHelper

open Yard.Generators.Common.AstNode

open ControlFlowGraph.Common
open ControlFlowGraph.InnerGraph
open TokensExtractor

/// <summary>
/// Takes ast family deriving assignment statement and builds appropriate graph.
/// <param name="intToToken">Mapping from position in input to token</param>
/// <param name="family">AST node that would be processed</param>
/// </summary>
let processAssignment intToToken tokenToString (family : Family) = 
    let toksGraph = extractNodesFromFamily intToToken tokenToString family
    Simple(Assignment, toksGraph)