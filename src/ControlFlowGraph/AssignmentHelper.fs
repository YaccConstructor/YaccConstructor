module ControlFlowGraph.AssignmentHelper

open Yard.Generators.Common.AstNode

open ControlFlowGraph.Common
open ControlFlowGraph.InnerGraph
open ControlFlowGraph.CfgTokensGraph
open ControlFlowGraph.Printers
open TokensExtractor

let processExpression intToToken tokenToString family =
    let graph = extractNodesFromFamily intToToken tokenToString family
    Simple(Expression, graph)

let processId intToToken tokenToString family =
    extractNodesFromFamily intToToken tokenToString family
    

/// <summary>
/// Takes ast family deriving assignment statement and builds appropriate graph.
/// </summary>
let rec processAssignment familyToBlock intToToken tokenToString (family : Family) = 
    
    let printTokens (graph : CfgTokensGraph<_>) = 
        graph.GetAvailableTokens()
        |> Seq.map tokenToString

    let processExpression' = processExpression intToToken tokenToString
    let processId' = processId intToToken tokenToString

    let idEdge = ref Unchecked.defaultof<_>
    let expressionEdge = ref EmptyEdge
    let innerAssignmentEdge = ref EmptyEdge

    let rec processFamily (fam : Family) = 
        match familyToBlock fam with
        | Some block -> 
            match block with
            | Identificator -> idEdge := processId' fam
            | Expression -> expressionEdge := processExpression' fam
            | Assignment -> 
                innerAssignmentEdge := processAssignment familyToBlock intToToken tokenToString fam
            | x -> invalidOp <| sprintf "Error in assignment procession: unexpected block type %A" x
        | None -> 
            fam.nodes.doForAll processNode

    and processNode (node : obj) = 
        match node with
        | :? Epsilon 
        | :? Terminal -> ()
        | :? AST as ast -> ast.doForAllFamilies processFamily
        | x -> failwithf "Unexpected node type: %A" x

    family.nodes.doForAll processNode
    
    let graphBuilder = new BlocksGraphBuilder<_>()
    let addEdge edge = 
        graphBuilder.AddEdge edge
        graphBuilder.UpdateVertex()
    
    [!innerAssignmentEdge; !expressionEdge]
    |> List.iter
        (
            fun edge -> 
                match edge with
                | EmptyEdge -> ()
                | _ -> addEdge edge
        )

    let rightPartGraph = graphBuilder.Build()

    let graphOpt = 
        if rightPartGraph.VertexCount >= 2
        then Some rightPartGraph
        else None
    
    //InnerGraphPrinter.RelaxedPrintToDot graph "``assignment graph.dot" <| Some(tokenToString)
    AssignmentEdge(Assignment, !idEdge, graphOpt)