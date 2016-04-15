module ControlFlowGraph.TokensExtractor

open System.Collections.Generic

open Yard.Generators.Common.AstNode

open CfgTokensGraph
open ControlFlowGraph.Printers

type ASTProcessingState = 
| Processed of int * int
| InProgress of int

type private EpsilonEdge(source, target) = 
    member this.Source with get() = source
    member this.Target with get() = target

/// <summary>
///<para>Takes family and builds graph.
///Each edge is labeled some token.</para><br />
///<para>If there aren't ambiguous then graph'll be linear.
///Otherwise it'll contain branches.</para><br />
/// </summary>
let extractNodesFromFamily intToToken tokenToString (fam : Family) = 
    let builder = new TokensGraphBuilder<_>()
    let cache = new Dictionary<Family, ASTProcessingState>()
    //hack against duplicated epsilon edges
    let addEpsilonEdge = 
        let epsEdges = new ResizeArray<_>()
        fun source target ->
            if source <> target 
            then 
                let epsilon = new EpsilonEdge(source, target)
                if not <| epsEdges.Contains epsilon
                then
                    builder.AddEdgeFromTo None source target
                    epsEdges.Add epsilon

    let rec collectTokens (node : obj) = 
            
        match node with 
        | :? Terminal as t -> 
            let tag = Some <| intToToken t.TokenNumber
            builder.AddEdge tag
            builder.UpdateVertex()

        | :? AST as ast -> 
            let commonStartVertex = builder.CurrentVertex
            let allEndVertex = 
                ast.map (processFamily commonStartVertex)
                |> Array.filter Option.isSome 
                |> Array.map Option.get

            if not <| Array.isEmpty allEndVertex
            then 
                let commonEndVertex = allEndVertex |> Array.max

                allEndVertex
                |> Array.filter ((<>) commonEndVertex)
                |> Array.iter (fun num -> addEpsilonEdge num commonEndVertex)

                builder.UpdateVertex()
        | :? Epsilon -> ()
        | _ -> invalidOp "Unexpected AST node type in Control-Flow construction"

    and processFamily startVertex fam = 
        builder.CurrentVertex <- startVertex

        match cache.TryGetValue fam with
        | true, value ->
            match value with
            | Processed (source, target) -> 
                addEpsilonEdge builder.CurrentVertex source
                builder.CurrentVertex <- target
                builder.NextVertex <- target
                Some target
            | InProgress start -> 

                addEpsilonEdge startVertex start
                match builder.TryFindLastVertex start with
                | Some vertex -> 
                    cache.[fam] <- Processed(start, vertex)
                    builder.CurrentVertex <- vertex
                    builder.NextVertex <- vertex
                    Some vertex
                | None -> None
        | false, _ ->
            cache.[fam] <- InProgress(startVertex)
            fam.nodes.doForAll collectTokens
            cache.[fam] <- Processed(startVertex, builder.CurrentVertex)
            
            Some <| builder.CurrentVertex
            
    fam.nodes.doForAll collectTokens
    let graph = builder.Build()
    //CfgTokensGraphPrinter.ToDot graph tokenToString "`afterExtraction.dot"
    graph
    //builder.Build()

/// <summary>
///<para>Takes AST and builds graph.
///Each edge is labeled some token.</para><br />
///<para>If there aren't ambiguous then graph'll be linear.
///Otherwise it'll contain branches.</para><br />
/// </summary>
let extractNodesFromAST intToToken tokenToString (ast : AST) = 
    ast.map (extractNodesFromFamily intToToken tokenToString)