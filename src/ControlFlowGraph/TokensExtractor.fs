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
    let graph = new CfgTokensGraph<_>()
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
                    graph.AddEdgeFromTo None source target
                    epsEdges.Add epsilon

    let rec collectTokens (node : obj) = 
            
        match node with 
        | :? Terminal as t -> 
            let tag = Some <| intToToken t.TokenNumber
            graph.AddEdge tag
            graph.UpdateVertex()

        | :? AST as ast -> 
            let commonStartVertex = graph.CurrentVertex
            let allEndVertex = 
                ast.map (fun fam -> processFamily commonStartVertex fam)
                |> Array.filter Option.isSome
                |> Array.map Option.get

            if allEndVertex.Length > 0
            then 
                let commonEndVertex = allEndVertex |> Array.max

                allEndVertex
                |> Array.filter ((<>) commonEndVertex)
                |> Array.iter (fun num -> addEpsilonEdge num commonEndVertex)

                graph.UpdateVertex()

        | _ -> failwith "Unexpected AST node type in Control-Flow construction"

    and processFamily startVertex fam = 
        graph.CurrentVertex <- startVertex

        if cache.ContainsKey fam
        then
            match cache.[fam] with
            | Processed (source, target) -> 
                addEpsilonEdge graph.CurrentVertex source
                graph.CurrentVertex <- target
                graph.NextVertex <- target
                Some target
            | InProgress start -> 

                addEpsilonEdge startVertex start
                match graph.TryFindLastVertex start with
                | Some vertex -> 
                    cache.[fam] <- Processed(start, vertex)
                    graph.CurrentVertex <- vertex
                    graph.NextVertex <- vertex
                    Some vertex
                | None -> None
        else
            cache.[fam] <- InProgress(startVertex)
            fam.nodes.doForAll collectTokens
            cache.[fam] <- Processed(startVertex, graph.CurrentVertex)
            
            Some <| graph.CurrentVertex
            
    fam.nodes.doForAll collectTokens
    CfgTokensGraphPrinter.ToDot graph tokenToString "`afterExtraction.dot"
    graph

/// <summary>
///<para>Takes AST and builds graph.
///Each edge is labeled some token.</para><br />
///<para>If there aren't ambiguous then graph'll be linear.
///Otherwise it'll contain branches.</para><br />
/// </summary>
let extractNodesFromAST (ast : AST) intToToken tokenToString = 
    ast.map (extractNodesFromFamily intToToken tokenToString)