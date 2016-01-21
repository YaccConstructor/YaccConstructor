module ControlFlowGraph.TokensExtractor

open System.Collections.Generic

open Yard.Generators.Common.AstNode
open CfgTokensGraph
open ControlFlowGraph.Printers

type private ASTProcessingState = 
| Processed of int * int
| InProgress of int

/// <summary>
///<para>Takes family and builds graph.
///Each edge is labeled some token.</para><br />
///<para>If there aren't ambiguous then graph'll be linear.
///Otherwise it'll contain branches.</para><br />
/// </summary>
let extractNodesFromFamily intToToken tokenToString (fam : Family) = 
    let tokensGraph = new CfgTokensGraph<_>()
    let cache = new Dictionary<Family, ASTProcessingState>()
            
    let rec collectTokens (node : obj) = 
            
        match node with 
        | :? Terminal as t -> 
            let tag = Some <| intToToken t.TokenNumber
            tokensGraph.AddEdge tag
            tokensGraph.UpdateVertex()

        | :? AST as ast -> 
            let commonStartVertex = tokensGraph.CurrentVertex
            let allEndVertex = 
                ast.map (fun fam -> processFamily commonStartVertex fam)
                |> Array.filter Option.isSome
                |> Array.map Option.get

            let commonEndVertex = allEndVertex |> Array.max

            allEndVertex
            |> Array.filter ((<>) commonEndVertex)
            |> Array.iter (fun num -> tokensGraph.AddEdgeFromTo None num commonEndVertex)

            tokensGraph.UpdateVertex()

        | _ -> failwith "Unexpected AST node type in Control-Flow construction"

    and processFamily startVertex fam = 
        tokensGraph.CurrentVertex <- startVertex

        if cache.ContainsKey fam
        then
            match cache.[fam] with
            | Processed (source, target) -> 
                if tokensGraph.CurrentVertex <> source
                then
                    tokensGraph.AddEdgeFromTo None tokensGraph.CurrentVertex source
                tokensGraph.CurrentVertex <- target
                tokensGraph.LastVertex <- target
                Some target
            | InProgress _ -> None
        else
            cache.[fam] <- InProgress(startVertex)
            fam.nodes.doForAll (fun node -> collectTokens node)
            cache.[fam] <- Processed(startVertex, tokensGraph.CurrentVertex)
            
            Some <| tokensGraph.CurrentVertex
            
    fam.nodes.doForAll(fun node -> collectTokens node)
    CfgTokensGraphPrinter.ToDot tokensGraph tokenToString "`afterExtraction.dot"
    tokensGraph

/// <summary>
///<para>Takes AST and builds graph.
///Each edge is labeled some token.</para><br />
///<para>If there aren't ambiguous then graph'll be linear.
///Otherwise it'll contain branches.</para><br />
/// </summary>
let extractNodesFromAST (ast : AST) intToToken tokenToString = 
    ast.map (extractNodesFromFamily intToToken tokenToString)