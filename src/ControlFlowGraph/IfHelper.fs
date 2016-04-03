module ControlFlowGraph.IfHelper

open System.Collections.Generic

open Yard.Generators.Common.AstNode

open ControlFlowGraph.Common
open ControlFlowGraph.InnerGraph
open TokensExtractor

/// <summary>
/// Creates a new graph in which the edges are labelled 
/// either conditional graph, or "then branch" graph or "else branch" graph.
/// </summary>
let private createIfGraph condGraph thenGraph (elseGraph : _ option) = 
    let condEdge = new BlockEdge<_>(0, 1, Complicated (Condition, condGraph))
    let thenEdge = new BlockEdge<_>(1, 2, Complicated (ThenStatement, thenGraph))
    let elseEdge = 
        match elseGraph with
        | Some elseGraph -> new BlockEdge<_>(1, 2, Complicated (ElseStatement, elseGraph))
        | None -> new BlockEdge<_>(1, 2, EmptyEdge)
                        
    let ifGraph = new CfgBlocksGraph<_>()
    ifGraph.AddEdgeForced condEdge
    ifGraph.AddEdgeForced thenEdge
    ifGraph.AddEdgeForced elseEdge

    ifGraph

/// <summary>
/// Takes ast family deriving 'if-then-else' and builds appropriate graph.
/// </summary>
/// <param name="intToToken">Position in input to token mapping</param>
/// <param name="tokenToNumber">Token to int mapping</param>
/// <param name="tempDict">Key is non-terminal number, value is block type of cfg</param>
/// <param name="family">AST node that would be processed</param>
/// <param name="processSeq">This function will be  then и else branches</param>
let processIf intToToken tokenToNumber (tempDict : Dictionary<int, _>) (family : Family) processSeq = 
    let blockType = ref IfStatement
                        
    let condGraph = new GraphConstructor<_>()
    let thenGraph = new GraphConstructor<_>()
    let elseGraphOpt = ref None

    let processNode (node : obj) = 
        match node with
        | :? Epsilon -> ()
        | :? Terminal as t -> 
            let terminalNum = tokenToNumber <| intToToken t.TokenNumber
            if tempDict.ContainsKey terminalNum 
            then blockType := tempDict.[terminalNum]

        | :? AST as ast -> 
            match !blockType with
            | IfStatement -> 
                let condToksGraphs = extractNodesFromAST ast

                condToksGraphs
                |> Array.map 
                    (
                        fun graph -> 
                            graph.CollectAllTokens()
                            |> List.concat
                            |> List.map intToToken
                    )
                |> Array.iter(Simple >> condGraph.AddEdge)
                blockType := ThenStatement
                                        
            | ThenStatement -> 
                processSeq ast thenGraph
                blockType := ElseStatement

            | ElseStatement -> 
                let elseGraph = new GraphConstructor<_>()
                processSeq ast elseGraph
                elseGraphOpt := Some <| elseGraph.Graph

            | x -> failwithf "Unexpected type in 'if' construction: %s" <| x.ToString()
                                    
        | x -> failwithf "Unexpected AST node type: %s" <| x.ToString()

    family.nodes.doForAll processNode
    
    let ifGraph = createIfGraph condGraph.Graph thenGraph.Graph !elseGraphOpt
    Complicated (IfStatement, ifGraph)