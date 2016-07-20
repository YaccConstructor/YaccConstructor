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
/// <param name="tokenToString">Token to string mapping</param>
/// <param name="tempDict">Key is non-terminal number, value is block type of cfg</param>
/// <param name="family">AST node that would be processed</param>
/// <param name="processSeq">This function will be  then и else branches</param>
let processIf (intToToken : int -> 'TokenType) tokenToNumber (tokenToString : 'TokenType -> string) (tempDict : Dictionary<int, _>) processSeq (family : Family) = 
    let blockType = ref IfStatement
                        
    let condGraph = new BlocksGraphBuilder<_>()
    let thenGraph = new BlocksGraphBuilder<_>()
    let elseGraphOpt = ref None

    let processNode (node : obj) = 
        match node with
        | :? Epsilon -> ()
        | :? Terminal as t -> 
            let terminalNum =  t.TokenNumber |> intToToken |> tokenToNumber
            match tempDict.TryGetValue terminalNum with
            | true, b -> blockType := b
            | false, _ -> ()

        | :? AST as ast -> 
            match !blockType with
            | IfStatement -> 
                let condToksGraphs = extractNodesFromAST intToToken tokenToString ast
                condToksGraphs
                |> Array.iter(fun graph -> condGraph.AddEdge <| Simple(Condition, graph))
                blockType := ThenStatement
                                        
            | ThenStatement -> 
                processSeq thenGraph ast 
                blockType := ElseStatement

            | ElseStatement -> 
                let elseGraph = new BlocksGraphBuilder<_>()
                processSeq elseGraph ast 
                elseGraphOpt := Some <| elseGraph.Build()

            | x -> failwithf "Unexpected type in 'if' construction: %A" x
                                    
        | x -> failwithf "Unexpected AST node type: %A" x

    family.nodes.doForAll processNode
    
    let ifGraph = createIfGraph 
                <| condGraph.Build() 
                <| thenGraph.Build() 
                <| !elseGraphOpt
    Complicated(IfStatement, ifGraph)