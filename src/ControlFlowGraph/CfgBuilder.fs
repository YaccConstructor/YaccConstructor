module ControlFlowGraph.CfgBuilder

open System.Collections.Generic

open ControlFlowGraph.AssignmentHelper
open ControlFlowGraph.Common
open ControlFlowGraph.GraphInterpreter
open ControlFlowGraph.IfHelper
open ControlFlowGraph.InnerGraph
open ControlFlowGraph.TokensExtractor

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open InputStructures

let buildCfg (tree : Tree<'TokenType>) 
            (parserSource : CfgParserSource<'TokenType>) 
            (langSource : LanguageSource) 
            tokToString = 

    let intToToken = fun i -> tree.Tokens.[i]
    
    let processIf' = processIf intToToken parserSource.TokenToNumber tokToString <| langSource.GetTempIfDict()
    let processAssignment' = processAssignment intToToken tokToString 
    
    let familyToVertices = new Dictionary<_, ASTProcessingState>()

    //hack against duplicated epsilon edges
    let addEpsilonEdge = 
        let epsEdges = new ResizeArray<_>()
        fun (graph : GraphConstructor<_>) source target ->
            if source <> target 
            then 
                let epsilon = (source, target)
                if not <| epsEdges.Contains epsilon
                then
                    graph.AddEdgeFromTo EmptyEdge source target
                    epsEdges.Add epsilon


    let rec handleNode (node : obj) (graph : GraphConstructor<_>) = 
        let handleFamily (family : Family) startVertex = 
            graph.CurrentVertex <- startVertex
            if familyToVertices.ContainsKey family 
            then 
                match familyToVertices.[family] with
                | Processed (source, target) -> 
                    
                    addEpsilonEdge graph graph.CurrentVertex source
                    
                    graph.CurrentVertex <- target
                    graph.LastVertex <- target
                    Some target
                | InProgress start -> 
                    addEpsilonEdge graph startVertex start
                    match graph.TryFindLastVertex start with
                    | Some vertex ->  
                        familyToVertices.[family] <- Processed(start, vertex)
                        graph.CurrentVertex <- vertex
                        graph.LastVertex <- vertex
                        Some vertex
                    | None -> None
            else
                familyToVertices.[family] <- InProgress(graph.CurrentVertex)
                let familyName = parserSource.LeftSides.[family.prod] |> parserSource.NumToString

                if langSource.NodeToType.ContainsKey familyName 
                then 
                    let edge = 
                        match langSource.NodeToType.[familyName] with
                        | IfStatement -> processIf' family handleNode
                        | Assignment -> processAssignment' family
                        | x -> failwithf "This construction isn't supported now: %A" x
                    
                    graph.AddEdge edge
                    graph.UpdateVertex()
                    familyToVertices.[family] <- Processed(startVertex, graph.CurrentVertex)
                    Some graph.CurrentVertex
                else 
                    family.nodes.doForAll (fun node -> handleNode node graph)
                    familyToVertices.[family] <- Processed(startVertex, graph.CurrentVertex)
                    Some graph.CurrentVertex

        match node with 
        | :? Epsilon 
        | :? Terminal -> ()
        | :? AST as ast ->
            let commonStart = graph.CurrentVertex
            let endNumbers = 
                ast.map (fun family -> handleFamily family commonStart)
                |> Seq.distinct
                |> Seq.fold 
                    (
                        fun acc numOpt -> 
                            match numOpt with 
                            | Some num -> num :: acc
                            | None -> acc
                    ) []
                |> Array.ofSeq
                
            if endNumbers.Length = 1
            then 
                graph.UpdateVertex()
            elif endNumbers.Length > 1
            then
                let commonEndVertex = graph.CreateNewVertex()

                endNumbers
                |> Array.iter(fun num -> addEpsilonEdge graph num commonEndVertex)
                graph.UpdateVertex()
                    
        | x -> failwithf "Unexpected node type: %A" x

    let graphInfo = new GraphConstructor<_>()
    handleNode tree.Root graphInfo

    match graphInfo.TryFindLastVertex graphInfo.CurrentVertex with
    | Some _ -> ()
    | None -> graphInfo.AddEdge EmptyEdge

    graphToCfg graphInfo.Graph <| Some parserSource.TokenToString

