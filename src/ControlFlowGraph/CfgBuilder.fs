module ControlFlowGraph.CfgBuilder

open System.Collections.Generic

open ControlFlowGraph.AssignmentHelper
open ControlFlowGraph.Common
open ControlFlowGraph.GraphInterpreter
open ControlFlowGraph.IfHelper
open ControlFlowGraph.InnerGraph
open ControlFlowGraph.InputStructures
open ControlFlowGraph.TokensExtractor

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode

let buildCfg (tree : Tree<'TokenType>) 
            (parserSource : GeneratedStuffSource<'TokenType, 'BackReference>) 
            (langSource : LanguageSource) 
            tokToString = 

    let intToToken i = tree.Tokens.[i]

    let familyToBlockTypeOption (family : Family) = 
        let familyName = parserSource.LeftSides.[family.prod] |> parserSource.NumToString

        match langSource.NodeToType.TryGetValue familyName with
        | true, value -> Some value
        | false, _ -> None
    
    let processIf' = processIf intToToken parserSource.TokenToNumber tokToString <| langSource.GetTempIfDict()
    let processAssignment' = processAssignment familyToBlockTypeOption intToToken tokToString 
    let processExpression' = processExpression intToToken tokToString
    
    let familyToState = new Dictionary<_, ASTProcessingState>()

    //hack against duplicated epsilon edges
    let addEpsilonEdge = 
        let epsEdges = new ResizeArray<_>()
        fun (graph : BlocksGraphBuilder<_>) source target ->
            if source <> target 
            then 
                let epsilon = (source, target)
                if not <| epsEdges.Contains epsilon
                then
                    graph.AddEdgeFromTo EmptyEdge source target
                    epsEdges.Add epsilon

    let rec handleNode (graph : BlocksGraphBuilder<_>) (node : obj) = 
        let handleFamily startVertex (family : Family) =
            graph.CurrentVertex <- startVertex
            
            match familyToState.TryGetValue family with
            | true, state ->
            
                match state with
                | Processed (source, target) ->                     
                    addEpsilonEdge graph graph.CurrentVertex source
                    
                    graph.CurrentVertex <- target
                    graph.NextVertex <- target
                    Some target
                | InProgress start -> 
                    addEpsilonEdge graph startVertex start
                    
                    match graph.TryFindLastVertex start with
                    | Some vertex ->  
                        familyToState.[family] <- Processed(start, vertex)
                        graph.CurrentVertex <- vertex
                        graph.NextVertex <- vertex
                        Some vertex
                    | None -> None
            | false, _ -> 
                familyToState.[family] <- InProgress(graph.CurrentVertex)
                
                match familyToBlockTypeOption family with
                | Some blockType -> 
                    let edge = 
                        match blockType with
                        | IfStatement -> processIf' handleNode family
                        | Assignment -> processAssignment' family
                        | Expression -> processExpression' family
                        | x -> invalidOp <| sprintf "This construction isn't supported now: %A" x
                    
                    graph.AddEdge edge
                    graph.UpdateVertex()
                | None ->
                    family.nodes.doForAll (handleNode graph)
                
                familyToState.[family] <- Processed(startVertex, graph.CurrentVertex)
                Some graph.CurrentVertex

        match node with
        | :? Epsilon 
        | :? Terminal -> ()
        | :? AST as ast ->
            let commonStart = graph.CurrentVertex
            let endNumbers = 
                ast.map (handleFamily commonStart)
                |> Seq.distinct
                |> Array.ofSeq
                |> Array.filter Option.isSome 
                |> Array.map Option.get
                
            match Array.length endNumbers with
            | 0 -> ()
            | 1 -> graph.UpdateVertex()
            | _ -> 
                let commonEndVertex = graph.CreateNewVertex()

                endNumbers
                |> Array.iter(fun num -> addEpsilonEdge graph num commonEndVertex)
                graph.UpdateVertex()
                    
        | x -> failwithf "Unexpected node type: %A" x

    let graphInfo = new BlocksGraphBuilder<_>()
    handleNode graphInfo tree.Root 

    match graphInfo.TryFindLastVertex graphInfo.CurrentVertex with
    | Some _ -> ()
    | None -> graphInfo.AddEdge EmptyEdge

    graphToCfg <| graphInfo.Build() <| Some parserSource.TokenToString