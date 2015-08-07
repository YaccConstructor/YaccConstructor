module ControlFlowGraph.TokensExtractor

open System.Collections.Generic

open QuickGraph

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode

type TokensEdge(source, target, tag) = 
    inherit TaggedEdge<int, int option>(source, target, tag)

type CfgTokensGraph() =
    inherit AdjacencyGraph<int, TokensEdge>()
    
    member this.StartVertex = 0

    member this.AddEdgeForced (e : TokensEdge) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    member this.CollectAllTokens() = 
        
        let vertexToTokens = Dictionary<_, _>()
        let expected = Queue<_>()
        expected.Enqueue this.StartVertex

        let endVertex = this.VertexCount - 1

        let handleEdge (edge : TokensEdge) data = 
            let newData = 
                data
                |> List.map 
                        (
                            fun tokList -> 
                                if edge.Tag.IsSome
                                then tokList |> List.append [edge.Tag.Value]
                                else tokList
                        )
            
            let newVertex = edge.Target
            
            if vertexToTokens.ContainsKey(newVertex)
            then
                let oldData = vertexToTokens.[newVertex]
                vertexToTokens.[newVertex] <- List.append oldData newData
            else
                vertexToTokens.[newVertex] <- newData
                expected.Enqueue newVertex
            
        let handleVertex vertex = 
            
            let data = 
                if not <| vertexToTokens.ContainsKey vertex 
                then 
                    vertexToTokens.[vertex] <-  [[]]
                vertexToTokens.[vertex]
            
            this.OutEdges vertex
            |> Seq.iter(fun edge -> handleEdge edge data)

        while expected.Count > 0 do
            let vertex = expected.Dequeue()
            handleVertex vertex

        vertexToTokens.[endVertex]
        |> List.map (fun set -> set |> List.rev)

///takes family and builds graph
///each edge is labeled some token
///if family doesn't contain ambiguous then graph'll be linear.
///otherwise it'll contain branches.
let extractNodesFromFamily (fam : Family) = 
    let tokensGraph = new CfgTokensGraph()
    let tStartVertex = ref tokensGraph.StartVertex
    let tEndVertex = ref <| tokensGraph.StartVertex + 1
            
    let rec collectTokens (node : obj) startVertex endVertex = 
            
        match node with 
        | :? Terminal as t -> 
            let edge = new TokensEdge(!startVertex, !endVertex, Some <| t.TokenNumber)
            tokensGraph.AddEdgeForced edge 

            startVertex := !endVertex
            incr endVertex

        | :? AST as ast -> 

            let commonStartVertex = !startVertex
            let newEndVertex = processFamily ast.first (ref commonStartVertex) endVertex
                        
            if ast.other <> null 
            then 
                let allEndVertex = 
                    ast.other
                    |> Array.map (fun fam -> processFamily fam (ref commonStartVertex) endVertex)
                    |> List.ofArray
                    |> List.append [newEndVertex]

                let commonEndVertex = !endVertex

                allEndVertex
                |> List.iter 
                        (
                            fun num -> 
                                let edge = TokensEdge(num, commonEndVertex, None)
                                tokensGraph.AddEdgeForced edge
                        )
                startVertex := !endVertex
                incr endVertex
            else
                startVertex := newEndVertex

        | _ -> failwith "Unexpected AST node type in Control-Flow construction"

    and processFamily (fam : Family) startVertex endVertex = 
            
        fam.nodes.doForAll (fun node -> collectTokens node startVertex endVertex)
        !endVertex - 1 //current it because of algorithm realization. Ideally, it should be !endVertex
            
    fam.nodes.doForAll(fun node -> collectTokens node tStartVertex tEndVertex)
    tokensGraph

let extractNodesFromAST (ast : AST)= 
    if ast.other = null
    then
        [|extractNodesFromFamily ast.first|]
    else
        ast.other
        |> Array.map (fun fam -> extractNodesFromFamily fam)
        |> Array.append [| extractNodesFromFamily ast.first |] 