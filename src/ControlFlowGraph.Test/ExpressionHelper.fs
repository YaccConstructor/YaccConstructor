module ControlFlowGraph.Test.ExpressionHelper

open ControlFlowGraph.CfgTokensGraph

let private getPrevEdges (graph : CfgTokensGraph<_>) vertex = 
    graph.Edges
    |> Seq.filter (fun edge -> edge.Target = vertex)
    |> List.ofSeq

let private getNextEdges (graph : CfgTokensGraph<_>) vertex = 
    graph.OutEdges vertex
    |> List.ofSeq

let private getTags (graph : CfgTokensGraph<_>) getEdges vertex = 
    let res = new ResizeArray<_>()    

    let rec handle acc processed expected = 
        match expected with 
        | [] -> acc
        | (head : TokensEdge<_>) :: tail -> 

            if processed |> List.exists ((=) head) 
            then 
                handle acc processed tail
            else
                match head.Tag with
                | Some token -> handle <| token :: acc <| head :: processed <| tail
                | None -> 
                    let newEdges = getEdges head.Target
                    handle acc (head :: processed) <| List.append expected newEdges

    handle [] [] <| getEdges vertex

let private findEdges tokenToNumber (graph : CfgTokensGraph<_>) tokenNumber = 
    let checkEdge (edge : TokensEdge<_>) = 
        match edge.Tag with
        | Some token -> 
            let num = tokenToNumber token 
            num = tokenNumber
        | None -> false
    
    graph.Edges
    |> Seq.filter checkEdge

let getOutTags tokenToNumber graph tag = 
    
    let vertices = 
        findEdges tokenToNumber graph tag
        |> Seq.map (fun edge -> edge.Target)

    let getNext' = getNextEdges graph

    vertices
    |> Seq.map (fun vertex -> getTags graph getNext' vertex)
    |> Seq.concat
    |> Seq.distinct
    |> Seq.map tokenToNumber

let getInTags tokenToNumber graph tag = 
    
    let vertices = 
        findEdges tokenToNumber graph tag
        |> Seq.map (fun edge -> edge.Source)

    let getPrev' = getPrevEdges graph

    vertices
    |> Seq.map (fun vertex -> getTags graph getPrev' vertex)
    |> Seq.concat
    |> Seq.distinct
    |> Seq.map tokenToNumber