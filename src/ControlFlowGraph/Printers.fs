module ControlFlowGraph.Printers

open System.IO
open System.Text

open QuickGraph

open ControlFlowGraph.CfgTokensGraph
open ControlFlowGraph.InnerGraph

let getClusterDotString clasterName body = 
    let strBuilder = new StringBuilder()

    let inline append str = 
        strBuilder.AppendLine str |> ignore

    append <| sprintf "subgraph %s{" clasterName
    append "node [style=filled, color=white]"
    append "style=filled"
    append "color=lightgrey"
    append body
    append "}"
    string strBuilder

type CfgTokensGraphPrinter private() = 
    
    static member GetDotString (graph : CfgTokensGraph<'TokenType>) tokenToString shift prefix = 
        let strBuilder = new StringBuilder()
        let getVertexName num = 
            sprintf "%s%d" prefix <| shift num
        
        graph.Edges
        |> Seq.iter 
            (
                fun edge -> 
                    let src = getVertexName edge.Source
                    let trg = getVertexName edge.Target
                    let lbl = 
                        match edge.Tag with
                        | Some token -> sprintf "%s" <| tokenToString token
                        | None -> "eps"
                    strBuilder.AppendLine (sprintf "%s -> %s [label=\"%s\"]" src trg lbl) |> ignore
            )
        string strBuilder

    static member ToDot (graph : CfgTokensGraph<'TokenType>) tokenToString (name : string) = 
        use out = new StreamWriter(name)
        out.WriteLine "digraph AST {"
        out.WriteLine "rankdir=LR"

        let body = CfgTokensGraphPrinter.GetDotString graph tokenToString id ""
        out.WriteLine body

        out.WriteLine "}"
        out.Close()

type InnerGraphPrinter private() = 
    
    /// <summary>
    /// Prints graph to .dot file. 
    /// </summary>
    /// <param name="name">Name of .dot file</param>
    /// <param name="tokenToStringOpt">Token to string mapping option</param>
    static member RelaxedPrintToDot (graph : CfgBlocksGraph<_>) (name : string) (tokenToStringOpt : _ option) =
        use out = new StreamWriter(name)
        out.WriteLine "digraph AST {"
        out.WriteLine "rankdir=LR"

        let clusterCount = ref -1
        let innerVertices = ref 0

        let printCluster (graph : CfgTokensGraph<_>, tok2Str : _ -> string) = 
            incr clusterCount
            let clasterName = sprintf "cluster%d" !clusterCount
            let shift = (+) !innerVertices

            let info = CfgTokensGraphPrinter.GetDotString graph tok2Str shift "_"

            let clusterString = getClusterDotString clasterName info
            out.WriteLine clusterString

            let innerStart = sprintf "_%d" <| shift graph.StartVertex
            let innerFinish = sprintf "_%d" <| shift (graph.VertexCount - 1)
            innerVertices := !innerVertices + graph.VertexCount
            clasterName, innerStart, innerFinish
            

        let printEdge (edge : BlockEdge<'TokenType>) = 
            let blockTypeStr = string edge.Tag
            
            match edge.Tag, tokenToStringOpt with
            | Simple (_, graph), Some tok2Str -> 
                let name, first, last =  printCluster (graph, tok2Str)
                let labelToCluster = sprintf "[label=\"%s\", lhead= %s]" blockTypeStr name
                out.WriteLine (sprintf "%d -> %s %s" edge.Source first labelToCluster)

                out.WriteLine (sprintf "%s -> %d [ltail = %s]" last edge.Target name)
            | _ -> 
                let label = sprintf "[label=\" %s \"]" blockTypeStr
                out.WriteLine (sprintf "%d -> %d %s" edge.Source edge.Target label)

        graph.Edges
        |> Seq.iter printEdge
        out.WriteLine("}")
        out.Close()

let getDotCluster (graph : AdjacencyGraph<_, _>) tokenToString shift prefix = 
    
    let startVertexName = sprintf "%s%d" prefix <| shift 0 
    let finishVertexName = sprintf "%s%d" prefix <| shift graph.VertexCount - 1

    let dotString = 
        match graph with
        | :? CfgTokensGraph<_> as tokensGraph -> 
            CfgTokensGraphPrinter.GetDotString tokensGraph tokenToString shift prefix
        //| :? CfgBlocksGraph<_> as blocksGraph -> 
          //  InnerGraphPrinter.
        | x -> failwithf "This graph type isn't supported now: %A" x

    dotString, startVertexName, finishVertexName