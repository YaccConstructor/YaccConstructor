module YC.GLL.GSS

open Yard.Generators.GLL.ParserCommon

type GSSVertex (posInGrammar: int<positionInGrammar>, posInInput: int<positionInInput>) =    

    let setU = new System.Collections.Generic.Dictionary<int<positionInGrammar>, ResizeArray<int<positionInInput>>>()
    let setP = new ResizeArray<int<positionInInput>*uint16>() 
    
    override this.Equals y = 
        y :? GSSVertex 
        && this.PositionInGrammar = (y :?> GSSVertex).PositionInGrammar
        && this.PositionInInput = (y :?> GSSVertex).PositionInInput

    override this.GetHashCode() = hash (this.PositionInGrammar, this.PositionInInput)
    
    member this.U = setU
    member this.P = setP
    member this.PositionInInput = posInInput
    member this.PositionInGrammar = posInGrammar

    /// Checks for existing of context in SetU. If not adds it to SetU.
    member this.ContainsContext (inputIndex: int<positionInInput>) (state : int<positionInGrammar>) =
        let cond, current = setU.TryGetValue state
        if cond
        then 
            if current.Contains inputIndex
            then true
            else 
             current.Add inputIndex
             false
        else
            setU.Add(state, new ResizeArray<_>([inputIndex]))
            false

[<Struct>]
type GSSEdgeLbl =
    val StateToContinue: int<positionInGrammar>
    val LengthOfProcessedString: uint16
    new (stateToContinue, len) = {StateToContinue = stateToContinue; LengthOfProcessedString = len}

type GSS () =
    inherit QuickGraph.AdjacencyGraph<GSSVertex,QuickGraph.TaggedEdge<GSSVertex,GSSEdgeLbl>>(true)
    /// Checks for existing of edge in gss edges set. If not adds it to edges set.
    member this.ContainsEdge (startVertex:GSSVertex, endVertex:GSSVertex, stateToContinue : int<positionInGrammar>, len : uint16) =
        let mutable realStartVertex = if startVertex = endVertex then endVertex else startVertex
        let cond, edges = this.TryGetEdges(startVertex, endVertex)
        let exists = 
            cond 
            && 
             let edg = edges |> Seq.tryFind (fun e -> e.Tag.LengthOfProcessedString = len && e.Tag.StateToContinue = stateToContinue)
             if edg.IsSome then realStartVertex <- edg.Value.Source
             edg.IsSome
        if not exists
        then this.AddVerticesAndEdge(new QuickGraph.TaggedEdge<_,_>(realStartVertex, endVertex, new GSSEdgeLbl(stateToContinue, len))) |> ignore
        exists, realStartVertex

    member this.ToDot fileName =
        // Should use standart printing!!!
        //QuickGraph.Graphviz.GraphvizAlgorithm(this).Generate()        
        let toPrint = new ResizeArray<_>(["digraph G {\nnode [shape = circle]"])
        let edgs = new ResizeArray<_>()
        let nodes = new ResizeArray<_>()
    
        let getStrFromVertex (v : GSSVertex) = 
            let edgeOfInput = CommonFuns.getEdge v.PositionInInput
            let posOnEdgeOfInput = CommonFuns.getPosOnEdge v.PositionInInput
        
            sprintf "St:%i;Edg:%i;Pos:%i" v.PositionInGrammar edgeOfInput posOnEdgeOfInput

        for edge in this.Edges do
            let endName = getStrFromVertex edge.Target
            let startName = getStrFromVertex edge.Source
            let edgeName = sprintf "ContinueSt:%i,Len:%i" edge.Tag.StateToContinue edge.Tag.LengthOfProcessedString

            edgeName |> edgs.Add

            if nodes.Contains endName |> not then
                endName |> nodes.Add
                let nName = sprintf "%i[label=\"%s\"]" (nodes.Count-1) endName
                nName |> toPrint.Add

            if nodes.Contains startName |> not then
                startName |> nodes.Add
                let nName = sprintf "%i[label=\"%s\"]" (nodes.Count-1) startName
                nName |> toPrint.Add

            let startId = nodes.IndexOf startName
            let endId = nodes.IndexOf endName

            let edge = sprintf "%i -> %i [label=\"%s\",color=blue]; \n" startId endId edgeName

            toPrint.Add edge

        toPrint.Add "}"

        System.IO.File.WriteAllLines(fileName, toPrint)


