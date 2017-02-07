module YC.GLL.GSS

open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.MeasureTypes
open QuickGraph
open QuickGraph.Graphviz

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
    inherit AdjacencyGraph<GSSVertex, TaggedEdge<GSSVertex, GSSEdgeLbl>>(true)
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
        let getStrFromVertex (v: GSSVertex) = 
            let edgeOfInput = CommonFuns.getEdge v.PositionInInput
            let posOnEdgeOfInput = CommonFuns.getPosOnEdge v.PositionInInput
            sprintf "St:%i;Edg:%i;Pos:%i" v.PositionInGrammar edgeOfInput posOnEdgeOfInput

        let printer = GraphvizAlgorithm(this)
        printer.CommonVertexFormat.Shape <- Dot.GraphvizVertexShape.Ellipse
        printer.FormatEdge.Add(fun (e:FormatEdgeEventArgs<_,_>) -> e.EdgeFormatter.Label.Value <- sprintf "ContSt:%i,Len:%i" e.Edge.Tag.StateToContinue e.Edge.Tag.LengthOfProcessedString)
        printer.FormatVertex.Add(fun (v:FormatVertexEventArgs<_>) -> v.VertexFormatter.Label <- getStrFromVertex v.Vertex)  
        let str = printer.Generate()        
            
        System.IO.File.WriteAllText(fileName, str)


