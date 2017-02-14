module YC.GLL.GSS

open Yard.Generators.GLL.ParserCommon
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open QuickGraph
open QuickGraph.Graphviz
open System.Collections.Generic.Customized

[<Struct>]
type PoppedData =
    val posInInput: int<positionInInput>
    val data: ParseData

    new (pos, d) = {posInInput = pos; data = d}

[<Measure>] type compressedPosInInputAndGrammar

type GSSVertex (nonterm: int<positionInGrammar>, posInInput: int<positionInInput>) =    

    let setU = new System.Collections.Generic.Dictionary<int64<compressedPosInInputAndGrammar>,HashSet<ParseData>>()
    let setP = new ResizeArray<PoppedData>() 
    
    override this.Equals y =
        y :? GSSVertex 
        && (let y = y :?> GSSVertex 
            this.Nonterm = y.Nonterm
            && this.PositionInInput = y .PositionInInput)

    override this.GetHashCode() = hash (this.Nonterm, this.PositionInInput)
    
    member this.U = setU
    member this.P with get () = setP
    member this.AddP d = 
        setP.Add d
    member this.PositionInInput = posInInput
    member this.Nonterm = nonterm
    member this.GetUncompressetPositions (compressedPos: int64<compressedPosInInputAndGrammar>) =
        (CommonFuns.getLeft (int64 compressedPos)) * 1<positionInInput>
        , (CommonFuns.getRight (int64 compressedPos)) * 1<positionInGrammar>

    /// Checks for existing of context in SetU. If not adds it to SetU.
    member this.ContainsContext (posInInput: int<positionInInput>) (posInGrammar : int<positionInGrammar>) (newData : ParseData)=
        let compressedPositions =
            CommonFuns.pack posInInput posInGrammar 
            |> FSharp.Core.LanguagePrimitives.Int64WithMeasure  
        let cond, data = setU.TryGetValue compressedPositions
        if cond
        then not <| data.Add newData
        else 
            setU.Add(compressedPositions, new HashSet<_>([|newData|]))
            false
    override this.ToString () = sprintf "Nonterm: %i, Index: %i" this.Nonterm this.PositionInInput

[<Struct>]
type GSSEdgeLbl =
    val StateToContinue: int<positionInGrammar>
    val Data: ParseData
    new (stateToContinue, len) = {StateToContinue = stateToContinue; Data = len}

type GSS () =
    inherit AdjacencyGraph<GSSVertex, TaggedEdge<GSSVertex, GSSEdgeLbl>>(true)
    /// Checks for existing of edge in gss edges set. If not adds it to edges set.
    member this.ContainsVertAndEdge (startVertex:GSSVertex, endVertex:GSSVertex, stateToContinue : int<positionInGrammar>, data : ParseData) =
        let mutable realStartVertex = if startVertex = endVertex then endVertex else startVertex
        let vertexExists, outEdges = this.TryGetOutEdges startVertex
        let edges =
            if vertexExists then
                let edg = 
                    outEdges
                    |> Array.ofSeq
                if edg.Length <> 0
                then
                    realStartVertex <- edg.[0].Source
                edg
                |> Array.filter (fun e -> e.Target = endVertex && e.Tag.Data = data && e.Tag.StateToContinue = stateToContinue)                
            else [||]
        let edgeExists = edges.Length > 0
        if not <| edgeExists
        then 
            this.AddVerticesAndEdge(new QuickGraph.TaggedEdge<_,_>(realStartVertex, endVertex, new GSSEdgeLbl(stateToContinue, data))) |> ignore
        vertexExists, edgeExists, realStartVertex

    member this.ToDot fileName =
        let getStrFromVertex (v: GSSVertex) = 
            let edgeOfInput = CommonFuns.getEdge v.PositionInInput
            let posOnEdgeOfInput = CommonFuns.getPosOnEdge v.PositionInInput
            sprintf "St:%i;Edg:%i;Pos:%i" v.Nonterm edgeOfInput posOnEdgeOfInput

        let printer = GraphvizAlgorithm(this)
        printer.CommonVertexFormat.Shape <- Dot.GraphvizVertexShape.Ellipse
        printer.FormatEdge.Add(fun (e:FormatEdgeEventArgs<_,_>) -> e.EdgeFormatter.Label.Value <- sprintf "ContSt:%i,Data:%A" e.Edge.Tag.StateToContinue e.Edge.Tag.Data)
        printer.FormatVertex.Add(fun (v:FormatVertexEventArgs<_>) -> v.VertexFormatter.Label <- getStrFromVertex v.Vertex)  
        let str = printer.Generate()        
            
        System.IO.File.WriteAllText(fileName, str)


