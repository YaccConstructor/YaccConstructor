module YC.GLL.GSS

open Yard.Generators.GLL.ParserCommon

type GSSVertex (posInGrammar: int<positionInGrammar>, posInInput: int<positionInInput>) =    

    let setU = new System.Collections.Generic.Dictionary<int<positionInGrammar>, int<positionInInput>>()
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
            true            
        else
            setU.Add(state, inputIndex)
            false

[<Struct>]
type GSSEdgeLbl =
    val StateToContinue: int<positionInGrammar>
    val LengthOfProcessedString: uint16
    new (stateToContinue, len) = {StateToContinue = stateToContinue; LengthOfProcessedString = len}

type GSS () =
    inherit QuickGraph.BidirectionalGraph<GSSVertex,QuickGraph.TaggedEdge<GSSVertex,GSSEdgeLbl>>(true)
    /// Checks for existing of edge in gss edges set. If not adds it to edges set.
    member this.ContainsEdge (startVertex:GSSVertex, endVertex:GSSVertex, stateToContinue : int<positionInGrammar>, len : uint16) =
        let mutable realStartVertex = startVertex
        let cond, edges = this.TryGetEdges(startVertex, endVertex)
        let exists = 
            cond 
            && 
             let edg = edges |> Seq.tryFind (fun e -> e.Tag.LengthOfProcessedString = len && e.Tag.StateToContinue = stateToContinue)
             if edg.IsSome then realStartVertex <- edg.Value.Source
             edg.IsSome
        if not exists
        then this.AddVerticesAndEdge(new QuickGraph.TaggedEdge<_,_>(startVertex,endVertex, new GSSEdgeLbl(stateToContinue, len))) |> ignore
        exists, realStartVertex
        

