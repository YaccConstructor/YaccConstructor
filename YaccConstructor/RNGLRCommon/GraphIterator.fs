module Yard.Generators.RNGLR.GraphIterator

open QuickGraph

type GraphEdgeEnumerator<'TokenType> (qGraph : AdjacencyGraph<int, TaggedEdge<_,'TokenType>>) = 
    
    member this.Counter 
        with get() = ref 0
        
    member this.Count = qGraph.EdgeCount
    member this.MoveNext() = 
        incr this.Counter
        not(!this.Counter >= this.Count)
    member this.Current = 
            let points = new ResizeArray<'TokenType> ()
            let mutable i = 0
            qGraph.Edges |> Seq.iter(
                fun e ->
                let edg = e :?> TaggedEdge<_, 'TokenType>
                points.Add(edg.Tag))
            points.[!this.Counter]
        