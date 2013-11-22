//   Copyright 2013 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

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
        