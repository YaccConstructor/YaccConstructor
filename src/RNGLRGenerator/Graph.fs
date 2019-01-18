//   Copyright 2013, 2014 YaccConstructor Software Foundation
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

namespace YC.Generators.RNGLR.Generator.Graph

[<AllowNullLiteral>]
type Vertex<'VertexLabel, 'EdgeLabel> (label : 'VertexLabel) =
    let out = new ResizeArray<Edge<'VertexLabel, 'EdgeLabel>>(4)
    let mutable _label = label
    member this.label = _label
    member this.addEdge edge = out.Add edge
    member this.outEdges = out
    member this.setLabel newLabel = _label <- newLabel

and Edge<'VertexLabel, 'EdgeLabel> (destination : Vertex<'VertexLabel, 'EdgeLabel>, label : 'EdgeLabel) =
    let mutable _label = label
    member this.dest = destination
    member this.label = _label
    member this.setLabel newLabel = _label <- newLabel


