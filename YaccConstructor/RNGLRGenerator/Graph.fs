//  Graph.fs contains generic graph representation
//
//  Copyright 2011-2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace Yard.Generators.RNGLR

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


