//  RNGLRAbstractParserTests.fs contains tests for RNGLRAbstractParser project.
//
//  Copyright 2013 Semyon Grigorev <rsdpisuy@gmail.com>
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


module RNGLRAbstractParserTests

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../../../Tests/AbstractRNGLR/DOT"

[<TestFixture>]
type ``RNGLR abstract parser tests`` () =
    let path name = System.IO.Path.Combine(baseInputGraphsPath,name)
    [<Test>]
    member this.``Load graph test from DOT`` () =
        let g = loadGraphFromDOT(path "IFExists_lex.dot")
        Assert.AreEqual(g.Edges |> Seq.length, 29)
        Assert.AreEqual(g.Vertices |> Seq.length, 25)

    [<Test>]
    member this.``Load graph test from DOT to QuickGraph`` () =
        let g = loadGraphFromDOT(path "IFExists_lex.dot")
        let qGraph = new AdjacencyGraph<int, TaggedEdge<_,string>>()
        g.Edges 
        |> Seq.iter(
            fun e -> 
                let edg = e :?> DotEdge<string>
                qGraph.AddVertex(int edg.Source.Id) |> ignore
                qGraph.AddVertex(int edg.Destination.Id) |> ignore
                qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id,edg.Label)) |> ignore)
        Assert.AreEqual(qGraph.Edges |> Seq.length, 29)
        Assert.AreEqual(qGraph.Vertices |> Seq.length, 25)