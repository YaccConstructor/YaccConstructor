//  eClosureTests.fs contains tests for e-closure
//
//  Copyright 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.GNESCCGenerator.FAL.Test

open Yard.Generators.GNESCCGenerator.FAL
open NUnit.Framework
open QuickGraph

[<TestFixture>]
type ``GNESCC FAL eClosure tests`` () = 

#if DEBUG
    let printDedbugInfo (actual:BidirectionalGraph<_,_>) (expected:BidirectionalGraph<_,_>) = 
        printfn "actual:\n"
        Seq.iter (fun (x:TaggedEdge<_,_>) -> printfn "%A, %A, %A" x.Source x.Tag x.Target)  (actual.ToArrayAdjacencyGraph().Edges |> List.ofSeq)
        printfn "%A"actual.Vertices
        printfn "expected:\n"
        Seq.iter (fun (x:TaggedEdge<_,_>) -> printfn "%A, %A, %A" x.Source x.Tag x.Target) (expected.ToArrayAdjacencyGraph().Edges |> List.ofSeq)
        printfn "%A"expected.Vertices
#endif
    [<Test>]
    member test.``Seq closure test`` () =
        let actual = FA.eCls FAs.seqFA 0
        let expected = 
            let f = new FA.FA<_,_>()
            f.AddVerticesAndEdgeRange(
                [new TaggedEdge<_,_>(0,1,(1,[[1;2]]))
                ;new TaggedEdge<_,_>(1,2,(2,[[3;4]]))
                ;new TaggedEdge<_,_>(2,3,(4,[[5;6]]))
                ])
            |> ignore
            f
#if DEBUG 
        printDedbugInfo actual expected
#endif
        Assert.IsTrue(Helpers.atmsAreEqual actual expected)

    [<Test>]
    member test.``Opt closure test`` () =
        let actual = FA.eCls FAs.optFA 0
        let expected = 
            let f = new FA.FA<_,_>()
            f.AddVerticesAndEdgeRange(
                [new TaggedEdge<_,_>(0,1,(1,[[1;6;2]]))
                ;new TaggedEdge<_,_>(0,2,(2,[[1;3;4;5]]))
                ;new TaggedEdge<_,_>(1,2,(2,[[4;5]]))
                ])
            |> ignore
            f
#if DEBUG 
        printDedbugInfo actual expected
#endif
        Assert.IsTrue(Helpers.atmsAreEqual actual expected)

    [<Test>]
    member test.``Alt closure test`` () =
        let actual = FA.eCls FAs.altFA 0
        let expected = 
            let f = new FA.FA<_,_>()
            f.AddVerticesAndEdgeRange(
                [new TaggedEdge<_,_>(0,2,(3, [[1; 2; 8]]))
                ;new TaggedEdge<_,_>(0,1,(1, [[1; 3; 4]]))
                ;new TaggedEdge<_,_>(1,3,(2, [[5; 6; 7]]))
                ;new TaggedEdge<_,_>(2,3,(2, [[9; 6; 7]]))
                ])
            |> ignore
            f
#if DEBUG 
        printDedbugInfo actual expected
#endif
        Assert.IsTrue(Helpers.atmsAreEqual actual expected)

    [<Test>]
    member test.``Cls closure test`` () =
        let actual = FA.eCls FAs.clsFA 0
        let expected = 
            let f = new FA.FA<_,_>()
            f.AddVerticesAndEdgeRange(
                [new TaggedEdge<_,_>(0,1,(1, [[1; 2; 4; 3]]))
                ;new TaggedEdge<_,_>(0,2,(2, [[1; 2; 5; 6]]))
                ;new TaggedEdge<_,_>(1,1,(1, [[3]]))
                ;new TaggedEdge<_,_>(1,2,(2, [[5; 6;]]))
                ])
            |> ignore
            f
#if DEBUG 
        printDedbugInfo actual expected
#endif
        Assert.IsTrue(Helpers.atmsAreEqual actual expected)