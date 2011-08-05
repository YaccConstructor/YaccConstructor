//  TestFAs.fs contains FAs for tests.
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

module Yard.Generators.GNESCCGenerator.FAL.Test.FAs

open Yard.Generators.GNESCCGenerator.FAL
open QuickGraph

let seqFA =
    let f = new FA.FA<_,_>()
    f.AddVerticesAndEdgeRange(
        [new TaggedEdge<_,_>(0,1,(0,[[1]]))
        ;new TaggedEdge<_,_>(1,2,(1,[[2]]))
        ;new TaggedEdge<_,_>(2,3,(0,[[3]]))
        ;new TaggedEdge<_,_>(3,4,(2,[[4]]))
        ;new TaggedEdge<_,_>(4,5,(0,[[5]]))
        ;new TaggedEdge<_,_>(5,6,(4,[[6]]))
        ])
    |> ignore
    f.Start <- Some 0
    f

let altFA =
    let f = new FA.FA<_,_>()
    f.AddVerticesAndEdgeRange(
        [new TaggedEdge<_,_>(0,1,(0,[[1]]))
        ;new TaggedEdge<_,_>(1,2,(0,[[2]]))
        ;new TaggedEdge<_,_>(1,3,(0,[[3]]))
        ;new TaggedEdge<_,_>(3,4,(1,[[4]]))        
        ;new TaggedEdge<_,_>(4,5,(0,[[5]]))
        ;new TaggedEdge<_,_>(5,6,(0,[[6]]))
        ;new TaggedEdge<_,_>(6,7,(2,[[7]]))
        ;new TaggedEdge<_,_>(2,8,(3,[[8]]))
        ;new TaggedEdge<_,_>(8,5,(0,[[9]]))
        ])
    |> ignore
    f.Start <- Some 0
    f

let optFA = 
    let f = new FA.FA<_,_>()
    f.AddVerticesAndEdgeRange(
        [new TaggedEdge<_,_>(0,1,(0,[[1]]))
        ;new TaggedEdge<_,_>(1,5,(0,[[6]]))
        ;new TaggedEdge<_,_>(5,2,(1,[[2]]))
        ;new TaggedEdge<_,_>(1,2,(0,[[3]]))
        ;new TaggedEdge<_,_>(2,3,(0,[[4]]))
        ;new TaggedEdge<_,_>(3,4,(2,[[5]]))
        ])
    |> ignore
    f.Start <- Some 0
    f

let clsFA =
    let f = new FA.FA<_,_>()
    f.AddVerticesAndEdgeRange(
        [new TaggedEdge<_,_>(0,1,(0,[[1]]))
        ;new TaggedEdge<_,_>(1,2,(0,[[2]]))
        ;new TaggedEdge<_,_>(1,2,(1,[[3]]))
        ;new TaggedEdge<_,_>(2,1,(0,[[4]]))        
        ;new TaggedEdge<_,_>(2,3,(0,[[5]]))
        ;new TaggedEdge<_,_>(3,4,(2,[[6]]))
        ])
    |> ignore
    f.Start <- Some 0
    f