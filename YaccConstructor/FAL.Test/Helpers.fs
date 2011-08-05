//  Helpers.fs contains helpers for tests.
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

module Yard.Generators.GNESCCGenerator.FAL.Test.Helpers

open QuickGraph

let eAreEqual (e1:TaggedEdge<_,_>) (e2:TaggedEdge<_,_>) = 
    e1.Source = e2.Source
    && e1.Tag = e2.Tag
    && e1.Target = e2.Target

let atmsAreEqual (atm1:BidirectionalGraph<_,_>) (atm2:BidirectionalGraph<_,_>) =
    atm1.VertexCount = atm2.VertexCount
    && Seq.forall (fun e1 -> Seq.exists (fun e2 -> eAreEqual e1 e2)  (atm2.Edges)) (atm1.Edges)
