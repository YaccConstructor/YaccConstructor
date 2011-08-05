//  AtmBuilder.fs contains functions for automata building
//
//  Copyright 2010, 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.GNESCCGenerator.FAL

open Yard.Generators.GNESCCGenerator
open QuickGraph

type AtmBuilder(enumerator:Enumerator) =
    class
        let concat (lAtm:FA.FA<_,_>) (rAtm:FA.FA<_,_>) lbl = 
                lAtm.AddVerticesAndEdgeRange rAtm.Edges
                |> ignore
                new TaggedEdge<_,_>(lAtm.Finale.Head, rAtm.Start.Value, lbl)
                |> lAtm.AddVerticesAndEdge
                |> ignore
                lAtm.Finale <- rAtm.Finale
                lAtm
                
        let alt (atm1:FA.FA<_,_>) (atm2:FA.FA<_,_>) alt1Slbl alt1Elbl alt2Slbl alt2Elbl =
            let startStateID = enumerator.Next()
            let finaleStateID = enumerator.Next()
            atm1.AddVerticesAndEdgeRange atm2.Edges
            |> ignore
            atm1.AddVerticesAndEdgeRange(
                [new TaggedEdge<_,_>(startStateID,atm1.Start.Value,alt1Slbl)
                ;new TaggedEdge<_,_>(startStateID,atm2.Start.Value,alt2Slbl)
                ;new TaggedEdge<_,_>(atm1.Finale.Head,finaleStateID,alt1Elbl)
                ;new TaggedEdge<_,_>(atm2.Finale.Head,finaleStateID,alt2Slbl)])
            |> ignore
            atm1.Start <- Some startStateID            
            atm1.Finale <- [finaleStateID]
            atm1

        let cls omega (atm:FA.FA<_,_>) clsSlbl clsElbl =
            let startStateID = enumerator.Next()
            let finaleStateID = enumerator.Next()
            atm.AddVerticesAndEdgeRange(
                [new TaggedEdge<_,_>(atm.Start.Value,atm.Finale.Head,(omega))
                ;new TaggedEdge<_,_>(atm.Finale.Head,atm.Start.Value,(omega))
                ;new TaggedEdge<_,_>(startStateID,atm.Start.Value,clsSlbl)
                ;new TaggedEdge<_,_>(atm.Finale.Head,finaleStateID,clsElbl)])
            |> ignore
            atm.Start <- Some startStateID
            atm.Finale <- [finaleStateID]
            atm
            
        let opt omega (atm:FA.FA<_,_>) clsSlbl clsElbl =
            let startStateID = enumerator.Next()
            let finaleStateID = enumerator.Next()
            let mdStateID = enumerator.Next()
            atm.AddVerticesAndEdgeRange(
                [new TaggedEdge<_,_>(mdStateID,atm.Finale.Head,(omega))
                ;new TaggedEdge<_,_>(mdStateID,atm.Start.Value,(omega))
                ;new TaggedEdge<_,_>(startStateID,mdStateID,clsSlbl)
                ;new TaggedEdge<_,_>(atm.Finale.Head,finaleStateID,clsElbl)])
            |> ignore
            atm.Start <- Some startStateID
            atm.Finale <- [finaleStateID]
            atm

        let wrap (hSmb, hLbl) (tSmb, tLbl) (atm:FA.FA<_,_>) =
            let hStateID = enumerator.Next()            
            let tStateID = enumerator.Next()
            
            atm.AddVerticesAndEdgeRange(
                [new TaggedEdge<_,_>(hStateID,atm.Start.Value,(hSmb,hLbl))              
                ;new TaggedEdge<_,_>(atm.Finale.Head,tStateID,(tSmb,tLbl))])
            |> ignore

            atm.Start <- Some hStateID
            atm.Finale <- [tStateID]
            atm                    
                
        let trivial smb lbl =
            let startStateID = enumerator.Next()
            let finaleStateID = enumerator.Next()
            let atm = new FA.FA<_,_>()
            atm.AddVerticesAndEdge(new TaggedEdge<_,_>(startStateID,finaleStateID,(smb,lbl)))
            |> ignore
            atm.Start <- Some startStateID
            atm.Finale <- [finaleStateID]
            atm

        member self.Concat lAtm rAtm lbl = concat lAtm rAtm lbl
        member self.Alt atm1 atm2 alt1Slbl alt1Elbl alt2Slbl alt2Elbl = 
            alt atm1 atm2 alt1Slbl alt1Elbl alt2Slbl alt2Elbl
        member self.Cls omega  atm clsSlbl clsElbl = cls omega atm clsSlbl clsElbl
        member self.Opt omega atm clsSlbl clsElbl = opt omega atm clsSlbl clsElbl
        member self.Wrap tag1 tag2 atm = wrap tag1 tag2 atm        
        member self.Trivial smb lbl = trivial smb lbl
    end