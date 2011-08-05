//  FA.fs contains functions for automata building
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

module Yard.Generators.GNESCCGenerator.FAL.FA

open QuickGraph
open QuickGraph.Algorithms
open System.Linq
open Yard.Generators.GNESCCGenerator

type AtmSymbol<'symbol> =
    | Epsilon
    | Dummy
    | AtmSymbol of 'symbol

type FA<'vertex, 'label> () =
    class
        inherit BidirectionalGraph<'vertex,TaggedEdge<'vertex,'label>>()        
        let mutable start: Option<'vertex> = None
        let mutable finale: List<'vertex> = []
        member self.Start
            with get() = start
            and  set value = start <- value
        member self.Finale
            with get() = finale
            and  set value = finale <- value
    end

type EdgeType = In|Out

let atmWolker (atm:FA<_,_>) edgesFilter s =
    let visited = ref []   
    let rec inner s =
        if List.exists ((=)s) !visited |> not
        then
            visited := s::!visited            
            atm.OutEdges(s) |> Seq.filter edgesFilter 
            |> fun x -> if x.Count() = 0 
                        then Seq.singleton (Seq.singleton (s,[]))
                        else Seq.map (fun (e:TaggedEdge<_,_>) -> inner e.Target) x
                             |> Seq.map2 (fun (e:TaggedEdge<_,_>) r -> Seq.map (fun r-> fst r, (snd e.Tag)@ (snd r) ) r) x
            |> Seq.concat
        else Seq.singleton (s,[])
    let res = Seq.map inner s
    res |> List.ofSeq |> List.map List.ofSeq , visited

let revert (g:FA<_,_>) = 
    let revEdges = 
        g.Edges
        |> Seq.map
            (fun e -> new TaggedEdge<_,_>(e.Target,e.Source,e.Tag))
        |> List.ofSeq
    g.Clear()
    g.AddVerticesAndEdgeRange revEdges


let eCls (g:FA<_,_>)  epsilon =
    let newStateEnumerator = new Enumerator()
    let newStateMap = new System.Collections.Generic.Dictionary<_,_>()
    let symbols = g.Edges |> Seq.map (fun e -> fst e.Tag) |> Set.ofSeq |> Set.filter ((<>)epsilon)
    let tran sttSet smb = 
        sttSet 
        |> Seq.map (fun s -> g.OutEdges(s))
        |> Seq.concat 
        |> Seq.filter (fun (e:TaggedEdge<_,_>) -> fst e.Tag = smb)        
        |> Seq.map (fun e -> e.Source, snd e.Tag ,e.Target)
        |> Seq.toList
        |> List.unzip3

    let filter = fun (e:TaggedEdge<_,_>) -> fst e.Tag = epsilon
    let trs,s = atmWolker g filter [g.Start.Value]    
    let newFA = new FA<_,_>()
    let traceCollection = ref Seq.empty
    let visited = new System.Collections.Generic.Dictionary<_,_>()
    let traceInfo = new System.Collections.Generic.Dictionary<_,_>()
    let formatTraceInfo trs =
        Seq.concat trs
        |> Seq. map (fun (x,y) -> x, List.concat y)

    let s' = List.sort !s
    visited.Add(s',false)
    traceInfo.Add(s', formatTraceInfo trs)    
    newStateMap.Add(s',newStateEnumerator.Next())
    while visited.ContainsValue(false) do
        let T = visited.First(fun kvp -> not kvp.Value).Key        
        visited.[T] <- true
        symbols 
        |> Seq.iter 
            (fun smb -> 
                let src,trTrs,tran = tran T smb
                let trs,U = atmWolker g filter tran
                if List.isEmpty !U |> not
                then
                    let U' = List.sort !U
                    let cFun = fun k -> k = U'
                    let cond = Seq.exists cFun visited.Keys
                    if cond  |> not
                    then 
                        visited.Add(U',false)
                        traceInfo.Add(U', formatTraceInfo trs)
                    let U' = 
                        if cond 
                        then Seq.find cFun visited.Keys
                        else U'
                    let trace = 
                        traceInfo.[T]
                        |> Seq.filter (fun (x,y) -> List.exists ((=)x) src)
                        |> Seq.toList
                        |> List.unzip
                        |> snd
                    let getOrAdd t =
                        let f,v = newStateMap.TryGetValue(t)
                        if f 
                        then v 
                        else 
                            let v = newStateEnumerator.Next()
                            newStateMap.Add(t,v)
                            v                    
                    new TaggedEdge<_,_>
                            (getOrAdd T
                            ,getOrAdd U'
                            ,(smb, List.concat trTrs
                                   |> if List.isEmpty trTrs 
                                      then fun _ -> [trace] 
                                      else 
                                        List.map 
                                            (fun t -> 
                                                if List.isEmpty trace 
                                                then [t] 
                                                else List.map (fun x -> x@t) trace)
                                   |> List.concat))
                    |> newFA.AddVerticesAndEdge 
                    |> ignore)    
    newFA