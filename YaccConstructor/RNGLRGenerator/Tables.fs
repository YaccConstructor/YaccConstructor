//  Copyright 2011-2012 by Dmitry Avdyukhin
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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

open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.RNGLR.States
open Yard.Generators.RNGLR

type Tables (grammar : FinalGrammar, states : StatesInterpreter) =
    let _reduces, _gotos, _acc =
        let symbolCount = grammar.indexator.fullCount
        let reduces : list<int * int>[,] = Array2D.create states.count symbolCount []
        let gotos : int list[,] = Array2D.create states.count symbolCount []
        let mutable acc = []
        if grammar.canInferEpsilon.[grammar.rules.leftSide grammar.startRule] then acc <- (*startState*)0::acc
        let endRule = KernelInterpreter.toKernel (grammar.startRule, grammar.rules.length grammar.startRule)
        for i = 0 to states.count-1 do
            let vertex = states.vertex i
            for e in vertex.outEdges do
                let symbol = e.label
                gotos.[i, symbol] <- e.dest.label::gotos.[i, symbol]
                if gotos.[i, symbol].Length > 1 then
                    eprintfn "Several gotos form state %d on symbol %d: %A" i symbol gotos.[i, symbol]
            let kernels, lookaheads = states.kernels i, states.lookaheads i
            for j = 0 to kernels.Length - 1 do
                let k, la = kernels.[j], lookaheads.[j]
                let prod, pos = KernelInterpreter.unzip k
                if k = endRule then acc <- i::acc
                elif grammar.epsilonTailStart.[prod] <= pos then
                    for symbol in la do 
                        reduces.[i, symbol] <- (prod,pos)::reduces.[i, symbol]
        reduces, gotos, acc

    member this.reduces = _reduces
    member this.gotos = _gotos
    member this.acc = _acc

type AttendedReduce = {symbol : int; reduces: list<int*int>}
type ReduceValue = {value: list<int*int>}
type RelaxedTables(grammar : FinalGrammar, states : StatesInterpreter) = 
    inherit Tables(grammar, states)
    let newGotos = base.gotos
    let mutable newReduces = base.reduces
    let mutable _attendedPushes : list<int * int list>[] = Array.create states.count [] // first is symbol, list is array of pushes
    let fillReduces(reduce : list<int*int>, i:int) = 
        for j in grammar.indexator.termsStart..grammar.indexator.termsEnd do
            newReduces.[i,j] <- reduce
    let caclulateReduces(ar:list<int * int>[,]) =
        let rec walk i (ar:list<int * int>[,]) =
            let rec inspectRow i j (ar:list<int * int>[,])(lastReduce:Option<list<int*int>>) =
                match ar.[i,j], lastReduce with
                | value, opt when value <> List.Empty  && opt.IsSome && opt.Value = List.Empty
                    -> if j< grammar.indexator.termsEnd then
                        inspectRow i (j+1) ar (Some(ar.[i,j]))
                       else 
                        if (not lastReduce.Value.IsEmpty) then  fillReduces(lastReduce.Value, i)
                        if (i < newGotos.GetLength(0) - 1) then walk (i+1) ar
                | value, opt when value <> List.Empty && opt.IsSome && opt.Value <> List.Empty
                    -> fillReduces(lastReduce.Value, i);
                        if(i < newReduces.GetLength(0) - 1) then
                            walk (i+1) ar
                | value, opt when (opt.IsSome && value = opt.Value) || value = List.Empty
                    -> if j < grammar.indexator.termsEnd then
                        inspectRow i (j+1) ar lastReduce
                       else
                        if (not lastReduce.Value.IsEmpty) then fillReduces(lastReduce.Value, i)
                        if (i < newGotos.GetLength(0) - 1) then walk (i+1) ar
                | _ -> if (i < newReduces.GetLength(0) - 1) then
                        walk (i+1) ar
            inspectRow i grammar.indexator.termsStart ar (Some(List.Empty))
        walk 0 newReduces
    let result = caclulateReduces(newReduces)
    let traverseArray(ar:int list[,]) = 
        let rec walk i (ar:int list[,]) = 
            let zeroList=List.Empty
            let rec inspectRow i j (ar:int list[,])(lastShift:Option<int list>) = 
                match ar.[i,j], lastShift with
                | value, opt when value <> zeroList && opt.IsSome && opt.Value = zeroList
                    -> if j < grammar.indexator.termsEnd then 
                        inspectRow i (j+1) ar (Some(ar.[i,j])) // init lastShift, go to next elem in row
                       else
                        if (not lastShift.Value.IsEmpty) then _attendedPushes.[i] <- [j, lastShift.Value] 
                        if (i < newGotos.GetLength(0) - 1) then  walk (i+1) ar
                | value, opt when value <> List.Empty && opt.IsSome && opt.Value <> zeroList
                    -> _attendedPushes.[i] <- [j, lastShift.Value]; 
                        if (i < newGotos.GetLength(0) - 1 ) then
                            walk (i+1) ar // to next row
                       //else true
                | value, opt when (opt.IsSome && value = opt.Value) || value = zeroList
                    -> if j < grammar.indexator.termsEnd then
                        inspectRow i (j+1) ar lastShift // go to next elem in row
                       else
                        if (not lastShift.Value.IsEmpty) then _attendedPushes.[i] <- [j, lastShift.Value];
                        if (i < newGotos.GetLength(0) - 1) then walk (i+1) ar
                | _ -> if (i < newGotos.GetLength(0) - 1) then 
                        walk (i+1) ar// to next row if we can
                       //else true
            inspectRow i grammar.indexator.termsStart ar (Some(List.Empty)) // start row inspecting at first elem of row with no lastShift
        walk 0 newGotos // start walking on array at first row
    let result = traverseArray(newGotos)
    
    member this.attendedPushes = _attendedPushes
    member this.attendedReduces = newReduces

