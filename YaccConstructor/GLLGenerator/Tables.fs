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