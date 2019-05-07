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

namespace YC.Parsing.RNGLR.Generator

open YC.Parsing.Common.FinalGrammar
open YC.Parsing.RNGLR
open States

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