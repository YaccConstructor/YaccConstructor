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

namespace Yard.Generators.RNGLR.EBNF.DFA

open Yard.Generators.Common

type SymbolType =
    | Nonterminal
    | Terminal

type ParserSourceReadBack<'TokenType> (gotos : int [][]
                               , reduces : (int * int) [][][]
                               , zeroReduces : int[][][]
                               , accStates : bool[]
                               , dfas : DFATable
                               , leftSide : int[]
                               , startRule : int
                               , eofIndex : int
                               , tokenToNumber : 'TokenType -> int
                               , acceptEmptyInput : bool
                               , numToString : int -> string
                               , indexToSymbolType : int -> SymbolType
                               , symbolsCount : int
                               , errorIndex : int
                               (*, errorRulesExists : bool*)
                               ) =
   
    let _dfas, _finishStates =
        let dfaTables = Array.zeroCreate dfas.Length
        let finishStates = Array.zeroCreate dfas.Length
        for i = 0 to dfas.Length - 1 do
            let numberOfStates, allTransitions, finishState = dfas.[i]
            dfaTables.[i] <- Array.init numberOfStates (fun j -> Array.create symbolsCount None)
            finishStates.[i] <- finishState
            for state, transitions in allTransitions do                
                for symbol, dests in transitions do
                    dfaTables.[i].[state].[symbol] <- Some dests
        dfaTables, finishStates

    member this.Reduces = reduces
    member this.ZeroReduces = zeroReduces
    member this.Gotos = gotos
    member this.DfaTables = _dfas
    member this.DfaFinishStates = _finishStates
    member this.AccStates = accStates
    member this.LeftSide = leftSide
    member this.StartRule = startRule
    member this.EofIndex = eofIndex
    member this.TokenToNumber = tokenToNumber
    member this.AcceptEmptyInput = acceptEmptyInput
    member this.NumToString = numToString
    member this.IndexToSymbolType = indexToSymbolType
    member this.ErrorIndex = errorIndex
    //member this.ErrorRulesExists = errorRulesExists
