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

namespace Yard.Generators.RNGLR
open AbstractAnalysis.Common
type ParserSource<'TokenType> (gotos : int[][]
                               , reduces : (int * int)[][][]
                               , zeroReduces : int[][][]
                               , accStates : bool[]
                               , rules : int[]
                               , rulesStart : int[]
                               , leftSide : int[]
                               , startRule : int
                               , eofIndex : int<token>
                               , tokenToNumber : 'TokenType -> int<token>
                               , acceptEmptyInput : bool
                               , numToString : int -> string
                               , errorIndex : int<token>
                               , errorRulesExists : bool
                               , ?tokenData: 'TokenType -> obj
                               , ?createErrorToken: 'TokenType -> 'TokenType) =
    let length =
        let res = Array.zeroCreate <| (rulesStart.Length - 1)
        for i=0 to res.Length-1 do
            res.[i] <- rulesStart.[i+1] - rulesStart.[i]
        res
    let _rules = Array.zeroCreate length.Length
    do for i = 0 to length.Length-1 do
        _rules.[i] <- Array.zeroCreate length.[i]
        for j = 0 to length.[i]-1 do
            _rules.[i].[j] <- rules.[rulesStart.[i] + j]

    member this.Reduces = reduces
    member this.ZeroReduces = zeroReduces
    member this.Gotos = gotos
    member this.AccStates = accStates
    member this.Rules = _rules
    member this.RulesStart = rulesStart
    member this.Length = length
    member this.LeftSide = leftSide
    member this.StartRule = startRule
    member this.EofIndex = eofIndex
    member this.TokenToNumber = tokenToNumber
    member this.AcceptEmptyInput = acceptEmptyInput
    member this.NumToString = numToString
    member this.TokenData = tokenData
    member this.ErrorIndex = errorIndex
    member this.CreateErrorToken = createErrorToken.Value
    member this.ErrorRulesExists = errorRulesExists