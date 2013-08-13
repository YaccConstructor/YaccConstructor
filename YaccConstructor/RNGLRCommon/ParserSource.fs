//  Parser.fs contains type, describing information, written to file as result of generation
//     and used by Parser and Translator.
//
//  Copyright 2011-2012 Avdyukhin Dmitry
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

namespace Yard.Generators.RNGLR

type ParserSource<'TokenType> (gotos : int[][]
                               , reduces : (int * int)[][][]
                               , zeroReduces : int[][][]
                               , accStates : bool[]
                               , rules : int[]
                               , rulesStart : int[]
                               , leftSide : int[]
                               , startRule : int
                               , eofIndex : int
                               , tokenToNumber : 'TokenType -> int
                               , acceptEmptyInput : bool
                               , numToString : int -> string
                               , errorIndex : int
                               , errorRulesExists : bool
                               ) =
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
    member this.ErrorIndex = errorIndex
    member this.ErrorRulesExists = errorRulesExists