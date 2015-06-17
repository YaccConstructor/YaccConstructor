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

type ParserSourceEBNF<'TokenType> (gotos : (int * (int * int)) option [][] //goto consists of number of state where to go to and sets of productions to Stack/not to Stack
                               , reduces : int [][][]
                               , zeroReduces : int[][][]
                               , dfaList : int[][][]
                               , finiteStates : int[][]
                               , accStates : bool[]
                               , leftSide : int[]
                               , startRule : int
                               , eofIndex : int
                               , tokenToNumber : 'TokenType -> int
                               , acceptEmptyInput : bool
                               , numToString : int -> string
                               , errorIndex : int
                               (*, errorRulesExists : bool*)
                               ) =
   
    member this.Reduces = reduces
    member this.ZeroReduces = zeroReduces
    member this.Gotos = gotos
    member this.DfaList = dfaList
    member this.finiteStates = finiteStates
    member this.AccStates = accStates
    member this.LeftSide = leftSide
    member this.StartRule = startRule
    member this.EofIndex = eofIndex
    member this.TokenToNumber = tokenToNumber
    member this.AcceptEmptyInput = acceptEmptyInput
    member this.NumToString = numToString
    member this.ErrorIndex = errorIndex
    //member this.ErrorRulesExists = errorRulesExists
