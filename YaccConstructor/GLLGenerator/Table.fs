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

type Table (_grammar : FinalGrammar) =
     let grammar = _grammar
     let table = 
        let _table = Array2D.create grammar.indexator.nonTermCount grammar.indexator.termCount 0
        for i = 0 to grammar.rules.rulesCount - 1 do
            let curFst = grammar.chainFirstSet.[i] //wat
            //let curFst = grammar.firstSet.[i]
            let curTerm = grammar.rules.leftSide i
            let curChain = grammar.rules.rightSide i
            for j = 0 to curFst.Count do
                _table.[]

        

     
       
    