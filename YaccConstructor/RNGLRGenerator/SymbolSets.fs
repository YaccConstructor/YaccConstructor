//  SymbolSets.fs contains computation of First set for all Elements
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

module Yard.Generators.RNGLR.SymbolSets

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.Epsilon

let firstSet (rules : NumberedRules) (indexator : Indexator) (canInferEpsilon : bool[]) =
    let result : Set<int>[] = Array.create indexator.fullCount Set.empty   
      
    let calc term = 
    //for term = indexator.nonTermCount to indexator.fullCount-1 do
        let was : bool[] = Array.zeroCreate indexator.fullCount
        let rec dfs u = 
            result.[u] <- result.[u].Add term
            was.[u] <- true
            let rec check (prod : int[]) i =
                if (i = prod.Length) then false
                elif (prod.[i] = u) then true
                elif (not canInferEpsilon.[prod.[i]]) then false
                else check prod (i+1)
            for i = 0 to rules.rulesCount-1 do
                let v = rules.leftSide i
                if (not was.[v]) then
                    if (check (rules.rightSide i) 0) then
                      dfs v
        dfs term

    calc indexator.errorIndex
    for term = indexator.nonTermCount to indexator.fullCount-1 do
        calc term
    result

let followSet (rules : NumberedRules) (indexator : Indexator) (canInferEpsilon : bool[]) (firstSet : Set<int>[]) =
    let result : Set<int>[][] = Array.zeroCreate rules.rulesCount
    
    for i = 0 to rules.rulesCount-1 do
        result.[i] <- Array.create (rules.length i) Set.empty
        let mutable curSet : Set<int> = Set.empty
        for j = rules.length i - 1 downto 0 do
            result.[i].[j] <- curSet
            let value = rules.symbol i j
            if (j <> 0) then
                if (not canInferEpsilon.[value]) then
                    curSet <- firstSet.[value]
                else
                    curSet <- Set.union curSet firstSet.[value]
    result
