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

module YC.Parsing.Common.SymbolSets


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
