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

namespace Yard.Generators.GLL

open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.RNGLR.States
open Yard.Generators.RNGLR



type Table (grammar : FinalGrammar) =
    let getFollowSets  =
        for i = 0 to grammar.rules.rulesCount-1 do
            printf "rules leftside %d ->" (grammar.rules.leftSide i)
            Array.iter (fun x -> printfn " %d" x) (grammar.rules.rightSide i)
        printf "\n"
        printf "RULES count %d \n" grammar.rules.rulesCount
        printf "FULL COUNT FOR INDEXATOR %d \n" grammar.indexator.fullCount
        printf "EOFINDEX FOR INDEXATOR %d \n" grammar.indexator.eofIndex
        printf "literals COUNT for indexator %d \n" grammar.indexator.literalsCount
        printf "literalStart FOR INDEXATOR %d \n" grammar.indexator.literalsStart
        printf "nonTermCount FOR INDEXATOR %d \n" grammar.indexator.nonTermCount
        printf "term COUNT FOR INDEXATOR %d \n" grammar.indexator.termCount
        printf "termStart FOR INDEXATOR %d \n" grammar.indexator.termsStart
        printf "termsEnd FOR INDEXATOR %d \n" grammar.indexator.termsEnd
        printf "CYYYYCLEEEEEEEE FOOOORR PRINT"
        for i = 0 to grammar.indexator.nonTermCount-1 do
            printf "index for non term = %d -> " i 
            printf "%s \n" (grammar.indexator.indexToNonTerm i)
        for i = grammar.indexator.termsStart to grammar.indexator.termsEnd do
            printf "index for term = %d -> " i 
            printf "%s \n" (grammar.indexator.indexToTerm i)
        printf "LIterals start %d \n" grammar.indexator.literalsStart
        printf "Literals end %d \n" grammar.indexator.literalsEnd
       
        let nonTermsCount = grammar.indexator.nonTermCount
        let followSets = Array.create nonTermsCount Set.empty 
        followSets.[grammar.rules.startSymbol] <- Set.ofList [grammar.indexator.eofIndex]
        let wasChange = ref true
        let addElementsToSet newElements setIndex =
            let oldSet = followSets.[setIndex]
            let newSet = oldSet + newElements
            followSets.[setIndex] <- newSet
            wasChange := !wasChange || oldSet <> newSet
        while !wasChange do
            wasChange := false
            for ruleIndex in 0..grammar.rules.rulesCount-1 do            
                let currentRuleLeft = grammar.rules.leftSide ruleIndex
                let currentRuleRight = grammar.rules.rightSide ruleIndex
                let mutable previousNonTerms = []
                for symbolIndex in 0..(grammar.rules.length ruleIndex)-1 do
                    let currentSymbol = currentRuleRight.[symbolIndex]
                    List.iter (addElementsToSet grammar.firstSet.[currentSymbol]) previousNonTerms
                    if not grammar.canInferEpsilon.[currentSymbol]
                    then previousNonTerms <- []
                    if grammar.indexator.isNonTerm currentSymbol
                    then previousNonTerms <- currentSymbol::previousNonTerms
                List.iter (addElementsToSet followSets.[currentRuleLeft]) previousNonTerms
        followSets 

    let getFirstSets =
        let firstSets = Array.create grammar.rules.rulesCount Set.empty
        for i = 0 to grammar.rules.rulesCount - 1 do
            let rightSide = grammar.rules.rightSide i
            let mutable curFirstSet : Set<int> = Set.empty
            let mutable condition = true
            let mutable j = 0
            while condition do
            if j< Array.length rightSide - 1 then
                if grammar.canInferEpsilon.[rightSide.[j]] then 
                    curFirstSet <- Set.union curFirstSet grammar.firstSet.[rightSide.[j]]
                    j <- j + 1 
                else 
                    curFirstSet <- Set.union curFirstSet grammar.firstSet.[rightSide.[j]]
                    firstSets.[i] <- curFirstSet
                    j <- Array.length rightSide - 1            
            else condition <- false
        firstSets 

    let first = getFirstSets
    let chainCanInferEpsilon = 
        let canInferEpsilon = Array.create grammar.rules.rulesCount true
        let mutable canInfer = true
        for i = 0 to grammar.rules.rulesCount-1 do
            let curFirst = Set.toArray first.[i] 
            for j = 0 to curFirst.Length-1 do
                if not grammar.canInferEpsilon.[curFirst.[j]] then canInfer <- false
            canInferEpsilon.[i] <- canInfer
        canInferEpsilon


    let follow = getFollowSets
    let canInferEpsilon = chainCanInferEpsilon
    let _table = 
        for k = 0 to Array.length follow - 1 do
            let arr = follow.[k] |> Set.toArray
            for l = 0 to Array.length arr - 1 do
                printfn "%d" arr.[l]

        printfn "DOOOOONE"
        
        let arr = Array2D.create grammar.indexator.fullCount grammar.indexator.fullCount -1
        let result = Array.zeroCreate ((Array2D.length1 arr)*(Array2D.length2 arr))
        for i = 0 to grammar.rules.rulesCount-1 do
            let curLeftSide = grammar.rules.leftSide i
            let curRigrhtSide = grammar.rules.rightSide i
            let curFirst = Set.toArray first.[i]
            for j = 0 to curFirst.Length-1 do
                arr.[curLeftSide,curFirst.[j]] <- i
            if canInferEpsilon.[i] then 
                let curFollow = Set.toArray follow.[curLeftSide]
                for j = 0 to curFollow.Length-1 do
                    arr.[curLeftSide,curFollow.[j]] <- i
                if Set.contains grammar.indexator.eofIndex follow.[curLeftSide] then 
                    printfn "uaaa"
        for i = 0 to grammar.indexator.fullCount-1 do
            for j = 0 to grammar.indexator.fullCount-1 do
                result.[(j+i*(grammar.indexator.fullCount))] <- arr.[i,j]

        result
  
    member this.result = _table




    

    
            
   