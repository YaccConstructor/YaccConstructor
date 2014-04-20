namespace Yard.Generators.GLL

open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.RNGLR.States
open Yard.Generators.RNGLR



type Table (grammar : FinalGrammar) =
    //let nonTermToIndex nTrm = 
    //let termToIndex =     
    let getFollowSets  =  
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

//    let getFirstSets =
//        let result = Array.create grammar.rules.rulesCount Set.empty
//        for i = 0 to grammar.rules.rulesCount - 1 do
//            let rightSide = grammar.rules.rightSide i
//            let mutable condition = true
//            let mutable j = 0
//            let mutable curFirst = result.[i]
//            while condition do
//            if j <= Array.length rightSide - 1 then
//                if grammar.canInferEpsilon.[rightSide.[j]] then 
//                    curFirst <- Set.union curFirst (grammar.firstSet.[rightSide.[j]])
//                    j <- j + 1 
//                else 
//                    let temp = grammar.firstSet.[rightSide.[j]]
//                    curFirst <- Set.union curFirst grammar.firstSet.[rightSide.[j]]
//                    condition <- false 
//                    result.[i] <- curFirst           
//            else 
//                condition <- false
//                result.[i] <- curFirst
//        result 
//
//    let first = getFirstSets
//    let chainCanInferEpsilon = 
//        let canInferEpsilon = Array.create grammar.rules.rulesCount true
//        let mutable canInfer = true
//        for i = 0 to grammar.rules.rulesCount-1 do
//            let curFirst = Set.toArray first.[i] 
//            for j = 0 to curFirst.Length-1 do
//                if not grammar.canInferEpsilon.[curFirst.[j]] then canInfer <- false
//            canInferEpsilon.[i] <- canInfer
//        canInferEpsilon

    let follow = getFollowSets
    //let canInferEpsilon = chainCanInferEpsilon
    let _table = 
        let length = grammar.indexator.fullCount
        let arr = Array2D.create length length (List.empty<int>)
        let result = Array.create (length*length) (Array.empty<int>)
        for i = 0 to grammar.rules.rulesCount-1 do
            let curFirst = Set.toArray grammar.firstSet.[i]
            for j = 0 to curFirst.Length-1 do
                arr.[grammar.rules.leftSide i,curFirst.[j]] <- i::arr.[grammar.rules.leftSide i,curFirst.[j]]
            if grammar.canInferEpsilon.[i] then 
                let curFollow = Set.toArray follow.[grammar.rules.leftSide i]
                for j = 0 to curFollow.Length-1 do
                    arr.[grammar.rules.leftSide i,curFollow.[j]] <- i::arr.[grammar.rules.leftSide i,curFollow.[j]]            
           // if Set.contains grammar.indexator.eofIndex follow.[grammar.rules.leftSide i] then 
             //   arr.[grammar.startRule,grammar.indexator.eofIndex] <- grammar.startRule :: arr.[grammar.startRule,grammar.indexator.eofIndex]

        //tr
        //trttr
        for i = 0 to length - 1 do
            for j = 0 to length - 1 do
                result.[length*i + j] <- List.toArray arr.[i,j]
        result
  
    member this.result = _table


       // if Set.contains grammar.indexator.eofIndex follow.[grammar.rules.leftSide i] then 
               //     arr.[grammar.startRule,grammar.indexator.eofIndex] <- grammar.startRule :: arr.[grammar.startRule,grammar.indexator.eofIndex]



    

    
            
   