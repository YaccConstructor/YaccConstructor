namespace Yard.Generators.GLL

open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.RNGLR.States
open Yard.Generators.RNGLR



type Table (grammar : FinalGrammar) =
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
        
        let arr = Array2D.create grammar.indexator.fullCount grammar.indexator.fullCount (List.empty<int>)
        let result = Array.create ((Array2D.length1 arr)*(Array2D.length2 arr)) Array.empty<int>
        for i = 0 to grammar.rules.rulesCount-1 do
            let curLeftSide = grammar.rules.leftSide i
            let curRigrhtSide = grammar.rules.rightSide i
            let curFirst = Set.toArray first.[i]
            for j = 0 to curFirst.Length-1 do
                printfn "%d THISS ADDD" i
                i::arr.[curLeftSide,curFirst.[j]] |> ignore
            if canInferEpsilon.[i] then 
                let curFollow = Set.toArray follow.[curLeftSide]
                for j = 0 to curFollow.Length-1 do
                    i::arr.[curLeftSide,curFollow.[j]] |> ignore
                if Set.contains grammar.indexator.eofIndex follow.[curLeftSide] then 
                    printfn "uaaa"
        for i = 0 to grammar.indexator.fullCount-1 do
            for j = 0 to grammar.indexator.fullCount-1 do
                result.[(j+i*(grammar.indexator.fullCount))] <- arr.[i,j] |> List.toArray 

        result
  
    member this.result = _table




    

    
            
   