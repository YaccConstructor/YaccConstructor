namespace Yard.Generators.GLL

open Yard.Generators.Common.FinalGrammar
open Yard.Generators.Common

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
                let currentruleLeft = grammar.rules.leftSide ruleIndex
                let currentruleRight = grammar.rules.rightSide ruleIndex
                let mutable previousNonTerms = []
                for symbolIndex in 0..(grammar.rules.length ruleIndex)-1 do
                    let currentSymbol = currentruleRight.[symbolIndex]
                    List.iter (addElementsToSet grammar.firstSet.[currentSymbol]) previousNonTerms
                    if not grammar.canInferEpsilon.[currentSymbol]
                    then previousNonTerms <- []
                    if grammar.indexator.isNonTerm currentSymbol
                    then previousNonTerms <- currentSymbol::previousNonTerms
                List.iter (addElementsToSet followSets.[currentruleLeft]) previousNonTerms
        followSets

    let newRightSideForStartRule () =
        let oldRightSide = grammar.rules.rightSide grammar.rules.startRule 
        let newRightSide = Array.zeroCreate<int> (oldRightSide.Length + 1)
        for i = 0 to oldRightSide.Length do
            if i <> oldRightSide.Length
            then
                newRightSide.[i] <- oldRightSide.[i]
            else
                newRightSide.[i] <- grammar.indexator.eofIndex
        //grammar.rules.
        grammar.rules.setRightSide grammar.rules.startRule newRightSide
    
    let follow = getFollowSets

    let canInferEpsilon = Array.create grammar.rules.rulesCount false

    //
    let firstForChain = 
        let ruleCount = grammar.rules.rulesCount
        let result = Array.create ruleCount Set.empty<int>
        //let mutable condition = true
        for i = 0 to ruleCount - 1 do
            let mutable condition = true
            let currule = grammar.rules.rightSide i
            let mutable j = 0
            while condition do
                if j <= currule.Length - 1
                then
                    let curFirst = grammar.firstSet.[currule.[j]]
                    result.[i] <- Set.union result.[i] curFirst
                    if grammar.canInferEpsilon.[currule.[j]]
                    then 
                        if j < currule.Length - 1
                        then j <- j + 1
                        else 
                            condition <- false
                            canInferEpsilon.[i] <- true
                    else condition <- false
                else condition <- false
        result

    let getTableIndex num =
        let result = num - grammar.indexator.nonTermCount
        result

    let addToDictionary (dict:System.Collections.Generic.Dictionary<int,ResizeArray<int>>) key value = 
        if dict.ContainsKey(key)
        then
            dict.[key].Add value
        else
            let ra = new ResizeArray<int>()
            ra.Add value
            dict.Add(key, ra)
            
    let _table = 
        let inline pack left right : int =  int((int32 left <<< 16) ||| int32 right)
        newRightSideForStartRule()
        let result = new System.Collections.Generic.Dictionary<int, ResizeArray<int>>()
        //let length1 = grammar.indexator.nonTermCount
        //let length2 = grammar.indexator.fullCount - grammar.indexator.nonTermCount
        let firsts = firstForChain
        for i = 0 to grammar.rules.rulesCount - 1 do
            let curFirst = Set.toArray firsts.[i]
            let curNTerm = grammar.rules.leftSide i
            for j = 0 to curFirst.Length - 1 do
                let key = pack curNTerm (getTableIndex curFirst.[j])
                addToDictionary result key i
            if grammar.epsilonRules.[i]
            then 
                let curFollow = Set.toArray follow.[curNTerm]
                for j = 0 to curFollow.Length - 1 do
                    let key = pack curNTerm (getTableIndex curFollow.[j])
                    addToDictionary result key i
        
        result

    member this.result = _table
    