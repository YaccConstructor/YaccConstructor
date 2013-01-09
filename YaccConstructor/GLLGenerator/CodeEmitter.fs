// CodeEmitter.fs contains logic that emits GLL parser code to a specified stream

module Yard.Generators.GLL.CodeEmitter

open System.Text
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.FinalGrammar

// Calculate set of terminals that can be at a start of specific production.
// Set is calculate as FIRST for a sequence of tokens on rignt side of the production
// plus FOLLOW of the left side, if the production can infer epsilon
let private getStart (grammar:FinalGrammar) (followSets:Set<int>[]) ruleIndex =
    let rec _getStart acc = function
        | [] -> acc + followSets.[grammar.rules.leftSide ruleIndex]                
        | x::xs -> 
             let acc = acc + grammar.firstSet.[x]
             if grammar.canInferEpsilon.[x]
                then _getStart acc xs 
                else acc
    let items = grammar.rules.rightSide ruleIndex |> List.ofArray
    _getStart Set.empty items

// calcualte FOLLOW sets for all nonterminals in a grammar
let private getFollowSets (grammar:FinalGrammar) =
    let nonTermsCount = grammar.indexator.nonTermCount
    let followSets = Array.create nonTermsCount Set.empty 
    followSets.[grammar.rules.startSymbol] <- Set.ofList [grammar.indexator.eofIndex]
    let wasChange = ref true
    let addElementsToSet newElements setIndex =
        let oldSet = followSets.[setIndex]
        let newSet = oldSet + newElements
        followSets.[setIndex] <- newSet
        if oldSet <> newSet then wasChange := true
    while !wasChange do
        wasChange := false
        for ruleIndex in 0..grammar.rules.rulesCount-1 do            
            let currentRuleLeft = grammar.rules.leftSide ruleIndex
            let currentRuleRight = grammar.rules.rightSide ruleIndex
            let mutable previousNonTerms = []
            for symbolIndex in 0..(grammar.rules.length ruleIndex)-1 do
                let currentSymbol = currentRuleRight.[symbolIndex]
                List.iter (addElementsToSet grammar.firstSet.[currentSymbol]) previousNonTerms
                if not grammar.canInferEpsilon.[currentSymbol] then
                    previousNonTerms <- []
                if grammar.indexator.isNonTerm currentSymbol then
                    previousNonTerms <- currentSymbol::previousNonTerms
            List.iter (addElementsToSet followSets.[currentRuleLeft]) previousNonTerms
    followSets

let private emitLine shift text (out:StringBuilder) =
    out
      .Append(String.replicate (shift<<<2) " ")
      .AppendLine(text)
    |> ignore 

let private emitEmptyLine (out:StringBuilder) = out.AppendLine() |> ignore

let private getArrayRepresentation (arr : string seq) =
    "[| " + System.String.Join("; ", arr) + " |]"

let emitNameAndUsages moduleName (out:StringBuilder) =
    out |> emitLine 0 (sprintf "module %s" moduleName)
    out |> emitLine 0 "open Yard.Generators.GLL.Parser"    

let emitGrammar (grammar:FinalGrammar) (out:StringBuilder) =
    let followSets = getFollowSets grammar

    // emit grammar info
    out |> emitLine 0 "(*"
    seq { 0 .. (grammar.indexator.nonTermCount-1) }
    |> Seq.iter (fun i -> out |> emitLine 1 (sprintf "%d -> %s" i (grammar.indexator.indexToNonTerm i)))
    seq { grammar.indexator.termsStart .. grammar.indexator.termsEnd }
    |> Seq.iter (fun i -> out |> emitLine 1 (sprintf "%d -> %s" i (grammar.indexator.indexToTerm i)))
    seq { 0 .. (grammar.indexator.nonTermCount-1) }
    |> Seq.iter (fun i -> let followArray = getArrayRepresentation <| Seq.map grammar.indexator.indexToTerm followSets.[i]
                          out |> emitLine 1 (sprintf "FOLLOW(%s) = %s" (grammar.indexator.indexToNonTerm i) followArray))
    out |> emitLine 0 "*)"

    let functionNames = ref ["continueExecution"]
    let registerFunctionName name = functionNames := name :: !functionNames

    let inline getFunctionName nonTermIndex = 
        let nonTerm = grammar.indexator.indexToNonTerm nonTermIndex
        sprintf "parse_%s" nonTerm
    let getParseFunctionName emitRuleNum ruleIndex =
        let nonTerm = grammar.indexator.indexToNonTerm <| grammar.rules.leftSide ruleIndex
        if emitRuleNum
            then sprintf "parse_yard_rule%d_%s" ruleIndex nonTerm
            else sprintf "parse_%s" nonTerm
    let getParseContinuationName ruleIndex nonTermOccurence =
        let nonTerm = grammar.indexator.indexToNonTerm <| grammar.rules.leftSide ruleIndex
        sprintf "parse_yard_nonterm%d_rule%d_%s" nonTermOccurence ruleIndex nonTerm

    // first pass: register all function names 
    let preEmitSimpleParseFunction emitRuleNum ruleIndex =        
        registerFunctionName <| getParseFunctionName emitRuleNum ruleIndex
        grammar.rules.rightSide ruleIndex
        |> Seq.filter grammar.indexator.isNonTerm
        |> Seq.mapi (fun i _ -> getParseContinuationName ruleIndex i)
        |> Seq.iter registerFunctionName

    let preEmitParseFunction nonTermIndex =
        let rules = grammar.rules.rulesWithLeftSide nonTermIndex
        if rules.Length = 0 then ()
        elif rules.Length = 1 then preEmitSimpleParseFunction false rules.[0]
        else registerFunctionName <| getParseFunctionName false rules.[0]
             Array.iter (preEmitSimpleParseFunction true) rules        

    Seq.iter preEmitParseFunction {0..grammar.indexator.nonTermCount-1}
    let functionNames = !functionNames |> List.toArray |> Array.rev

    // second pass: emit function bodies and all
    let inline getFunctionIndex name =
        Array.findIndex ((=) name) functionNames
    let inline getFunctionReference name =
        sprintf "_parseFunctions.[%d]" <| getFunctionIndex name

    let emitSimpleParseFunction emitRuleNum ruleIndex =        
        out |> emitEmptyLine
        out |> emitLine 0 (sprintf "let private %s () =" <| getParseFunctionName emitRuleNum ruleIndex)
        let mutable lastNonTermOccurence = -1        
        for symbol in grammar.rules.rightSide ruleIndex do
            if grammar.indexator.isNonTerm symbol then
                lastNonTermOccurence <- lastNonTermOccurence + 1
                let functionName = getFunctionName symbol
                let continuationName = getParseContinuationName ruleIndex lastNonTermOccurence                
                out |> emitLine 1 (sprintf "push (%d,pos) // %s" (getFunctionIndex continuationName) continuationName)
                out |> emitLine 1 (sprintf "%s () // %s" (getFunctionReference functionName) functionName)
                out |> emitLine 0 (sprintf "let private %s () =" continuationName)                
            else
                out |> emitLine 1 (sprintf "if _tokens.[pos] <> %d then continueExecution () else" symbol)
                out |> emitLine 1 "pos <- pos + 1"
        out |> emitLine 1 "pop(); continueExecution()"
    
    let emitParseFunction nonTermIndex =
        let rules = grammar.rules.rulesWithLeftSide nonTermIndex
        if rules.Length = 0 then ()
        elif rules.Length = 1 then emitSimpleParseFunction false rules.[0]
        else            
            let emitAltCheck ruleIndex =
                let startSet = getStart grammar followSets ruleIndex                
                if startSet.Count = 1
                    then out |> emitLine 1 (sprintf "if _tokens.[pos] = %s" <| startSet.MinimumElement.ToString())
                    else let arrayString = Set.toSeq startSet |> Seq.map (fun x -> x.ToString()) |> getArrayRepresentation
                         out |> emitLine 1 (sprintf "if Array.exists ((=) _tokens.[pos]) %s" arrayString)
                let functionName = getParseFunctionName true ruleIndex                
                out |> emitLine 2 (sprintf "then addDescriptor %d // %s" (getFunctionIndex functionName) functionName)
            out |> emitEmptyLine            
            out |> emitLine 0 (sprintf "let private %s () =" <| getParseFunctionName false rules.[0])
            Array.iter emitAltCheck rules
            out |> emitLine 1 "continueExecution ()"
            Array.iter (emitSimpleParseFunction true) rules            
    
    Seq.iter emitParseFunction {0..grammar.indexator.nonTermCount-1}    
    out |> emitEmptyLine    
    out |> emitLine 0 "let parse tokens ="
    out |> emitLine 1 (sprintf "init tokens %s" <| getArrayRepresentation functionNames)
    out |> emitLine 1 (sprintf "%s ()" <| getParseFunctionName false grammar.startRule) 