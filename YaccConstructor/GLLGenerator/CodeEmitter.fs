// CodeEmitter.fs contains logic that emits GLL parser code to a specified stream

module Yard.Generators.GLL.CodeEmitter

open System.Text
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.FinalGrammar

// Calculate set of terminals that can be at a start of specific production.
// Set is calculated as FIRST of the sequence of tokens on rignt side of the production
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

let private emitLine shift text (out:StringBuilder) =
    out
      .Append(String.replicate (shift<<<2) " ")
      .AppendLine(text)
    |> ignore 

let private emitEmptyLine (out:StringBuilder) = out.AppendLine() |> ignore

let private getArrayRepresentation (arr : string seq) =
    "[|" + System.String.Join(";", arr) + "|]"
let private getListRepresentation (arr : string seq) =
    "[" + System.String.Join(";", arr) + "]"

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
    let emitSets setName getSet =
        seq { 0 .. (grammar.indexator.nonTermCount-1) }
        |> Seq.iter (fun i -> let set = getSet i
                              let array = Seq.map grammar.indexator.indexToTerm set |> getArrayRepresentation
                              out |> emitLine 1 (sprintf "%s(%s) = %s" setName (grammar.indexator.indexToNonTerm i) array))
    emitSets "FIRST" (fun i -> grammar.firstSet.[i])
    emitSets "FOLLOW" (fun i -> followSets.[i])    
    out |> emitLine 0 "*)"
    
    let productionsArr =
        seq { 0 .. (grammar.rules.rulesCount-1) }
        |> Seq.map (grammar.rules.rightSide
                    >> Array.map (fun idx -> 
                                      if grammar.indexator.isNonTerm idx
                                      then sprintf "Nonterminal %d" idx
                                      else sprintf "Terminal %d" idx)
                    >> getListRepresentation)
        |> getArrayRepresentation
    
    let addAction ruleIndex actions terminalIndex =
        let ruleLeft = grammar.rules.leftSide ruleIndex
        let mapKey = (terminalIndex, ruleLeft)
        let existingRules = 
            match Map.tryFind mapKey actions with
            | None -> Set.empty
            | Some x -> x
        Map.add mapKey (Set.add ruleIndex existingRules) actions
    let addActionsForRule actions ruleIndex =
        let startSet = getStart grammar followSets ruleIndex
        Seq.fold (addAction ruleIndex) actions startSet
    let actions = seq { 0 .. (grammar.rules.rulesCount-1) }
                  |> Seq.fold addActionsForRule Map.empty
    let actionsArr =
        Map.toSeq actions
        |> Seq.map (fun ((x,y),lst) -> let listRepresentation =
                                           Set.toArray lst
                                           |> Array.map (fun x -> x.ToString())
                                           |> getListRepresentation
                                       sprintf "(%d,%d),%s" x y  listRepresentation)
        |> getArrayRepresentation

    out
      .AppendLine(sprintf "let private productions = %s" productionsArr)
      .AppendLine(sprintf "let private actions = %s " actionsArr)
      .AppendLine(sprintf "let parse tokens = ParserBase(%d, %d, actions, productions, tokens).parse()"
                          grammar.rules.startSymbol
                          grammar.indexator.eofIndex)
    |> ignore
