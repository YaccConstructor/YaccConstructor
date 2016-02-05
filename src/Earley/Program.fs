namespace Yard.Earley 

open Yard.Core.Conversions.Linearize
open Yard.Core.IL.Production
open System.Collections.Generic

type EarleyItem(rule, dotPosition : int, startPosition : int) =
    let rule = rule
    let dotPosition = dotPosition
    let startPosition = startPosition

    member this.getRule = rule
    member this.getDotPosition = dotPosition
    member this.getStartPosition = startPosition

type Grammar(grammarPath : string) = 
    let ilParser = new Yard.Frontends.YardFrontend.YardFrontend()
    let il = ilParser.ParseGrammar(grammarPath)
    let converter = Yard.Core.Conversions.ExpandTopLevelAlt.ExpandTopLevelAlt()
    let grammar = converter.ConvertGrammar(il.grammar)
    
    member this.getGrammar = grammar.Head.rules
    member this.getIL = il

type Recognizer(grammar : Grammar) = 
    let grammar = grammar.getGrammar
    
    member this.recognize(input : System.String) = 
        let currentPosition = ref 0
        let stateSet = [|for _ in 0..(input.Length + 1) do yield new ResizeArray<EarleyItem>()|]
        
        // Prediction, Scan and Completion steps as described in the original article
        let prediction (item : EarleyItem) (nonTerminal : Yard.Core.IL.Source.t) =
            let predictions = grammar |> List.filter (fun (rule : Yard.Core.IL.Rule.t<_, _>) -> rule.name.text = nonTerminal.text)
            for prediction in predictions do
                let newItem = EarleyItem(prediction, 0, !currentPosition)
                if not (stateSet.[!currentPosition].Contains(newItem)) then
                    stateSet.[!currentPosition].Add(newItem)
            
        let scan (item : EarleyItem) terminal =
            if !currentPosition < input.Length && terminal = input.[!currentPosition] then 
                stateSet.[(!currentPosition + 1)].Add(EarleyItem(item.getRule, item.getDotPosition + 1, item.getStartPosition))
           
        let completion (item : EarleyItem) =
            let startOfPartialParse = item.getStartPosition
            let parsedNonTerminal = item.getRule.name.text
            for previousItem in stateSet.[startOfPartialParse] do
                let rightSideOfARule = match previousItem.getRule.body with 
                                        | PSeq(r, _, _) -> r
                                        | _ -> []
                if previousItem.getDotPosition < rightSideOfARule.Length then
                    match rightSideOfARule.[previousItem.getDotPosition].rule with
                    | PRef(previousNonTerminal, _) when previousNonTerminal.text = parsedNonTerminal ->
                        stateSet.[!currentPosition].Add(EarleyItem(previousItem.getRule, previousItem.getDotPosition + 1, previousItem.getStartPosition))
                    | _ -> ()

        let exploreItem (item : EarleyItem) =
            let currentRule = item.getRule
            let body = currentRule.body
            let rightSideOfARule = match body with 
                                        | PSeq(x, _, _) -> x
                                        | _ -> []            
            if rightSideOfARule.Length = item.getDotPosition then
                completion item
            else
                match rightSideOfARule.[item.getDotPosition].rule with
                | PToken(terminal) -> 
                    scan item terminal.text.[0]
                | PRef(nonTerminal, _) -> 
                    prediction item nonTerminal
                | _ -> 
                    printfn "MAYBE ERROR"

        let satisfiesSuccessfulParse (item : EarleyItem) = 
            let rule = item.getRule
            let body = rule.body
            let ruleLength = match body with 
                                   | PSeq(x, _, _) -> x.Length
                                   | _ -> 0
            rule.isStart && item.getStartPosition = 0 && ruleLength = item.getDotPosition
            
        // initialize
        stateSet.[0] <- new ResizeArray<EarleyItem>([for rule in grammar do if rule.isStart then yield EarleyItem(rule, 0, 0)])
        
        // main body: 2 loops
        for _ in 0..input.Length do
            let state = stateSet.[!currentPosition]
            let mutable pointer = 0
            while pointer < state.Count do
                exploreItem state.[pointer]
                pointer <- pointer + 1
            currentPosition := !currentPosition + 1

        // final check
        stateSet.[input.Length].Exists(fun i -> satisfiesSuccessfulParse(i))
