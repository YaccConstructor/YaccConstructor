namespace Yard.Earley 

open Yard.Core.Conversions.Linearize
open Yard.Core.IL.Production
open System.Collections.Generic

type EarleyItem = struct
    val Rule : Yard.Core.IL.Rule.t<Yard.Core.IL.Source.t, Yard.Core.IL.Source.t>
    val DotPosition : int
    val StartPosition : int

    new (rule, dotPosition, startPosition) = 
        {Rule = rule; DotPosition = dotPosition; StartPosition = startPosition}
end

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
        
        let getRightSide(rule : Yard.Core.IL.Rule.t<_, _>) =
            match rule.body with 
                | PSeq(x, _, _) -> x
                | _ -> []
            
        // Prediction, Scan and Completion steps as described in the original article
        let prediction (item : EarleyItem) (nonTerminal : Yard.Core.IL.Source.t) =
            let predictions = grammar |> List.filter (fun (rule : Yard.Core.IL.Rule.t<_, _>) -> rule.name.text = nonTerminal.text)
            for prediction in predictions do
                let newItem = new EarleyItem(prediction, 0, !currentPosition)
                if not (stateSet.[!currentPosition].Contains(newItem)) then
                    stateSet.[!currentPosition].Add(newItem)
            
        let scan (item : EarleyItem) terminal =
            if !currentPosition < input.Length && terminal = input.[!currentPosition] then 
                stateSet.[(!currentPosition + 1)].Add(new EarleyItem(item.Rule, item.DotPosition + 1, item.StartPosition))
           
        let completion (item : EarleyItem) =
            let startOfPartialParse = item.StartPosition
            let parsedNonTerminal = item.Rule.name.text
            for previousItem in stateSet.[startOfPartialParse] do
                let rightSideOfARule = getRightSide(previousItem.Rule)
                if previousItem.DotPosition < rightSideOfARule.Length then
                    match rightSideOfARule.[previousItem.DotPosition].rule with
                    | PRef(previousNonTerminal, _) when previousNonTerminal.text = parsedNonTerminal ->
                        stateSet.[!currentPosition].Add(new EarleyItem(previousItem.Rule, previousItem.DotPosition + 1, previousItem.StartPosition))
                    | _ -> ()

        let exploreItem (item : EarleyItem) =
            let currentRule = item.Rule
            let rightSideOfARule = getRightSide(currentRule)
            if rightSideOfARule.Length = item.DotPosition then
                completion item
            else
                match rightSideOfARule.[item.DotPosition].rule with
                | PToken(terminal) -> 
                    scan item terminal.text.[0]
                | PRef(nonTerminal, _) -> 
                    prediction item nonTerminal
                | _ -> 
                    raise (System.ArgumentException("Right side of a rule contains unsupported items"))

        let satisfiesSuccessfulParse (item : EarleyItem) = 
            let rule = item.Rule
            let ruleLength = getRightSide(rule).Length
            rule.isStart && item.StartPosition = 0 && ruleLength = item.DotPosition
            
        // initialize
        stateSet.[0] <- new ResizeArray<EarleyItem>([for rule in grammar do if rule.isStart then yield new EarleyItem(rule, 0, 0)])
        
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
