module ProductionGraph

open System.Collections.Generic
open Yard.Generators.Common.FinalGrammar
open QuickGraph
open Yard.Core.IL
open Yard.Core.IL.Rule

type ContextLabel = L | R | B

type ProductionGraph(grammar: FinalGrammar) as this =
    inherit AdjacencyGraph<int, TaggedEdge<int, ContextLabel>>()
    let checkContext pos length =
        if length > 1 
        then
            if pos = length - 1
            then ContextLabel.L
            elif pos = 0 
            then ContextLabel.R
            else ContextLabel.B
        else ContextLabel.B
    do
        for nonTerm in set grammar.rules.leftSideArr do
            let targetNonTerms = new Dictionary<int, ContextLabel>()
            for rule in grammar.rules.rulesWithLeftSide nonTerm do
                let rightSide = grammar.rules.rightSide rule
                for i in 0 .. rightSide.Length - 1 do
                    let symb = rightSide.[i]
                    if grammar.indexator.isNonTerm symb
                    then
                        if targetNonTerms.ContainsKey symb
                        then
                            let curLabel = targetNonTerms.[symb]
                            if curLabel <> B
                            then
                                if curLabel <> checkContext i rightSide.Length
                                then targetNonTerms.[symb] <- B
                            else ()
                        else targetNonTerms.Add (symb, checkContext i rightSide.Length)
            for item in targetNonTerms do
                this.AddVerticesAndEdge 
                     (new TaggedEdge<int, ContextLabel> (nonTerm, item.Key, item.Value)) |> ignore
    
    new () =
        let rule = {name = new Source.t(""); 
                    body = Production.PToken(new Source.t("")); 
                    args = []; 
                    isStart = true; 
                    isPublic = false; 
                    metaArgs = []}
        ProductionGraph(new FinalGrammar([rule], true))