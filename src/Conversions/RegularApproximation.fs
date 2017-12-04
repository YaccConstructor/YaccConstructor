module Yard.Core.Conversions.RegularApproximation

open QuickGraph
open Yard.Core
open Yard.Core.IL
open TransformAux
open QuickGraph.Algorithms

let private grammarToGraph (grammar : Grammar<_,_>) =

    let handleOneRule (rule : Rule<_,_>) =
        List.map (fun (nonTerm : Source) -> SEdge<string>(rule.name.text, nonTerm.text)) <| getAllNonTermOfProd rule.body

    let edges = List.collect handleOneRule grammar.Head.rules
    edges.ToAdjacencyGraph<string, SEdge<string>>()

let private getStronglyConnectedComps graph =
    let a = ConnectedComponents.StronglyConnectedComponentsAlgorithm<string, SEdge<string>>(graph)
    do a.Compute()
    a.Components
    |> Seq.toList
    |> List.groupBy (fun elem  -> elem.Value)
    |> List.map (snd >> List.map (fun elem -> elem.Key))

let private approximate (grammar : Grammar<_,_>) =
    let handleSets sets =
        let handleOneSet changed unchanged nonterminals =
            let inline newNonTerm nonTerm = new Source(nonTerm + "'")
            let epsilonRules =
                let newNonTerms = List.map newNonTerm nonterminals
                let epsilon = PSeq(List.empty, None, None)
                List.map (fun head -> defaultRule head epsilon) newNonTerms

            let wrapSeq head elemList =
                let addToRules isEmpty notEmpty head stock rules =
                    let rule = if List.isEmpty stock then isEmpty else PSeq(List.append stock [notEmpty], None, None)
                    List.Cons (defaultRule head rule, rules)
                let newHead, stock, lst =
                    List.fold (fun (head, stock, rules) elem ->
                        match elem.rule with
                        | PRef(nonTerm, _) as ref when List.contains nonTerm.text nonterminals ->
                            newNonTerm nonTerm.text, List.empty, addToRules ref elem head stock rules
                        | PToken _
                        | PRef _ -> head, List.append stock [elem], rules
                        | _ -> failwith "wrong grammar!")
                        (head, List.empty, List.empty) elemList
                let head' = PRef(newNonTerm head.text, None)
                addToRules head' (createDefaultElem head') newHead stock lst

            let rec transformOneRule isStart head =
                let makeStartRule rules =
                    let x :: xs = List.rev rules
                    { x with isStart = isStart } :: xs
                function
                | PToken _
                | PRef _ as prod -> makeStartRule <| wrapSeq head [createDefaultElem prod]
                | PSeq(elemList, _, _) -> makeStartRule <| wrapSeq head elemList
                | PAlt(prod1, prod2) -> List.append (transformOneRule isStart head prod1) (transformOneRule isStart head prod2)
                | _ -> failwith "wrong grammar!"

            let handleRulesOfNonterm = List.collect (fun rule -> transformOneRule rule.isStart rule.name rule.body)

            let changed, unchanged =
                List.fold (fun (changedRules, unchangedRules) nonterm ->
                        let needToChange, unchanchedRules = List.partition (fun (rule : Rule<_,_>) -> rule.name.text = nonterm) unchangedRules
                        List.append changedRules <| handleRulesOfNonterm needToChange, unchanchedRules)
                    (changed, unchanged)
                    nonterminals
            List.append epsilonRules changed, unchanged

        List.append <|| List.fold (fun (changedRules, unchangedRules) -> handleOneSet changedRules unchangedRules) (List.empty, grammar.Head.rules) sets

    grammar
    |> grammarToGraph
    |> getStronglyConnectedComps
    |> handleSets
    |> defaultModules

// ATTENTION! It is believed that there is ONLY ONE MODULE in the grammar, NO CONJUNCTION, NO EBNF, NO METARULES.
type RegularApproximation() =
    inherit Conversion()
        override this.Name = "RegularApproximation"
        override this.ConvertGrammar (grammar, _) = approximate grammar
