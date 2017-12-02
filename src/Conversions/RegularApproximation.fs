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
    let epsilon = PSeq(List.empty, None, None)
    let inline newNonTerm nonTerm = new Source(nonTerm + "'")
    let handleSets sets =
        let handleOneSet changed unchanged nonterminals =
            let epsilonRules =
                let newNonTerms = List.map newNonTerm nonterminals
                List.map (fun head -> defaultRule head epsilon) newNonTerms

            let wrapSeq head elemList =
                let newHead, stock, lst =
                    List.fold (fun (head, stock, rules) elem ->
                        match elem.rule with
                        | PRef(nonTerm, _) when List.contains nonTerm.text nonterminals ->
                            newNonTerm nonTerm.text, List.empty, List.Cons (defaultRule head <| PSeq(List.append stock [elem], None, None), rules)
                        | PToken _
                        | PRef _ -> head, List.append stock [elem], rules
                        | _ -> failwith "wrong grammar!")
                        (head, List.empty, List.empty) elemList
                let head' = PRef(newNonTerm head.text, None)
                let stock = if stock.IsEmpty then head' else PSeq(List.append stock [createDefaultElem <| head'], None, None)
                List.Cons(defaultRule newHead stock, lst)

            let rec transformOneRule isStart head prod =
                let x :: xs =
                    match prod with
                    | PToken _
                    | PRef _ as prod -> wrapSeq head [createDefaultElem prod]
                    | PSeq(elemList, _, _) -> wrapSeq head elemList
                    | PAlt(prod1, prod2) -> List.append (transformOneRule isStart head prod1) (transformOneRule isStart head prod2)
                    | _ -> failwith "wrong grammar!"
                { x with isStart = isStart } :: xs

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
