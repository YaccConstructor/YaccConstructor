module Yard.Core.Conversions.RegularApproximation

open QuickGraph
open Yard.Core
open Yard.Core.IL
open TransformAux

let private grammarToGraph (grammar : Grammar<_,_>) =

    let handleOneRule (rule : Rule<_,_>) =
        List.map (fun nonTerm -> SEdge<Source>(rule.name, nonTerm)) <| getAllNonTermOfProd rule.body

    let edges = List.collect handleOneRule grammar.Head.rules
    edges.ToAdjacencyGraph<Source, SEdge<Source>>()

let inline private getStronglyConnectedComps graph =
    QuickGraph.Algorithms.ConnectedComponents.StronglyConnectedComponentsAlgorithm(graph).Graphs

let private approximate (grammar : Grammar<_,_>) =
    let newNonTerm (nonTerm : Source) = new Source(nonTerm.text + "'")
    let PhaseOne =
        let newNonTerms = List.map (fun (rule : Rule<_,_>) -> newNonTerm rule.name) grammar.Head.rules
        let epsilon = PSeq(List.empty, None, None)
        List.map (fun head -> defaultRule head epsilon) newNonTerms

    let PhaseTwo =
        let handleOneComponent vertexes =
            let wrapSeq head elemList =
                let newHead, stock, lst =
                    List.fold (fun (head, stock, rules) elem ->
                        match elem.rule with
                        | PRef(nonTerm, _) when List.contains nonTerm vertexes ->
                            newNonTerm nonTerm, List.empty, List.Cons (defaultRule head <| PSeq(List.append stock [elem], None, None), rules)
                        | PToken _
                        | PRef _ -> head, List.append stock [elem], rules
                        | _ -> failwith "wrong grammar!")
                        (head, List.empty, List.empty) elemList
                let head' = PRef(newNonTerm head, None)
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

            let handleOneVertex vertex =
                grammar.Head.rules
                |> List.filter (fun rule -> rule.name = vertex)
                |> List.collect (fun rule -> transformOneRule rule.isStart rule.name rule.body)

            List.collect handleOneVertex vertexes

        grammar
        |> grammarToGraph
        |> getStronglyConnectedComps
        |> List.ofSeq
        |> List.map (fun stronglyConnectedComponent -> List.ofSeq stronglyConnectedComponent.Vertices)
        |> List.collect handleOneComponent

    defaultModules(List.append PhaseOne PhaseTwo)

// ATTENTION! It is believed that there is ONLY ONE MODULE in the grammar, NO CONJUNCTION, NO EBNF, NO METARULES.
type RegularApproximation() =
    inherit Conversion()
        override this.Name = "RegularApproximation"
        override this.ConvertGrammar (grammar, _) = approximate grammar
