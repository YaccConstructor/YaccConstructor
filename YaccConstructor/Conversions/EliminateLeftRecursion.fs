//  Module EliminateLeftRecursion contains:
//  - a conversion which deletes left recursion from grammar
//  Current limitations:
//   - no epsilon productions elimination (so overall elimination not always works)
//   - no check if some productions would have duplicating binding names after inlining
//   - DLabels are just copied when inlining or modifying rules
//
//  Copyright 2009,2010 Ilia Chemodanov
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Yard.Core.Conversions.EliminateLeftRecursion

open Yard.Core
open Yard.Core.IL
open Production
open Yard.Core.Namer
open TransformAux

type Rule = Rule.t<Source.t,Source.t>
type Production = Production.t<Source.t,Source.t>

let noReduceError = sprintf "Cannot eliminate left recursion. Nonterminal %s does not reduce."
let trivialReduceError x = sprintf "Cannot eliminate left recursion. There is derivation %s -*> %s." x x

let lstArgName = Names.x "lst"
let tmpArgName = Names.x "tmp"
let defaultArgName = "_"
let emptyListAc = "[]"
let letTemplate = sprintf "let %s ="

let recPrefix = sprintf "(fun (%s) ->"
let recSuffix = sprintf ")::%s" lstArgName

let restPrefix = letTemplate tmpArgName
let restSuffix = sprintf "List.fold (fun s f -> f s) %s %s" tmpArgName lstArgName

let wrapActionCode preffix suffix (text:string) =
    let paddedText = System.Text.StringBuilder("    ").Append(text).Replace("\n", "\n    ").ToString()
    sprintf "%s\n%s\n%s" preffix paddedText suffix

let rec getLeftNonTermNames = function
        | PAlt (item1,item2) -> getLeftNonTermNames item1 + getLeftNonTermNames item2
        | PSeq (firstItem::_,_,_) -> getLeftNonTermNames firstItem.rule
        | PRef (name,_) -> Set.ofList [name.text]
        | _ -> Set.empty

// Tarjan algorithm implementation for getting strong components list
let getComponents (graph:int list[]) =
    let indices = Array.zeroCreate graph.Length
    let lowlinks = Array.zeroCreate graph.Length
    let stack = ref []
    let index = ref 0
    let strongComponents = ref []

    let rec strongConnect v =
        index := !index + 1
        indices.[v] <- !index
        lowlinks.[v] <- !index
        stack := v::(!stack)
        for w in graph.[v] do
            if indices.[w] = 0
            then strongConnect w
                 lowlinks.[v] <- min lowlinks.[v] lowlinks.[w]
            elif List.exists ((=) w) !stack
            then lowlinks.[v] <- min lowlinks.[v] indices.[w]
        if indices.[v] = lowlinks.[v] then
            let w = ref -1
            let comp = ref []
            while !w <> v do
                w := (!stack).Head
                stack := (!stack).Tail
                comp := !w::(!comp)
            strongComponents := !comp::(!strongComponents)

    for v = 0 to indices.Length-1 do
        if indices.[v] = 0 then strongConnect v
    !strongComponents

let splitAlt (rule:Rule) =
    let rec splitAlt' result = function
        | PAlt (item1,item2) -> List.append (splitAlt' result item1) (splitAlt' result item2)
        | x -> x::result
    splitAlt' [] rule.body

let createAlt = List.rev >> function
    | x::xs -> List.fold (fun state pr -> PAlt (pr, state)) x xs
    | _ -> failwith "no productions"

let removeImmediateRecursion (rule:Rule) =
    let isRecursiveProduction ruleName = function
        | PSeq (firstItem::_,_,_) ->
            match firstItem.rule with
            | PRef (name,_) when name.text=ruleName -> true
            | _ -> false
        | _ -> false

    let ruleName = rule.name.text
    let recProductions, restProductions =
        List.partition (isRecursiveProduction ruleName) (splitAlt rule)
    if recProductions.IsEmpty then [rule] else
    if restProductions.IsEmpty then
        failwith <| noReduceError ruleName
    let newNonTermName = Names.leftRec + ruleName
    let changedRestProductions = restProductions |> List.map (function
        | PSeq (items,ac,dlabel) as p ->
            let newLastItem = { omit = false
                                rule = PRef (genNewSource newNonTermName p, None)
                                binding = genNewSource lstArgName p |> Some
                                checker = None }//
            let newAc = ac |> Option.map (fun ac ->
                let newText = wrapActionCode restPrefix restSuffix ac.text
                Source.t(newText, ac))
            PSeq (List.append items [newLastItem], newAc, dlabel)
        | _ -> failwith "Unexpected production, expected PSeq")
    let newNonTermProductions = recProductions |> List.map (function
        | PSeq (recCall::items,ac,dlabel) as p ->
            if items.IsEmpty then 
                failwith <| trivialReduceError ruleName
            let newLastItem = { omit = false
                                rule = PRef (genNewSource newNonTermName p, None)
                                binding = genNewSource lstArgName p |> Some
                                checker = None }//
            let recCallBinding = 
                match recCall with
                | { binding = Some b } -> b.text
                | _ -> defaultArgName
            let newAc = ac |> Option.map (fun ac ->
                 let newText = wrapActionCode (recPrefix recCallBinding) recSuffix ac.text
                 Source.t(newText, ac))
            PSeq (List.append items [newLastItem], newAc, dlabel)//
        | _ -> failwith "Unexpected production, expected PSeq")
    let newNonTermProductions =
        PSeq ([], Some <| Source.t(emptyListAc), None)::newNonTermProductions
    [{ rule with body = createAlt changedRestProductions };
     { name = Source.t(newNonTermName)
       body = createAlt newNonTermProductions
       isStart = false
       isPublic = false
       args = []
       metaArgs = [] }]

let inlineRule indexToRule targetProductions sourceRuleIndex =
    let sourceRule = indexToRule sourceRuleIndex
    let sourceProductions = splitAlt sourceRule
    let isSourceRuleRef = function
        | PRef (name,_) -> name.text = sourceRule.name.text
        | _ -> false
    let inlineProduction = function
        | PSeq (firstItem::targetItems,targetAC,dlabel) when isSourceRuleRef firstItem.rule ->
            let (inlineProductionPart:Production->Production) = function
                | PSeq (sourceItems,sourceAC,_) ->
                    let inlinedItems = List.append sourceItems targetItems
                    let inlinedAC =
                        match sourceAC,targetAC with
                        | Some sourceAC, Some targetAC ->
                            let firstItemBinding =
                                match firstItem with
                                | { binding = Some b } -> b.text
                                | _ -> defaultArgName
                            let text = wrapActionCode (letTemplate firstItemBinding) targetAC.text sourceAC.text
                            Some <| Source.t(text,sourceAC)
                        | (Some _ as x), None
                        | None, (Some _ as x) -> x
                        | None, None -> None
                    PSeq (inlinedItems, inlinedAC, dlabel)//
                | _ -> failwith "Unexpected production, expected PSeq"
            List.map inlineProductionPart sourceProductions
        | x -> [x]
    targetProductions |> List.collect inlineProduction

let eliminateLeftRecursion (ruleList: Rule list) =
    let rules = Array.ofList ruleList
    let getLeftNonTermIndices (rule:Rule) =
        getLeftNonTermNames rule.body
        |> Seq.map (fun name -> rules |> Array.findIndex (fun rule -> rule.name.text = name))
        |> List.ofSeq
    // represents graph that has nonterminals as vertexes
    // and has edge from i to j iff there is a production i ::= j...
    let rulesWithLeftNonTerms = Array.map getLeftNonTermIndices rules
    let strongComponents = getComponents rulesWithLeftNonTerms
    // eliminate recursion in every strong component
    let rules = rules |> Array.map (fun x -> [x])
    let indexToRule i = rules.[i].Head
    for comp in strongComponents do
        for i = 0 to comp.Length-1 do
            let ruleIndex = comp.[i]
            let rule = indexToRule ruleIndex
            let newRuleBody =
                Seq.take i comp
                    |> Seq.fold (inlineRule indexToRule) (splitAlt rule)
                    |> createAlt
            rules.[ruleIndex] <- removeImmediateRecursion {rule with body=newRuleBody}
    Array.collect List.toArray rules |> Array.toList

// requires RemoveMeta, RemoveEbnf, RemoveInnerAlt, RemoveBrackets
type EliminateLeftRecursion() =
    inherit Conversion()
        override this.Name = "EliminateLeftRecursion"
        override this.ConvertGrammar (grammar,_) =
            mapGrammar eliminateLeftRecursion grammar
