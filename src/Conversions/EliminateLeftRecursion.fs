//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

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

let rec getLeftNonTermNames onlyLeftmost = function
        | PAlt (item1,item2) -> getLeftNonTermNames onlyLeftmost item1 + getLeftNonTermNames onlyLeftmost item2
        | PSeq (items,_,_) -> 
            if onlyLeftmost
            then match items with
                 | firstItem::_ -> getLeftNonTermNames true firstItem.rule
                 | _ -> Set.empty
            else items |> List.fold (fun state item -> state + getLeftNonTermNames false item.rule) Set.empty
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

// returns the connected component that includes specified vertex
let getConnectedComponent (graph:int list[]) v =
    let visited = Array.zeroCreate graph.Length
    visited.[v] <- true
    let stack = ref [v]
    while not (!stack).IsEmpty do
        let v = (!stack).Head
        let newVertices = List.filter (fun x -> not visited.[x]) graph.[v]
        for i in newVertices do
            visited.[i] <- true
        stack := List.append (!stack).Tail newVertices
    [0..graph.Length-1] |> List.filter (fun x -> visited.[x])

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
                                rule = PRef (genNewSourceWithRange newNonTermName p, None)
                                binding = genNewSourceWithRange lstArgName p |> Some
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
                                rule = PRef (genNewSourceWithRange newNonTermName p, None)
                                binding = genNewSourceWithRange lstArgName p |> Some
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
       isInline = false
       args = []
       metaArgs = [] }]

let isEpsilonProduction = function
    | PSeq ( [],_,_) -> true
    | _ -> false

let rec isRuleRef (rule:Rule) = function
    | PRef (name,_) -> name.text = rule.name.text
    | PSeq (item::items,_,_) -> isRuleRef rule item.rule
    | _ -> false

let getRulesWithEpsilons indexToRule strongComponent =
    let hasEpsilon i =
        let rule = indexToRule i
        (rule |> splitAlt |> List.exists isEpsilonProduction) &&
        (List.exists (fun j -> j<>i && List.exists (isRuleRef rule) (j |> indexToRule |> splitAlt))
                     strongComponent)
    List.filter hasEpsilon strongComponent    

let inlineRule indexToRule targetProductions sourceRuleIndex =
    let sourceRule = indexToRule sourceRuleIndex
    let sourceProductions = splitAlt sourceRule
    let inlineProduction = function
        | PSeq (firstItem::targetItems,targetAC,dlabel) as prod when isRuleRef sourceRule firstItem.rule ->
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

let rec eliminateLeftRecursion (ruleList: Rule list) =
    let rules = Array.ofList ruleList
    let getNonTermIndices onlyLeftmost (rules:Rule[]) (rule:Rule) =
        getLeftNonTermNames onlyLeftmost rule.body
        |> Seq.map (fun name -> rules |> Array.findIndex (fun rule -> rule.name.text = name))
        |> List.ofSeq
    // represents graph that has nonterminals as vertexes
    // and has edge from i to j iff there is a production i ::= j...
    let rulesWithLeftNonTerms = Array.map (getNonTermIndices true rules) rules
    let strongComponents = getComponents rulesWithLeftNonTerms
    let rules = rules |> Array.map (fun x -> [x])
    let indexToRule i = rules.[i].Head

    // inline epsilon productions that prevent hidden recursion elimination
    let rulesWithEps = List.collect (getRulesWithEpsilons indexToRule) strongComponents
    if not rulesWithEps.IsEmpty
    then
        for ruleIndex in rulesWithEps do
            for i = 0 to rules.Length-1 do
                let rule = indexToRule i
                let newBody = inlineRule indexToRule (splitAlt rule) ruleIndex |> createAlt
                rules.[i] <- [{rule with body=newBody}]
        let rules = Array.collect List.toArray rules |> Array.toList
        eliminateLeftRecursion rules
    else

    // eliminate recursion in every strong component
    for comp in strongComponents do        
        let comp = List.sortBy (indexToRule >> splitAlt >>
                                List.filter (not << isEpsilonProduction) >>
                                List.length) comp
        for i = 0 to comp.Length-1 do
            let ruleIndex = comp.[i]
            let rule = indexToRule ruleIndex
            let newRuleBody =
                Seq.take i comp
                    |> Seq.fold (inlineRule indexToRule) (splitAlt rule)
                    |> createAlt
            rules.[ruleIndex] <- removeImmediateRecursion {rule with body=newRuleBody}

    // remove unused rules
    let rules = Array.collect List.toArray rules
    let rulesWithNonTerms = Array.map (getNonTermIndices false rules) rules
    let startRule = rules |> Array.findIndex (fun rule -> rule.isStart)
    let usedRules = getConnectedComponent rulesWithNonTerms startRule
    usedRules |> List.map (fun i -> rules.[i])    

// requires RemoveMeta, RemoveEbnf, RemoveInnerAlt, RemoveBrackets






type EliminateLeftRecursion() =
    inherit Conversion()
        override this.Name = "EliminateLeftRecursion"
        override this.ConvertGrammar (grammar,_) =
            mapGrammar eliminateLeftRecursion grammar
