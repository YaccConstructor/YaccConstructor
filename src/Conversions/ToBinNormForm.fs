module Yard.Core.Conversions.ToBinNormForm

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule
open Microsoft.FSharp.Collections
open ToChomNormForm


let mutable startEps = false
type NonTermComparer<'t1,'t2 when 't1:equality and 't2:equality>() =
    interface IEqualityComparer<Production.elem<'t1,'t2>> with
        member this.Equals(x, y) = match x.rule, y.rule with PRef (x, _), PRef (y, _) -> x.text = y.text | _ -> false
        member this.GetHashCode x = match x.rule with PRef (x, _) -> hash x.text | _ -> hash x

type RuleComparer<'t1,'t2 when 't1:equality and 't2:equality>() =
    interface IEqualityComparer<Rule.t<'t1,'t2>> with
        member this.Equals(x, y) = (x.name.ToString() + x.body.ToString() = y.name.ToString() + y.body.ToString())
        member this.GetHashCode x = hash (x.name.ToString() + x.body.ToString())


let rec getElements body lst1 lst2 flag = 
    match body with 
    | PSeq(e, a, l) -> 
                    if flag then lst1 @ [e], lst2
                    else lst1, lst2 @ [e]
                    
    | PRef(t, a) -> 
                    if flag then lst1 @ [[TransformAux.createDefaultElem(PRef(t, a))]], lst2
                    else lst1, lst2 @ [[TransformAux.createDefaultElem(PRef(t, a))]]
    | PToken t -> 
                    if flag then lst1 @ [[TransformAux.createDefaultElem(PToken t)]], lst2
                    else lst1, lst2 @ [[TransformAux.createDefaultElem(PToken t)]]
    | PConj(a, b) -> fst (getElements a lst1 lst2 flag) @ fst(getElements b lst1 lst2 flag), snd (getElements a lst1 lst2 flag) @ snd(getElements b lst1 lst2 flag) 
    | PNeg t -> getElements t lst1 lst2 false
     

let rec createConj rules  = 
    match Seq.length rules with
    | 2 -> PConj((Seq.item(0) rules).body, (Seq.item(1) rules).body)
    | 1 -> (Seq.item(0) rules).body
    | _ -> PConj((Seq.item(0) rules).body, createConj (Seq.except [(Seq.item(0) rules)] rules)) 


let deleteLongRules (rules: Rule.t<_,_> list) =
    let newRules = HashSet<_>(new RuleComparer<_,_>())
    for rule in rules do
        let elements = getElements rule.body [] [] true
        let conjRule = ResizeArray<_>()
        for el in fst elements @ snd elements do 
            let res = ToChomNormForm.deleteLongRules [TransformAux.createRule rule.name rule.args (PSeq(el, None, None)) rule.isStart rule.metaArgs]           
            newRules.UnionWith(List.filter (fun x -> not (x.name.text = rule.name.text)) res)
            if List.contains el (fst elements) then conjRule.Add(List.find (fun x -> x.name.text = rule.name.text) res)
            else conjRule.Add(TransformAux.createRule rule.name rule.args (PNeg (List.find (fun x -> x.name.text = rule.name.text) res).body) rule.isStart rule.metaArgs)
        newRules.Add(TransformAux.createRule rule.name rule.args (createConj conjRule) rule.isStart rule.metaArgs) |> ignore
    newRules |> Seq.toList

let rec getCombinations lstlst =
    match lstlst with
    | h::[] ->
        List.fold (fun acc elem -> [elem]::acc) [] h
    | h::t ->
        List.fold (fun cacc celem ->
            (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
            ) [] (getCombinations t)
    | _ -> []

let deleteEpsilonRules (rules: Rule.t<_,_> list) =
    let splitRules = HashSet<_>() 
    for rule in rules do 
        for el in fst (getElements rule.body [] [] true) do
            splitRules.Add(TransformAux.createRule rule.name rule.args (PSeq(el, None, None)) rule.isStart rule.metaArgs) |> ignore
    let epsNonterms = ToChomNormForm.collectEpsNonterms (Seq.toList splitRules)
    let newRules = HashSet<_>(rules, new RuleComparer<_, _>()) 
    for rule in rules do
        let conjs = ResizeArray<t<_,_> list>()
        let elements = getElements rule.body [] [] true
        for el in fst elements @ snd elements do
            let res = ToChomNormForm.deleteEpsilonRules [TransformAux.createRule rule.name rule.args (PSeq(el, None, None)) rule.isStart rule.metaArgs] epsNonterms  |> Seq.toList   
            if List.contains el (fst elements) then conjs.Add(res) 
            else conjs.Add(res |> List.map (fun x -> TransformAux.createRule x.name x.args (PNeg(x.body)) x.isStart x.metaArgs))
        let combinations = getCombinations (conjs |> Seq.toList) 
        for i in 0..combinations.Length - 1 do
            newRules.Add(TransformAux.createRule rule.name rule.args (createConj combinations.[i]) rule.isStart rule.metaArgs) |> ignore     
    newRules.RemoveWhere(fun x -> List.contains [] (fst (getElements x.body [] [] true))) |> ignore
    let start = (List.find (fun x -> x.isStart = true) rules).name
    if List.contains (TransformAux.createDefaultElem (PRef(start,None))) epsNonterms
    then startEps <- true
    newRules |> Seq.map (fun x -> 
                                if match x.body with PSeq(e,a,l) -> true | _ -> false && match (match x.body with PSeq(e,a,l) -> e).[0].rule with PToken t -> true |_ -> false && (match x.body with PSeq(e,a,l) -> e).Length = 1
                                then x
                                else TransformAux.createRule x.name x.args (PConj(x.body, PNeg(PSeq([], None, None)))) x.isStart x.metaArgs ) |> Seq.toList

let collectNontermsAndUnitRules (rules: Rule.t<_,_> list) = 
    let unitRules = HashSet<_>(new RuleComparer<_, _>())
    let nonterms = HashSet<_>(new NonTermComparer<_,_>())
    for rule in rules do
        nonterms.Add(TransformAux.createDefaultElem(PRef(rule.name, None))) |> ignore
        let elements = getElements rule.body [] [] true
        for el in fst elements @ snd elements do 
            if el.Length = 1 && match el.[0].rule with PRef(t, _) -> true | _ -> false
            then 
                nonterms.Add(el.[0]) |> ignore
                unitRules.Add(rule) |> ignore
    nonterms, unitRules

let filter rule = 
    let el1 = fst (getElements rule.body [] [] true) |> List.toSeq |> Seq.toList |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs) 
    let el2 = (snd (getElements rule.body [] [] true) |> List.filter(fun x -> not (x = []))) @ [[]] |> List.toSeq |> Seq.toList |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs)
    TransformAux.createRule rule.name rule.args (createConj (el1 @ el2)) rule.isStart rule.metaArgs

let deleteUnitRules (rules: Rule.t<_,_> list) =
    let newRules = HashSet<_>(rules, new RuleComparer<_, _>())
    let rules2 = HashSet<_>(rules, new RuleComparer<_, _>())
    let nonTerms, unitRules = collectNontermsAndUnitRules rules
    let mutable flag = true
    while flag do
        for rule in rules2 do
            if unitRules.Contains(rule) then 
                let elements = getElements rule.body [] [] true
                let unitConjs = List.filter (fun x -> List.length x = 1 && Seq.contains x.[0] nonTerms) (fst elements)
                let unitConjsNeg = List.filter (fun x -> List.length x = 1 && Seq.contains x.[0] nonTerms) (snd elements)
                for c in unitConjs @ unitConjsNeg do 
                    let rulesToAdd = Seq.filter (fun x -> x.name.text = c.[0].rule.ToString()) rules2              
                    if List.contains c unitConjs then 
                       newRules.UnionWith(rulesToAdd |> Seq.map (fun x -> 
                                                                            let pos = (List.except([c]) (fst elements) @ fst (getElements x.body [] [] true)) 
                                                                                                |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs)
                                                                            let neg = (snd elements @ snd (getElements x.body [] [] true)) 
                                                                                                        |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs)
                                                                            let newBody = pos @ neg |> List.toSeq
                                                                            TransformAux.createRule rule.name rule.args (createConj newBody) rule.isStart rule.metaArgs |> filter))
                    else
                        newRules.UnionWith(rulesToAdd |> Seq.collect (fun x -> 
                                                                            let pos = fst elements |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs)
                                                                            let neg = List.except([c]) (snd elements) |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs)
                                                                            let neg2 = 
                                                                                if List.isEmpty (fst (getElements x.body [] [] true) |> List.filter(fun x -> not (List.isEmpty x)) |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs))
                                                                                then [TransformAux.createRule rule.name rule.args (createConj (pos @ neg |> List.toSeq)) rule.isStart rule.metaArgs]                                                                          
                                                                                else
                                                                                    (fst (getElements x.body [] [] true) |> List.filter(fun x -> not (List.isEmpty x))|> List.map (fun x -> TransformAux.createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs))
                                                                                |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PConj(x.body, createConj (pos @ neg |> List.toSeq))) rule.isStart rule.metaArgs)
                                                                            let pos2 = 
                                                                                if List.isEmpty (snd (getElements x.body [] [] true) |> List.filter(fun x -> not (List.isEmpty x)) |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs))
                                                                                then neg2                                                                         
                                                                                else
                                                                                    (snd (getElements x.body [] [] true)|> List.filter(fun x -> not (List.isEmpty x)) |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs))
                                                                                |> List.map (fun x -> TransformAux.createRule rule.name rule.args (PConj(x.body, createConj neg2)) rule.isStart rule.metaArgs)                                                     
                                                                            pos2 |> Seq.map filter))
     
                    
                newRules.Remove(rule) |> ignore
        rules2.Clear()
        rules2.UnionWith(newRules)
        unitRules.Clear()
        unitRules.UnionWith(snd (collectNontermsAndUnitRules (rules2|>Seq.toList)))
        if unitRules.Count = 0 then flag <- false   
    newRules |> Seq.toList

let deleteFewTermRules  (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_,_>())
    for rule in rules do
        let elements = getElements rule.body [] [] true
        let conjRule = ResizeArray<_>()
        for el in fst elements @ snd elements do 
            if not (el = []) then
                let res = ToChomNormForm.deleteFewTermRules [TransformAux.createRule rule.name rule.args (PSeq(el, None, None)) rule.isStart rule.metaArgs] newRules         
                newRules.UnionWith(List.filter (fun x -> not (x.name.text = rule.name.text)) res)
                if List.contains el (fst elements) then conjRule.Add(List.find (fun x -> x.name.text = rule.name.text) res)
                else conjRule.Add(TransformAux.createRule rule.name rule.args (PNeg (List.find (fun x -> x.name.text = rule.name.text) res).body) rule.isStart rule.metaArgs)
            else conjRule.Add(TransformAux.createRule rule.name rule.args (PNeg(PSeq([], None, None))) rule.isStart rule.metaArgs)
        newRules.Remove(rule) |> ignore
        newRules.Add(TransformAux.createRule rule.name rule.args (createConj conjRule) rule.isStart rule.metaArgs) |> ignore
    newRules |> Seq.toList

let filterAndStuff (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_,_>())
    for rule in rules do
        let elements = getElements rule.body [] [] true
        for el in fst elements @ snd elements do 
            if (fst elements).Length + (snd elements).Length > 2 && el.Length = 1 && match el.[0].rule with PToken t -> true | _ -> false 
            then newRules.Remove(rule) |> ignore
        if not (Seq.isEmpty (Seq.filter (fun x -> 
                                  let el1, el2 = fst (getElements x.body [] [] true), snd(getElements x.body [] [] true) 
                                  if not (x = rule)
                                        && x.name.text = rule.name.text 
                                            && not (List.contains false (el1 |> List.map (fun x -> List.contains x (fst elements))))
                                                && not (List.contains false (el2 |> List.map (fun x -> List.contains x (snd elements))))
                                  then true
                                  else false) newRules)) 
        then newRules.Remove(rule) |> ignore 
    if startEps then 
        let start = (Seq.find (fun x -> x.isStart = true) newRules).name
        newRules.Add(TransformAux.createRule start [] (PSeq([], None, None)) true [] ) |> ignore
    newRules |> Seq.toList
                 

let toBinNormForm (rules: Rule.t<_,_> list) = 
    rules
    |> deleteLongRules
    |> deleteEpsilonRules
    |> deleteUnitRules
    |> deleteFewTermRules
    |> filterAndStuff
    
type ToBinNormForm() = 
    inherit Conversion()
        override this.Name = "ToBinNormForm"
        override this.ConvertGrammar (grammar, _) = mapGrammar toBinNormForm grammar



                
        
    

