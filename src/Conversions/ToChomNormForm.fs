module Yard.Core.Conversions.ToChomNormForm

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule
open Microsoft.FSharp.Collections



let deleteLongRules (rules: Rule.t<'a,'b> list) =
    let newRules = ref []
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e 
        if elements.Length < 3 then 
            newRules := !newRules @ [rule]
        else 
            let names = [|for i in 0..elements.Length-3 -> Namer.newSource(rule.name)|]  
            newRules := !newRules @ [TransformAux.createRule rule.name rule.args (PSeq([elements.[0]; TransformAux.createDefaultElem (PRef(names.[0], None))], None, None)) false rule.metaArgs] 
            for i in 1..elements.Length-3 do
                newRules := !newRules @ [ TransformAux.createRule names.[i-1] rule.args (PSeq([elements.[i]; TransformAux.createDefaultElem (PRef(names.[i], None))], None, None)) false rule.metaArgs] 
            newRules := !newRules @ [TransformAux.createRule names.[elements.Length-3] rule.args (PSeq([elements.[elements.Length-2]; elements.[elements.Length-1]], None, None)) false rule.metaArgs] 
    !newRules

let collectEpsNeterms (rules: Rule.t<'a,'b> list) = 
    let epsNeterms = new ResizeArray<elem<'a,'b>> ()
    let mutable flag = false
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e
        if elements.IsEmpty then
            epsNeterms.Add(TransformAux.createDefaultElem(PRef(rule.name, None)))
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e
        if not (elements.IsEmpty) && epsNeterms.Contains(elements.[0]) then flag <- true
        while flag do
            for el in elements do
                if not (epsNeterms.Contains(el)) then flag <- false
        if flag = true then 
            flag <- false
            epsNeterms.Add(TransformAux.createDefaultElem(PRef(rule.name, None)))
    epsNeterms |> Seq.toList

let deleteEpsilonRules (rules: Rule.t<'a,'b> list) = 
    let epsNeterms = collectEpsNeterms rules |> List.map (fun x -> x.rule.ToString())
    let rules2 = new ResizeArray<Rule.t<'a,'b>> ()
    for rule in rules do rules2.Add(rule)   
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e
        if elements.Length = 2 then
            if (List.contains (elements.[0].rule.ToString()) epsNeterms) && (List.contains (elements.[1].rule.ToString()) epsNeterms) then
                rules2.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[0]], None, None)) false rule.metaArgs)
                rules2.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[1]], None, None)) false rule.metaArgs)
            else
                if (List.contains (elements.[0].rule.ToString()) epsNeterms) then
                    rules2.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[1]], None, None)) false rule.metaArgs)
                if (List.contains (elements.[1].rule.ToString()) epsNeterms) then
                    rules2.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[0]], None, None)) false rule.metaArgs)
    let toDelete =  new ResizeArray<int> ()
    for rule in rules2 do
        let elements = match rule.body with PSeq(e, a, l) -> e
        if elements.IsEmpty then toDelete.Add(rules2.IndexOf(rule))
    for i in (Seq.rev toDelete) do rules2.RemoveAt(i)
    rules2 |> Seq.toList
        
let toChomNormForm (ruleList: Rule.t<_, _> list) = 
    ruleList
    |> deleteLongRules
    |> deleteEpsilonRules

type ToChomNormForm() = 
    inherit Conversion()
        override this.Name = "ToChomNormForm"
        override this.ConvertGrammar (grammar,_) = mapGrammar toChomNormForm grammar



                
        
    

