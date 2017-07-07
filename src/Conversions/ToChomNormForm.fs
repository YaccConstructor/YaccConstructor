module Yard.Core.Conversions.ToChomNormForm

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule
open Microsoft.FSharp.Collections

type NonTermComparer<'t1,'t2 when 't1:equality and 't2:equality> () =
    interface IEqualityComparer<Production.elem<'t1,'t2>> with
        member this.Equals(x,y) = match x.rule,y.rule with PRef (x,_),PRef (y,_) -> x.text = y.text | _ -> false
        member this.GetHashCode x = match x.rule with PRef (x,_) -> hash x.text | _ -> hash x

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
    let epsNeterms = System.Collections.Generic.HashSet<_>(new NonTermComparer<_,_>())
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e
        if elements.IsEmpty then
            if epsNeterms.Add(TransformAux.createDefaultElem(PRef(rule.name, None))) then ()    
    let mutable flag = true
    //epsNeterms |> Seq.iter (fun x -> printfn "%A" (x.GetHashCode()))
    //printfn "_____"
    while flag do
        for rule in rules do
            let elements = match rule.body with PSeq(e, a, l) -> e
      //      if not (elements.IsEmpty)  then elements.[0].GetHashCode() |> printfn "%A" 
            if not (elements.IsEmpty) 
               && (   (elements.Length = 1 && epsNeterms.Contains(elements.[0])) 
                   || (elements.Length = 2 && epsNeterms.Contains(elements.[0]) && epsNeterms.Contains(elements.[1]))) 
            then epsNeterms.Add(TransformAux.createDefaultElem(PRef(rule.name, None))) |> ignore
            else flag <- false
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

      

let findPairs (pairs: ResizeArray<Source.t*Source.t>) (rules: ResizeArray<Rule.t<'a,'b>>) = 
    let pairs2 = System.Collections.Generic.HashSet<_>(pairs)
    let queue = new Queue<Source.t*Source.t>(pairs)
    while queue.Count > 0 do
        let q = queue.Dequeue()
        for rule in rules do
            let elements = match rule.body with PSeq(e, a, l) -> e
            let el = match elements.[0].rule with PRef(t,_) -> t      
            if (snd q).ToString() = rule.name.ToString() then 
                if pairs2.Add((fst q, el)) then
                    queue.Enqueue((fst q, el))
    pairs2 

let deleteUnitRules (rules: Rule.t<'a,'b> list) = 
    let newRules = new ResizeArray<Rule.t<'a,'b>> ()
    let unitRules = new ResizeArray<Rule.t<'a,'b>> ()
    for rule in rules do newRules.Add(rule)
    let ruleNames = new ResizeArray<string> () 
    for rule in rules do ruleNames.Add(rule.name.ToString() + rule.body.ToString())
    let neterminals = new ResizeArray<Source.t*Source.t> ()
    let names = new ResizeArray<string*string> ()
    for rule in rules do
        if not (Seq.contains (rule.name.ToString(), rule.name.ToString()) names) then 
            neterminals.Add((rule.name, rule.name))
            names.Add((rule.name.ToString(), rule.name.ToString()))
        let elements = match rule.body with PSeq(e, a, l) -> e
        if elements.Length = 1 && (match elements.[0].rule with PRef(t,_) -> true | _ -> false) && not (Seq.contains ((match elements.[0].rule with PRef(t,_) -> t).ToString(),(match elements.[0].rule with PRef(t,_) -> t).ToString()) names ) && (match elements.[0].rule with PRef(t,_) -> true | _ -> false) then 
            neterminals.Add(((match elements.[0].rule with PRef(t,_) -> t), (match elements.[0].rule with PRef(t,_) -> t)))
            unitRules.Add(rule)
            names.Add(((match elements.[0].rule with PRef(t,_) -> t).ToString(),(match elements.[0].rule with PRef(t,_) -> t).ToString()))
            
    let pairs = findPairs neterminals unitRules
    
    for pair in pairs do
        for rule in rules do
            let elements = match rule.body with PSeq(e, a, l) -> e
            let newRule = TransformAux.createRule (fst pair) rule.args rule.body false rule.metaArgs 
            if ((elements.Length = 2) || (match elements.[0].rule with | PRef(t,_) -> false | _ -> true)) && (snd pair).ToString() = rule.name.ToString() && not (ruleNames.Contains(newRule.name.ToString() + newRule.body.ToString())) then 
                newRules.Add(newRule) 
                ruleNames.Add(newRule.name.ToString() + newRule.body.ToString())
    let toDelete =  new ResizeArray<int> ()
    for rule in newRules do
        let elements = match rule.body with PSeq(e, a, l) -> e
        if elements.Length = 1 && (match elements.[0].rule with PRef(t,_) -> true | _ -> false) then toDelete.Add(newRules.IndexOf(rule))
    for i in (Seq.rev toDelete) do newRules.RemoveAt(i)
    newRules |> Seq.toList 

let deleteNonGenerating (rules: Rule.t<_, _> list) = 
    let generating = System.Collections.Generic.HashSet<string>()
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e |> List.map (fun x -> match x.rule with | PToken t -> true | _ -> false )
        if not (List.contains false elements) && generating.Add(rule.name.ToString()) then ()
    let mutable flag = true
    while flag do
        for rule in rules do
            let elements = match rule.body with PSeq(e, a, l) -> e |> List.filter(fun x -> match x.rule with | PToken t -> false | _ -> true) |> List.map (fun x -> generating.Contains(x.rule.ToString()))
            if not (List.contains false elements) && generating.Add(rule.name.ToString()) then ()
            else flag <- false
    let newRules = new ResizeArray<Rule.t<'a,'b>> (rules)
    let toDelete =  new ResizeArray<int> ()
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e |> List.map (fun x -> match x.rule with | PToken t -> true | _ -> false || generating.Contains(x.rule.ToString()))
        if List.contains false elements then toDelete.Add(newRules.IndexOf(rule))
    for i in (Seq.rev toDelete) do newRules.RemoveAt(i)
    newRules |> Seq.toList

let deleteUnreachable (rules: Rule.t<_, _> list) = 
    let reachable = System.Collections.Generic.HashSet<string>(["s"])
    let mutable flag = true
    while flag do
        for rule in rules do
            let elements = match rule.body with PSeq(e, a, l) -> e  |> List.filter(fun x -> match x.rule with | PToken t -> false | _ -> true) |> List.map (fun x -> (x.rule.ToString()))
            if reachable.Contains(rule.name.ToString()) && ((elements.Length = 1 && reachable.Add(elements.[0])) || (elements.Length = 2 && reachable.Add(elements.[0]) && reachable.Add(elements.[1]))) then ()
            else flag <- false
    let newRules = new ResizeArray<Rule.t<'a,'b>> (rules)
    let toDelete =  new ResizeArray<int> ()
    for rule in rules do
        if not (reachable.Contains(rule.name.ToString())) then toDelete.Add(newRules.IndexOf(rule))
    for i in (Seq.rev toDelete) do newRules.RemoveAt(i)
    newRules |> Seq.toList
        

let deleteUselessRules (rules: Rule.t<_, _> list) = 
    deleteUnreachable (deleteNonGenerating rules)
    
let deleteFewTermRules  (rules: Rule.t<_, _> list) = 
    let newRules = new ResizeArray<Rule.t<'a,'b>> (rules)
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e
        if elements.Length = 2 && (match elements.[0].rule with | PToken t -> true | _ -> false) && (match elements.[1].rule with | PToken t -> true | _ -> false) then
            let n1 = Namer.newSource(rule.name)
            let n2 = Namer.newSource(rule.name)
            let a1 = TransformAux.createDefaultElem(PRef(n1, None))
            let a2 = TransformAux.createDefaultElem(PRef(n2, None))
            newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([a1; a2], None, None)) false rule.metaArgs)
            newRules.Add(TransformAux.createRule n1 rule.args (PSeq([elements.[0]], None, None)) false rule.metaArgs)
            newRules.Add(TransformAux.createRule n2 rule.args (PSeq([elements.[1]], None, None)) false rule.metaArgs)
            if newRules.Remove(rule) then ()
        if elements.Length = 2 && (match elements.[0].rule with | PToken t -> true | _ -> false) && not (match elements.[1].rule with | PToken t -> true | _ -> false) then
            let n1 = Namer.newSource(rule.name)
            let a1 = TransformAux.createDefaultElem(PRef(n1, None))
            newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([a1; elements.[1]], None, None)) false rule.metaArgs)
            newRules.Add(TransformAux.createRule n1 rule.args (PSeq([elements.[0]], None, None)) false rule.metaArgs)
            if newRules.Remove(rule) then ()
        if elements.Length = 2 && not (match elements.[0].rule with | PToken t -> true | _ -> false) &&  (match elements.[1].rule with | PToken t -> true | _ -> false) then
            let n1 = Namer.newSource(rule.name)
            let a1 = TransformAux.createDefaultElem(PRef(n1, None))
            newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[0]; a1], None, None)) false rule.metaArgs)
            newRules.Add(TransformAux.createRule n1 rule.args (PSeq([elements.[1]], None, None)) false rule.metaArgs)
            if newRules.Remove(rule) then ()
    newRules |> Seq.toList
        
let toChomNormForm (ruleList: Rule.t<_, _> list) = 
    ruleList
    |> deleteLongRules
    |> deleteEpsilonRules
    |> deleteUnitRules
    |> deleteUselessRules
    |> deleteFewTermRules
    
type ToChomNormForm() = 
    inherit Conversion()
        override this.Name = "ToChomNormForm"
        override this.ConvertGrammar (grammar,_) = mapGrammar toChomNormForm grammar



                
        
    

