module Yard.Core.Conversions.ToChomNormForm

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule
open Microsoft.FSharp.Collections

type NonTermComparer<'t1,'t2 when 't1:equality and 't2:equality>() =
    interface IEqualityComparer<Production.elem<'t1,'t2>> with
        member this.Equals(x, y) = match x.rule, y.rule with PRef (x, _), PRef (y, _) -> x.text = y.text | _ -> false
        member this.GetHashCode x = match x.rule with PRef (x, _) -> hash x.text | _ -> hash x

type PairComparer<'t1,'t2 when 't1:equality and 't2:equality>() =
    interface IEqualityComparer<Production.elem<'t1,'t2> * Production.elem<'t1,'t2>> with
        member this.Equals((x1, x2), (y1, y2)) = match (x1.rule,x2.rule), (y1.rule,y2.rule) with (PRef (x1, _), PRef (x2, _)), (PRef (y1, _), PRef (y2, _)) -> x1.text = y1.text && x2.text = y2.text | _ -> false
        member this.GetHashCode x = match (fst x).rule, (snd x).rule with PRef (x1, _), PRef(x2, _) -> hash (x1.text + x2.text) | _ -> hash (fst x) + hash (snd x)

type RuleComparer<'t1,'t2 when 't1:equality and 't2:equality>() =
    interface IEqualityComparer<Rule.t<'t1,'t2>> with
        member this.Equals(x, y) = (x.name.ToString() + x.body.ToString() = y.name.ToString() + y.body.ToString())
        member this.GetHashCode x = hash (x.name.ToString() + x.body.ToString())

let getElements rule = 
    match rule.body with 
    | PSeq(e, a, l) -> e 
    | PRef(t, a) -> [TransformAux.createDefaultElem(PRef(t, a))]
    | PToken t -> [TransformAux.createDefaultElem(PToken t)]

let deleteLongRules (rules: Rule.t<_,_> list) =
    let newRules = HashSet<_>(new RuleComparer<_,_>())
    for rule in rules do
        let elements = getElements rule
        if elements.Length < 3 
        then newRules.Add(rule) |> ignore
        else 
            let newNames = [|for i in 0..elements.Length - 3 -> Namer.newSource(rule.name)|]  
            newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[0]; TransformAux.createDefaultElem (PRef(newNames.[0], None))], None, None)) rule.isStart rule.metaArgs) |> ignore
            for i in 1..elements.Length - 3 do
                newRules.Add(TransformAux.createRule newNames.[i - 1] rule.args (PSeq([elements.[i]; TransformAux.createDefaultElem (PRef(newNames.[i], None))], None, None)) false rule.metaArgs) |> ignore
            newRules.Add(TransformAux.createRule newNames.[elements.Length - 3] rule.args (PSeq([elements.[elements.Length - 2]; elements.[elements.Length - 1]], None, None)) false rule.metaArgs) |> ignore
    newRules |> Seq.toList

let collectEpsNonterms (rules: Rule.t<_,_> list) = 
    let epsNonterms = HashSet<_>(new NonTermComparer<_,_>())
    for rule in rules do
        let elements = getElements rule
        if elements.IsEmpty 
        then epsNonterms.Add(TransformAux.createDefaultElem(PRef(rule.name, None))) |> ignore   
    let mutable flag = true
    while flag do
        let mutable k = epsNonterms.Count
        for rule in rules do
            let elements = getElements rule |> List.map (fun x -> epsNonterms.Contains(x))
            if not (elements.IsEmpty) && not (List.contains false elements)
            then epsNonterms.Add(TransformAux.createDefaultElem(PRef(rule.name, None))) |> ignore
        if epsNonterms.Count = k then flag <- false
    epsNonterms |> Seq.toList

let deleteEpsilonRules (rules: Rule.t<_, _> list) = 
    let epsNonterms = collectEpsNonterms rules |> List.map (fun x -> x.rule.ToString())
    let newRules = HashSet<_>(rules, new RuleComparer<_, _>())  
    for rule in rules do
        let elements = getElements rule
        if elements.Length = 2 
        then
            if (List.contains (elements.[0].rule.ToString()) epsNonterms) && (List.contains (elements.[1].rule.ToString()) epsNonterms) 
            then                
                newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[0]], None, None)) rule.isStart rule.metaArgs) |> ignore
                newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[1]], None, None)) rule.isStart rule.metaArgs) |> ignore
            else
                if (List.contains (elements.[0].rule.ToString()) epsNonterms) 
                then newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[1]], None, None)) rule.isStart rule.metaArgs) |> ignore
                if (List.contains (elements.[1].rule.ToString()) epsNonterms) 
                then newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[0]], None, None)) rule.isStart rule.metaArgs) |> ignore
//если выводима пустая строка
//    let start = (List.find (fun x -> x.name.text = "s") rules).name
//    if List.contains start.text epsNonterms
//    then        
//        let newStart = Namer.newSource(start)
//        newRules.Add(TransformAux.createRule newStart [] (PSeq([TransformAux.createDefaultElem(PRef(start, None))], None, None)) true []) |> ignore
//        newRules.Add(TransformAux.createRule start [] (PSeq([], None, None)) false []) |> ignore
    newRules.RemoveWhere(fun x -> (getElements x).IsEmpty) |> ignore
    newRules |> Seq.toList

let findPairs (nonterms: HashSet<_*_>) (rules: HashSet<_>) = 
    let pairs = HashSet<_>(nonterms, new PairComparer<_,_>())
    let queue = new Queue<_*_>(nonterms)
    while queue.Count > 0 do
        let q = queue.Dequeue()
        for rule in rules do
            let elements = getElements rule
            let el = TransformAux.createDefaultElem(PRef(match elements.[0].rule with PRef(t, _) -> t, None))    
            if (snd q).rule.ToString() = rule.name.ToString() then 
                if pairs.Add((fst q, el)) then
                    queue.Enqueue((fst q, el))
    pairs 

let deleteUnitRules (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_, _>())
    let unitRules = HashSet<_>(new RuleComparer<_, _>())
    let nonterms = HashSet<_*_>(new PairComparer<_,_>())
    for rule in rules do
        nonterms.Add(TransformAux.createDefaultElem(PRef(rule.name, None)), TransformAux.createDefaultElem(PRef(rule.name, None))) |> ignore
        let elements = getElements rule
        if elements.Length = 1 && match elements.[0].rule with PRef(t, _) -> true | _ -> false
        then 
            nonterms.Add(elements.[0], elements.[0]) |> ignore
            unitRules.Add(rule) |> ignore
    let pairs = findPairs nonterms unitRules
    
    for pair in pairs do
        let name = match (fst pair).rule with PRef(t, _) -> t 
        for rule in rules do
            if (snd pair).rule.ToString() = rule.name.text && not (unitRules.Contains(rule))
            then
                let elements = getElements rule
                let startFlag = newRules |> Seq.exists (fun x -> x.name = name && x.isStart)
                let newRule = TransformAux.createRule name rule.args rule.body startFlag rule.metaArgs 
                newRules.Add(newRule) |> ignore

    newRules.RemoveWhere(fun x -> (getElements x).Length = 1 && (match (getElements x).[0].rule with PRef(t, _)  -> true | _ -> false)) |> ignore
    newRules |> Seq.toList 

let deleteNonGenerating (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_,_>())
    let generating = HashSet<string>()
    for rule in rules do
        let elements = getElements rule |> List.map (fun x -> match x.rule with | PToken t -> true | _ -> false )
        if not (List.contains false elements)
        then generating.Add(rule.name.ToString()) |> ignore
    let mutable flag = true
    while flag do
        let mutable k = generating.Count
        for rule in rules do
            let elements = getElements rule |> List.filter(fun x -> match x.rule with | PToken t -> false | _ -> true) |> List.map (fun x -> generating.Contains(x.rule.ToString()))
            if not (List.contains false elements) && not (List.isEmpty elements)
            then generating.Add(rule.name.ToString()) |> ignore
        if generating.Count = k then flag <- false
    newRules.RemoveWhere(fun r -> 
                                let elements = getElements r |> List.map (fun x -> match x.rule with | PToken t -> true | _ -> false || generating.Contains(x.rule.ToString()))
                                List.contains false elements) |> ignore
    newRules |> Seq.toList

let deleteUnreachable (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_,_>())
    let start = (List.find (fun x -> x.isStart) rules).name.text
    let reachable = HashSet<string>([start])
    let mutable flag = true
    while flag do
        let mutable k = reachable.Count
        for rule in rules do
            let elements = getElements rule  |> List.filter (fun x -> match x.rule with | PToken t -> false | _ -> true) |> List.map (fun x -> (x.rule.ToString()))
            if reachable.Contains(rule.name.ToString()) && 
                ((elements.Length = 1 && reachable.Add(elements.[0])) 
                    || (elements.Length = 2 && reachable.Add(elements.[0]) && reachable.Add(elements.[1]))
                    || (elements.Length = 2 && (reachable.Add(elements.[0]) || reachable.Add(elements.[1])))) then ()
        if reachable.Count = k then flag <- false
    newRules.RemoveWhere(fun r -> not (reachable.Contains(r.name.ToString()))) |> ignore
    newRules |> Seq.toList
        
let deleteUselessRules (rules: Rule.t<_, _> list) = 
    deleteUnreachable (deleteNonGenerating rules)
 
let tryFindRule (term: Source.t) rule (rules: HashSet<_>) = 
    let a = Seq.filter (fun x -> List.length (getElements x) = 1
                                && match (getElements x).[0].rule with PToken t -> true | _ -> false 
                                && (match (getElements x).[0].rule with PToken t -> t).text = term.text 
                                && Seq.filter (fun y -> y.name.text = x.name.text) rules |> Seq.length = 1) rules
    if not (Seq.isEmpty a) then (Seq.head a).name
    else Namer.newSource(rule.name)

       
let deleteFewTermRules  (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_,_>())
    for rule in rules do
        let elements = getElements rule
        if elements.Length = 2 
        then
            if (match elements.[0].rule with | PToken t -> true | _ -> false) && (match elements.[1].rule with | PToken t -> true | _ -> false)
            then
                let n1, n2 = tryFindRule (match elements.[0].rule with | PToken t -> t) rule newRules,  tryFindRule (match elements.[1].rule with | PToken t -> t) rule newRules
                let a1, a2 = TransformAux.createDefaultElem(PRef(n1, None)), TransformAux.createDefaultElem(PRef(n2, None))
                newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([a1; a2], None, None)) rule.isStart rule.metaArgs) |> ignore
                newRules.Add(TransformAux.createRule n1 rule.args (PSeq([elements.[0]], None, None)) false rule.metaArgs) |> ignore
                newRules.Add(TransformAux.createRule n2 rule.args (PSeq([elements.[1]], None, None)) false rule.metaArgs) |> ignore
                newRules.Remove(rule) |> ignore
            elif (match elements.[0].rule with | PToken t -> true | _ -> false) && not (match elements.[1].rule with | PToken t -> true | _ -> false) 
            then
                let n1 =  tryFindRule (match elements.[0].rule with | PToken t -> t) rule newRules
                let a1 = TransformAux.createDefaultElem(PRef(n1, None))
                newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([a1; elements.[1]], None, None)) rule.isStart rule.metaArgs) |> ignore
                newRules.Add(TransformAux.createRule n1 rule.args (PSeq([elements.[0]], None, None)) false rule.metaArgs) |> ignore
                newRules.Remove(rule)|> ignore
            elif elements.Length = 2 && not (match elements.[0].rule with | PToken t -> true | _ -> false) &&  (match elements.[1].rule with | PToken t -> true | _ -> false) 
            then
                let n1 =  tryFindRule (match elements.[1].rule with | PToken t -> t) rule newRules
                let a1 = TransformAux.createDefaultElem(PRef(n1, None))
                newRules.Add(TransformAux.createRule rule.name rule.args (PSeq([elements.[0]; a1], None, None)) rule.isStart rule.metaArgs) |> ignore
                newRules.Add(TransformAux.createRule n1 rule.args (PSeq([elements.[1]], None, None)) false rule.metaArgs) |> ignore
                newRules.Remove(rule) |> ignore
    newRules |> Seq.toList

let toChomNormForm (rules: Rule.t<_,_> list) = 
    rules
    |> deleteLongRules
    |> deleteEpsilonRules
    |> deleteUnitRules
    |> deleteUselessRules
    |> deleteFewTermRules
    
type ToChomNormForm() = 
    inherit Conversion()
        override this.Name = "ToChomNormForm"
        override this.ConvertGrammar (grammar, _) = mapGrammar toChomNormForm grammar



                
        
    

