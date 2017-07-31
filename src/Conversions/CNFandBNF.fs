module Yard.Core.Conversions.CNFandBNF

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule
open Microsoft.FSharp.Collections
open TransformAux


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

let mutable startIsEps = false

let rec getElements body pos neg flag = 
    match body with 
    | PSeq(e, a, l) -> 
                    if e.Length > 0 && (match e.[0].rule with PNeg t -> true | _ -> false) 
                    then getElements (match e.[0].rule with PNeg t -> t) pos neg false
                    else
                        if flag 
                        then pos @ [e], neg
                        else pos, neg @ [e]
                    
    | PRef(t, a) -> 
                    if flag 
                    then pos @ [[createDefaultElem(PRef(t, a))]], neg
                    else pos, neg @ [[createDefaultElem(PRef(t, a))]]
    | PToken t -> 
                    if flag 
                    then pos @ [[createDefaultElem(PToken t)]], neg
                    else pos, neg @ [[createDefaultElem(PToken t)]]
    | PConj(a, b) -> fst (getElements a pos neg flag) @ fst (getElements b pos neg flag), snd (getElements a pos neg flag) @ snd (getElements b pos neg flag) 
    | PNeg t -> getElements t pos neg false
     
let rec createConj rules = 
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
            if el.Length < 3 
            then
                if List.contains el (fst elements) then conjRule.Add(createRule rule.name rule.args (PSeq(el, None, None)) rule.isStart rule.metaArgs)
                else conjRule.Add(createRule rule.name rule.args (PNeg  (PSeq(el, None, None))) rule.isStart rule.metaArgs)
            else 
                let newNames = [|for i in 0..el.Length - 3 -> Namer.newSource(rule.name)|]  
                conjRule.Add(createRule rule.name rule.args (PSeq([el.[0]; createDefaultElem (PRef(newNames.[0], None))], None, None)) rule.isStart rule.metaArgs) |> ignore
                for i in 1..el.Length - 3 do
                    newRules.Add(createRule newNames.[i - 1] rule.args (PSeq([el.[i]; createDefaultElem (PRef(newNames.[i], None))], None, None)) false rule.metaArgs) |> ignore
                newRules.Add(createRule newNames.[el.Length - 3] rule.args (PSeq([el.[el.Length - 2]; el.[el.Length - 1]], None, None)) false rule.metaArgs) |> ignore
        newRules.Add(createRule rule.name rule.args (createConj conjRule) rule.isStart rule.metaArgs) |> ignore
    newRules |> Seq.toList

let rec getCombinations conjs =
    match conjs with
    | h::[] -> List.fold (fun acc elem -> [elem]::acc) [] h
    | h::t -> List.fold (fun cacc celem -> (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc) [] (getCombinations t)
    | _ -> []

let collectEpsNonterms (rules: Rule.t<_,_> list) = 
    let epsNonterms = HashSet<_>(new NonTermComparer<_,_>())
    for rule in rules do
        let elementsPos = fst (getElements rule.body [] [] true)
        let elementsNeg = snd (getElements rule.body [] [] true)
        if List.contains [] elementsPos && elementsNeg.IsEmpty
        then epsNonterms.Add(createDefaultElem(PRef(rule.name, None))) |> ignore   
    let mutable flag = true
    while flag do
        let mutable k = epsNonterms.Count
        for rule in rules do
            let checkPos = fst (getElements rule.body [] [] true) |> List.forall (fun x -> List.forall (fun y -> epsNonterms.Contains(y)) x)
            let checkNeg = snd (getElements rule.body [] [] true) |> List.forall (fun x -> List.exists (fun y -> not (epsNonterms.Contains(y))) x)
            if checkPos && checkNeg
            then epsNonterms.Add(createDefaultElem(PRef(rule.name, None))) |> ignore
        if epsNonterms.Count = k then flag <- false
    epsNonterms |> Seq.toList

let getConjs rule epsNonterms = 
    let epsNonterms = epsNonterms |> List.map (fun x -> x.rule.ToString())
    let conjs = HashSet<_>(new RuleComparer<_, _>()) 
    conjs.Add(rule) |> ignore 
    let elements = (fst (getElements rule.body [] [] true)).[0]
    if elements.Length = 2 
    then
        if (List.contains (elements.[0].rule.ToString()) epsNonterms) && (List.contains (elements.[1].rule.ToString()) epsNonterms) 
        then                
            conjs.Add(createRule rule.name rule.args (PSeq([elements.[0]], None, None)) rule.isStart rule.metaArgs) |> ignore
            conjs.Add(createRule rule.name rule.args (PSeq([elements.[1]], None, None)) rule.isStart rule.metaArgs) |> ignore
        else
            if (List.contains (elements.[0].rule.ToString()) epsNonterms) 
            then conjs.Add(createRule rule.name rule.args (PSeq([elements.[1]], None, None)) rule.isStart rule.metaArgs) |> ignore
            if (List.contains (elements.[1].rule.ToString()) epsNonterms) 
            then conjs.Add(createRule rule.name rule.args (PSeq([elements.[0]], None, None)) rule.isStart rule.metaArgs) |> ignore
    conjs.RemoveWhere(fun x -> ((fst (getElements x.body [] [] true)).[0]).IsEmpty) |> ignore
    conjs |> Seq.toList

let deleteEpsilonRules (rules: Rule.t<_,_> list) =
    let epsNonterms = collectEpsNonterms rules
    let newRules = HashSet<_>(rules, new RuleComparer<_, _>()) 
    for rule in rules do
        let conjs = ResizeArray<t<_,_> list>()
        let elements = getElements rule.body [] [] true
        for el in fst elements @ snd elements do
            let c = getConjs (createRule rule.name rule.args (PSeq(el, None, None)) rule.isStart rule.metaArgs) epsNonterms |> Seq.toList   
            if List.contains el (fst elements) then conjs.Add(c) 
            else conjs.Add(c |> List.map (fun x -> createRule x.name x.args (PNeg(x.body)) x.isStart x.metaArgs))
        let combinations = getCombinations (conjs |> Seq.toList) 
        for i in 0..combinations.Length - 1 do
            newRules.Add(createRule rule.name rule.args (createConj combinations.[i]) rule.isStart rule.metaArgs) |> ignore     
    newRules.RemoveWhere(fun x -> List.contains [] (fst (getElements x.body [] [] true))) |> ignore
    let start = (List.find (fun x -> x.isStart = true) rules).name
    if List.contains (createDefaultElem (PRef(start,None))) epsNonterms
    then startIsEps <- true
    newRules |> Seq.map (fun x -> 
                                if (match x.body with PSeq(e ,a, l) -> true | _ -> false) && (match (match x.body with PSeq(e,a,l) -> e).[0].rule with PToken t -> true | _ -> false) && ((match x.body with PSeq(e,a,l) -> e).Length = 1)
                                then x
                                else createRule x.name x.args (PConj(x.body, PNeg(PSeq([], None, None)))) x.isStart x.metaArgs ) 
             |> Seq.toList

let collectNontermsAndUnitRules (rules: Rule.t<_,_> list) = 
    let unitRules = HashSet<_>(new RuleComparer<_, _>())
    let nonterms = HashSet<_>(new NonTermComparer<_,_>())
    for rule in rules do
        nonterms.Add(createDefaultElem(PRef(rule.name, None))) |> ignore
        let elements = getElements rule.body [] [] true
        for el in fst elements @ snd elements do 
            if el.Length = 1 && (match el.[0].rule with PRef(t, _) -> true | _ -> false)
            then 
                nonterms.Add(el.[0]) |> ignore
                unitRules.Add(rule) |> ignore
    nonterms |> Seq.map (fun x -> x.rule.ToString()), unitRules

let filter rule = 
    let el1 = fst (getElements rule.body [] [] true) |> List.toSeq |> Seq.toList |> List.map (fun x -> createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs) 
    let el2 = (snd (getElements rule.body [] [] true) |> List.filter(fun x -> not (x = []))) @ [[]] |> List.toSeq |> Seq.toList |> List.map (fun x -> createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs)
    createRule rule.name rule.args (createConj (el1 @ el2)) rule.isStart rule.metaArgs

let deleteUnitRules (rules: Rule.t<_,_> list) =
    let newRules = HashSet<_>(rules, new RuleComparer<_, _>())
    let oldRules = HashSet<_>(rules, new RuleComparer<_, _>())
    let nonTerms, unitRules = collectNontermsAndUnitRules rules
    let mutable flag = true
    while flag do
        for rule in oldRules do
            if unitRules.Contains(rule) then 
                let elements = getElements rule.body [] [] true
                let unitConjs = List.filter (fun x -> List.length x = 1 && Seq.contains ((x.[0]).rule.ToString()) nonTerms) (fst elements)
                let unitConjsNeg = List.filter (fun x -> List.length x = 1 && Seq.contains ((x.[0]).rule.ToString()) nonTerms) (snd elements)
                for c in unitConjs @ unitConjsNeg do 
                    let rulesToAdd = Seq.filter (fun x -> x.name.text = c.[0].rule.ToString()) oldRules              
                    if List.contains c unitConjs then 
                        newRules.UnionWith(rulesToAdd 
                                           |> Seq.map 
                                              (fun x -> 
                                                       let pos = (List.except([c]) (fst elements) @ fst (getElements x.body [] [] true)) 
                                                                  |> List.map (fun x -> createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs)
                                                       let neg = (snd elements @ snd (getElements x.body [] [] true)) 
                                                                  |> List.map (fun x -> createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs)
                                                       createRule rule.name rule.args (createConj (pos @ neg |> List.toSeq)) rule.isStart rule.metaArgs |> filter))
                    else
                        let conjs = ResizeArray<t<_,_> list>()
                        let pos = fst elements |> List.map (fun x -> createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs)
                        let neg = List.except([c]) (snd elements) |> List.map (fun x -> createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs)                                                       
                        for r in rulesToAdd do
                            let pos2 = snd (getElements r.body [] [] true) |> List.filter (fun x -> not (x = [])) |> List.map (fun x -> createRule r.name r.args (PSeq(x, None, None)) r.isStart r.metaArgs)
                            let neg2 = fst (getElements r.body [] [] true) |> List.map (fun x -> createRule r.name r.args (PNeg(PSeq(x, None, None))) r.isStart r.metaArgs)
                            conjs.Add(pos2 @ neg2) |> ignore
                        let combinations = getCombinations (conjs |> Seq.toList) 
                        for i in 0..combinations.Length - 1 do
                            newRules.Add(filter (createRule rule.name rule.args (createConj (combinations.[i] @ pos @ neg)) rule.isStart rule.metaArgs)) |> ignore                      
                newRules.Remove(rule) |> ignore
        oldRules.Clear()
        oldRules.UnionWith(newRules)
        unitRules.Clear()
        unitRules.UnionWith(snd (collectNontermsAndUnitRules (oldRules|>Seq.toList)))
        if unitRules.Count = 0 then flag <- false   
    newRules |> Seq.toList

let deleteNonGenerating (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_,_>())
    let generating = HashSet<string>()
    for rule in rules do
        let elements = (fst (getElements rule.body [] [] true)).[0] |> List.map (fun x -> match x.rule with | PToken t -> true | _ -> false )
        if not (List.contains false elements)
        then generating.Add(rule.name.ToString()) |> ignore
    let mutable flag = true
    while flag do
        let mutable k = generating.Count
        for rule in rules do
            let elements = (fst (getElements rule.body [] [] true)).[0] |> List.filter (fun x -> match x.rule with | PToken t -> false | _ -> true) |> List.map (fun x -> generating.Contains(x.rule.ToString()))
            if not (List.contains false elements) && not (List.isEmpty elements)
            then generating.Add(rule.name.ToString()) |> ignore
        if generating.Count = k then flag <- false
    newRules.RemoveWhere(fun r -> 
                                let elements = (fst (getElements r.body [] [] true)).[0] |> List.map (fun x -> match x.rule with | PToken t -> true | _ -> false || generating.Contains(x.rule.ToString()))
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
            let elements = (fst (getElements rule.body [] [] true)).[0]  |> List.filter (fun x -> match x.rule with | PToken t -> false | _ -> true) |> List.map (fun x -> (x.rule.ToString()))
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
    let b = Seq.filter (fun x -> match x.body with PConj(a,b) -> false |_ -> true) rules
    let a = Seq.filter (fun x -> List.length ((fst (getElements x.body [] [] true)).[0]) = 1
                                 && match ((fst (getElements x.body [] [] true)).[0]).[0].rule with PToken t -> true | _ -> false
                                 && (match ((fst (getElements x.body [] [] true)).[0]).[0].rule with PToken t -> t).text = term.text 
                                 && Seq.filter (fun y -> y.name.text = x.name.text) rules |> Seq.length = 1) b
    if not (Seq.isEmpty a) then (Seq.head a).name
    else Namer.newSource(rule.name)
       
let cutRule rule (rules: HashSet<_>)  = 
    let newRules = HashSet<_>(new RuleComparer<_,_>())
    newRules.Add(rule) |> ignore
    let elements = (fst (getElements rule.body [] [] true)).[0]
    if elements.Length = 2 
    then
        if (match elements.[0].rule with | PToken t -> true | _ -> false) && (match elements.[1].rule with | PToken t -> true | _ -> false)
        then
            let n1, n2 = tryFindRule (match elements.[0].rule with | PToken t -> t) rule rules,  tryFindRule (match elements.[1].rule with | PToken t -> t) rule rules
            let a1, a2 = createDefaultElem(PRef(n1, None)), createDefaultElem(PRef(n2, None))
            newRules.Add(createRule rule.name rule.args (PSeq([a1; a2], None, None)) rule.isStart rule.metaArgs) |> ignore
            newRules.Add(createRule n1 rule.args (PSeq([elements.[0]], None, None)) false rule.metaArgs) |> ignore
            newRules.Add(createRule n2 rule.args (PSeq([elements.[1]], None, None)) false rule.metaArgs) |> ignore
            newRules.Remove(rule) |> ignore
        elif (match elements.[0].rule with | PToken t -> true | _ -> false) && not (match elements.[1].rule with | PToken t -> true | _ -> false) 
        then
            let n1 =  tryFindRule (match elements.[0].rule with | PToken t -> t) rule rules
            let a1 = createDefaultElem(PRef(n1, None))
            newRules.Add(createRule rule.name rule.args (PSeq([a1; elements.[1]], None, None)) rule.isStart rule.metaArgs) |> ignore
            newRules.Add(createRule n1 rule.args (PSeq([elements.[0]], None, None)) false rule.metaArgs) |> ignore
            newRules.Remove(rule)|> ignore
        elif elements.Length = 2 && not (match elements.[0].rule with | PToken t -> true | _ -> false) &&  (match elements.[1].rule with | PToken t -> true | _ -> false) 
        then
            let n1 =  tryFindRule (match elements.[1].rule with | PToken t -> t) rule rules
            let a1 = TransformAux.createDefaultElem(PRef(n1, None))
            newRules.Add(createRule rule.name rule.args (PSeq([elements.[0]; a1], None, None)) rule.isStart rule.metaArgs) |> ignore
            newRules.Add(createRule n1 rule.args (PSeq([elements.[1]], None, None)) false rule.metaArgs) |> ignore
            newRules.Remove(rule) |> ignore
    newRules |> Seq.toList

let deleteFewTermRules  (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_,_>())
    for rule in rules do
        let elements = getElements rule.body [] [] true
        let conjRule = ResizeArray<_>()
        for el in fst elements @ snd elements do 
            if not (el = []) 
            then
                let res = cutRule (createRule rule.name rule.args (PSeq(el, None, None)) rule.isStart rule.metaArgs) newRules         
                newRules.UnionWith(List.filter (fun x -> not (x.name.text = rule.name.text)) res)
                if List.contains el (fst elements) then conjRule.Add(List.find (fun x -> x.name.text = rule.name.text) res)
                else conjRule.Add(createRule rule.name rule.args (PNeg (List.find (fun x -> x.name.text = rule.name.text) res).body) rule.isStart rule.metaArgs)
            else conjRule.Add(createRule rule.name rule.args (PNeg(PSeq([], None, None))) rule.isStart rule.metaArgs)
        newRules.Remove(rule) |> ignore
        newRules.Add(createRule rule.name rule.args (createConj conjRule) rule.isStart rule.metaArgs) |> ignore
    newRules |> Seq.toList

let filterAndStuff (rules: Rule.t<_,_> list) = 
    let newRules = HashSet<_>(rules, new RuleComparer<_,_>())
    let toAdd = HashSet<_>(new RuleComparer<_,_>())
    for rule in rules do
        let elements = getElements rule.body [] [] true
        for el in fst elements do 
            if (fst elements).Length > 1 && el.Length = 1 && match el.[0].rule with PToken t -> true | _ -> false 
            then newRules.Remove(rule) |> ignore
            if (fst elements).Length = 1 && (snd elements).Length > 1 && el.Length = 1 && match el.[0].rule with PToken t -> true | _ -> false 
            then 
                newRules.Remove(rule) |> ignore
                newRules.Add(createRule rule.name rule.args (PSeq([el.[0]], None, None)) rule.isStart rule.metaArgs) |> ignore
        for el in snd elements  do 
            if (fst elements).Length > 0 && el.Length = 1 && match el.[0].rule with PToken t -> true | _ -> false 
            then 
                newRules.Remove(rule) |> ignore
                let b1 = (fst elements) |> List.map (fun x -> createRule rule.name rule.args (PSeq(x, None, None)) rule.isStart rule.metaArgs)  
                let b2 = (snd elements) |> List.filter (fun x -> not (x = el)) |> List.map (fun x -> createRule rule.name rule.args (PNeg(PSeq(x, None, None))) rule.isStart rule.metaArgs)
                if b2.Length = 1 && b1.Length = 1 && (match b1.[0].body with PSeq(e ,a, l) -> true | _ -> false) && (match (match b1.[0].body with PSeq(e,a,l) -> e).[0].rule with PToken t -> true | _ -> false) && ((match b1.[0].body with PSeq(e,a,l) -> e).Length = 1)
                then newRules.Add(b1.[0]) |> ignore
                else newRules.Add(createRule rule.name rule.args (createConj (b1 @ b2)) rule.isStart rule.metaArgs) |> ignore
            if (fst elements).Length = 0 && (snd elements).Length > 2 && el.Length = 1 && match el.[0].rule with PToken t -> true | _ -> false 
            then toAdd.Add(rule) |> ignore
        if not (Seq.isEmpty (Seq.filter (fun x -> 
                                  let el1, el2 = fst (getElements x.body [] [] true), snd(getElements x.body [] [] true) 
                                  if not (x = rule)
                                        && x.name.text = rule.name.text 
                                            && not (List.contains false (el1 |> List.map (fun x -> List.contains x (fst elements))))
                                                && not (List.contains false (el2 |> List.map (fun x -> List.contains x (snd elements))))
                                  then true
                                  else false) newRules)) 
        then newRules.Remove(rule) |> ignore 
  //  for rule in toAdd do
    //    let nt = snd(getElements rule.body [] [] true) 
      //  let c = (fst (collectNontermsAndUnitRules (newRules |> Seq.toList))) |> Seq.filter (fun x -> )
    if startIsEps then 
        let start = (Seq.find (fun x -> x.isStart = true) newRules).name
        newRules.Add(createRule start [] (PSeq([], None, None)) true [] ) |> ignore
    newRules |> Seq.toList

let cutNonEps (rules: Rule.t<_,_> list) = 
    rules 
    |> List.map 
       (fun x ->
                let elements = getElements x.body [] [] true
                if List.contains [] (snd elements)
                then 
                    let newBody = (fst elements @ snd elements) 
                                    |> List.filter (fun y -> not (List.isEmpty y))
                                    |> List.map 
                                        (fun y -> 
                                                 if List.contains y (fst elements) 
                                                 then createRule x.name x.args (PSeq(y, None, None)) x.isStart x.metaArgs 
                                                 else createRule x.name x.args (PNeg(PSeq(y, None, None))) x.isStart x.metaArgs)
                    createRule x.name x.args (createConj newBody) x.isStart x.metaArgs
                else x)  

let toCNFandBNF (rules: Rule.t<_,_> list) conversionType =
    match conversionType with
    | "CNF" -> rules |> deleteLongRules |> deleteEpsilonRules |> deleteUnitRules |> deleteUselessRules |> deleteFewTermRules |> filterAndStuff |> cutNonEps
    | "BNFconj" -> rules |> deleteLongRules |> deleteEpsilonRules |> deleteUnitRules |> deleteFewTermRules |> filterAndStuff |> cutNonEps 
    | "BNFbool" -> rules |> deleteLongRules |> deleteEpsilonRules |> deleteUnitRules |> deleteFewTermRules |> filterAndStuff
    
type CNF() = 
    inherit Conversion()
        override this.Name = "CNF"
        override this.ConvertGrammar (grammar, _) = mapGrammar (fun g -> toCNFandBNF g "CNF") grammar

type BNFconj() = 
    inherit Conversion()
        override this.Name = "BNFconj"
        override this.ConvertGrammar (grammar, _) = mapGrammar (fun g -> toCNFandBNF g "BNFconj") grammar

type BNFbool() = 
    inherit Conversion()
        override this.Name = "BNFbool"
        override this.ConvertGrammar (grammar, _) = mapGrammar (fun g -> toCNFandBNF g "BNFbool") grammar