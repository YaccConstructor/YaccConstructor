//  Module ReplaceInline replace nonterminals, from which only
//    Pref, PToken or PLiteral can be infered, whit their right sides.
//
//  Copyright 2011, 2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//  This is war
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

module Yard.Core.Conversions.ToCNF

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule

//--Функция для удаления эпсилон-правил------------------------------------------------------------

let deleteEpsRule (ruleList: Rule.t<_,_> list) = 
    
    /// Generate all subsets of [1..N]
    let genSubsets N =
        [1 .. (1 <<< N) - 1]
        |> List.map (fun num ->
            [1..N] |> List.filter (fun i ->
                (num &&& (1 <<< (i-1))) <> 0
            )
        )
        
    // Find all epsilon-rules
    let epsList = 
        ruleList |> List.collect
            (fun rule -> 
                match rule.body with
                | PSeq(elements, actionCode, lbl) when elements.IsEmpty -> [rule.name.text]
                | x -> []
            )

    //--Функция для проверки вхождения эпсилон-правила---------------------------------------------

    let isEps s = epsList |> List.filter ((=) s)
    
    //--Список эпсилон-правил входящих в данное правило--------------------------------------------     

    let rec epsInRule elements = 
        elements |> List.collect
                    (fun elem ->
                        match elem.rule with
                        | PSeq(e, a, l) -> epsInRule e
                        | PRef(t, _) -> isEps t.text
                        | _ -> []
                    )
                        
    //--Функция для добавления нового правила------------------------------------------------------

    let newRule (rule: Rule.t<_, _>) (epsRef: list<string>) =         
        if not epsRef.IsEmpty then
            let numberEpsRef = genSubsets epsRef.Length
            let ac,lbl = match rule.body with PSeq(e, a, l) -> a,l | x -> None,None
            let i = ref 0
            let newBody elements =
                elements 
                |> List.collect
                    (fun elem ->
                        match elem.rule with
                        | PRef(t, _) when t.text |> isEps |> List.isEmpty |> not-> 
                            incr i
                            [{
                                omit = elem.omit
                                rule = PRef(Source.t(string !i), None)
                                binding = elem.binding
                                checker = elem.checker
                            }]
                        | _ -> [elem]
                    )
            let numberBody =                
                match rule.body with
                |PSeq(elements, _, _) -> 
                    PSeq(newBody elements, ac, lbl)
                |_ -> rule.body
            let addRule (numberRule: Rule.t<_, _>) eps =
                let epsWithNameExists t = 
                    eps
                    |> List.map string
                    |> List.exists ((=) t)
                let ac,lbl = match numberRule.body with PSeq(e, a, l) -> a,l | _ -> None,None
                let newBody = 
                    match numberRule.body with PSeq(e, a, l) -> e | _ -> []
                    |> List.collect
                        (fun elem ->
                            match elem.rule with
                            | PRef(t,_) when epsWithNameExists t.text -> []
                            | PRef(t,_) when not <| epsWithNameExists t.text -> 
                                    [{
                                        omit = elem.omit
                                        rule = PRef(Source.t(epsRef.[int t.text - 1]), None)
                                        binding = elem.binding
                                        checker = elem.checker
                                    }]
                            | _ -> [elem])
                [{numberRule with body=PSeq(newBody, ac, lbl)}]
            let numberRule = {rule with body=numberBody}
            numberEpsRef |> List.collect (addRule numberRule)
        else []
            
            
    //--Добавляем новые правила--------------------------------------------------------------------
    
    ruleList |> List.collect
        (fun rule ->
            match rule.body with
            | PSeq(elements, actionCode, lbl) when not elements.IsEmpty ->
                newRule rule (epsInRule elements) @ [rule]
            | _ -> []
        )

//--Функция для удаления цепных правил-------------------------------------------------------------
                
let deleteChainRule (ruleList: Rule.t<_, _> list) = 
    
    let rec newRule (mainRule: Rule.t<_, _>) name =
        ruleList |> List.collect
            (fun rule ->
                let isOneRule rule =
                    match rule.body with
                    | PSeq(elements, actionCode, lbl)
                        when elements.Length = 1
                        && (match elements.Head.rule with PRef(t, _) -> true | x -> false) -> true
                    | _ -> false
                let label (rl: Rule.t<_, _>) = match rl.body with PSeq(_, _, l) -> l | _ -> None
                let bodyChange (mR: Rule.t<_, _>) (r: Rule.t<_, _>) =
                    match label mR with
                    | None -> r.body
                    | Some _ as labelMR ->
                        if label r = None then
                            match r.body with
                            | PSeq(e, a, _) -> e, a, labelMR
                            | _ -> [], None, labelMR
                            |> PSeq
                        else
                            printfn "label1 and label2 confict"
                            match r.body with
                            | PSeq(e, a, _) -> e, a, labelMR
                            | _ -> [], None, labelMR
                            |> PSeq
                if rule.name.text = name then
                    if isOneRule rule then
                        match rule.body with
                        | PSeq(elements, actionCode, lbl) -> match elements.Head.rule with PRef(t, _) -> t.text | _ -> ""
                        | _ -> ""
                        |> newRule mainRule
                    else
                        [{mainRule with body = bodyChange mainRule rule}] 
                else []
            )

    ruleList |> List.collect
        (fun rule -> 
            match rule.body with
            | PSeq(elements, actionCode, lbl) 
                when elements.Length = 1
                && (match elements.Head.rule with PRef(t, _) -> true | _ -> false) -> 
                newRule rule (match elements.Head.rule with PRef(t, _) -> t.text | _ -> "")
            | _ -> [rule]
        )

//--Переименование терминалов в нетерминалы в неподходящих правилах---------------------------

let renameTerm ruleList = 
    
    let isToken (elem: elem<_,_>) = match elem.rule with PToken _ -> true | _ -> false
    let isRef (elem: elem<_,_>) = match elem.rule with PRef(_,_) -> true | _ -> false
    let isCNF (rule: Rule.t<_,_>) = 
        match rule.body with
        | PSeq(elements,_,_) when elements.Length = 1 && isToken elements.Head -> true
        | PSeq(elements,_,_) when elements.Length = 2 && isRef elements.[0] && isRef elements.[1] -> true
        | _ -> false

    let list1 = ref []
    let renameRule (rule: Rule.t<_, _>) = 
        let rename (elem: elem<_, _>) = 
            if isToken elem then 
                let newName = Source.t("new_" + (match elem.rule with PToken t -> t.text | _ -> ""))
                if not (!list1 |> List.exists (fun rl -> rl.name.text = newName.text)) then
                    list1 :=
                        [{
                            name = newName
                            args = rule.args
                            isStart = false
                            isPublic = false
                            metaArgs = rule.metaArgs 
                            body = PSeq([elem], None, None)
                        }] @ !list1
                {
                    omit = elem.omit
                    binding=elem.binding
                    checker=elem.checker
                    rule=PRef(newName, None)
                }
            else elem
        let elements = match rule.body with PSeq(e, a, l) -> e | x -> []
        let elems = [rename elements.[0]; rename elements.[1]]
        [{rule with
            body =
                match rule.body with
                | PSeq(e, a, l) -> elems, a, l
                | _ -> elems, None, None
                |> PSeq
        }]
    
    (ruleList |> List.collect 
        (fun rule -> if isCNF rule then [rule] else renameRule rule)) @ !list1

//--Transform grammar into CNF---------------------------------------------------------------------------

let toCNF (ruleList: Rule.t<_, _> list) = 

    let cnf rules =    
        let i = ref 0
        let list2 = ref []
        let rec newRule (rule: Rule.t<_, _>) =  
            let elements = match rule.body with PSeq(e, a, l) -> e | x -> [] 
            let addRule elem1 elem2 =
                incr i
                list2 :=
                    [{
                        name = Source.t("newCnfRule" + i.Value.ToString())
                        args = rule.args
                        isStart = false
                        isPublic = false
                        metaArgs = rule.metaArgs 
                        body = PSeq([elem1; elem2], None, None)
                    }] @ !list2
                "newCnfRule" + i.Value.ToString()
            let cutRule = 
                if elements.Length > 2 then
                    ((elements |> List.rev).Tail.Tail |> List.rev) @ 
                        [{
                            omit = false
                            rule = PRef(Source.t(addRule elements.[elements.Length - 2] elements.[elements.Length - 1]), None)
                            binding = None
                            checker = None
                        }]
                else []       
            if elements.Length > 2 then
                newRule 
                    {rule with
                        body = PSeq(cutRule, 
                                (match rule.body with PSeq(e, a, l) -> a | x -> None),
                                (match rule.body with PSeq(e, a, l) -> l | x -> None))
                    }
            else 
                [rule]

        (rules |> List.collect newRule) @ !list2

    ruleList
    |> deleteEpsRule
    |> deleteChainRule
    |> cnf
    |> renameTerm

type ToCNF() = 
    inherit Conversion()
        override this.Name = "ToCNF"
        override this.ConvertGrammar (grammar,_) = mapGrammar toCNF grammar

type DeleteEpsRule() = 
    inherit Conversion()
        override this.Name = "DeleteEpsRule"
        override this.ConvertGrammar (grammar,_) = mapGrammar deleteEpsRule grammar

type DeleteChainRule() = 
    inherit Conversion()
        override this.Name = "DeleteChainRule"
        override this.ConvertGrammar (grammar,_) = mapGrammar deleteChainRule grammar
