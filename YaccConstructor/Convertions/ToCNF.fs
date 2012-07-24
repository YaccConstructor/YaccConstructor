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

module Yard.Core.Convertions.ToCNF

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule

//--Функция для удаления эпсилон-правил------------------------------------------------------------

let deleteEpsRule (ruleList: Rule.t<_,_> list) = 
    
    //--Генерация перестановок---------------------------------------------------------------------
        
    let genPermutation N = 
        let genList = ref [[]]
        let addInList list = 
            genList := list @ !genList
            list
        let startList = [for i in [1 .. N] -> [i]]
        genList := startList
        let rec iter listList = 
            if List.length listList <> 1 then
                listList |> List.collect 
                    (fun i -> [List.max i + 1 .. N] |> List.collect (fun j -> [ i @ [j] ])) 
                |> addInList 
                |> iter                  
        iter startList
        !genList
        
    //--Находим все эпсилон-правила----------------------------------------------------------------

    let epsList = 
        ruleList |> List.collect
            (fun rule -> 
                match rule.body with
                |PSeq(elements, actionCode, lbl) when elements.IsEmpty -> [rule.name]
                |x -> []
            )

    //--Функция для проверки вхождения эпсилон-правила---------------------------------------------

    let isEps s = 
        epsList |> List.collect (fun eps -> if s = eps then [eps] else [])
    
    //--Список эпсилон-правил входящих в данное правило--------------------------------------------     

    let rec epsInRule elements = 
        elements |> List.collect
                    (fun elem ->
                        match elem.rule with
                        |PSeq(e, a, l) -> epsInRule e
                        |PRef(t, _) -> isEps (fst t)
                        |x -> []
                    )
                        
    //--Функция для добавления нового правила------------------------------------------------------

    let newRule (rule: Rule.t<_, _>) (epsRef: list<string>) = 
        if not epsRef.IsEmpty then
            let numberEpsRef = genPermutation epsRef.Length 
            let numberBody = 
                let i = ref 0
                match rule.body with
                |PSeq(elements, _, _) -> 
                    PSeq(
                        elements |> List.collect
                            (fun elem ->
                                match elem.rule with
                                |PRef(t, _) when fst t |> isEps |> List.isEmpty |> not-> 
                                    incr i
                                    [{
                                        omit = elem.omit
                                        rule = PRef(( (!i).ToString(), (0, 0)), None)
                                        binding = elem.binding
                                        checker = elem.checker
                                    }]
                                |x -> [elem]
                            ),
                            (match rule.body with PSeq(e, a, l) -> a | x -> None),
                            (match rule.body with PSeq(e, a, l) -> l | x -> None))
                |x -> rule.body
            let addRule (numberRule: Rule.t<_, _>) eps =                
                [{
                    name=numberRule.name
                    args=numberRule.args
                    _public=numberRule._public
                    metaArgs=numberRule.metaArgs
                    body=PSeq(
                                (match numberRule.body with PSeq(e, a, l) -> e | x -> [])
                                |> List.collect 
                                    (fun elem ->
                                        match elem.rule with
                                        |PRef(t, _) when 
                                            eps
                                            |> List.map (fun e -> string e) 
                                            |> List.exists (fun e -> e = fst t) -> []
                                        |PRef(t, _) when 
                                            not (
                                                eps 
                                                |> List.map (fun e -> e.ToString()) 
                                                |> List.exists (fun e -> (e = (fst t)))) -> 
                                             [{
                                                omit = elem.omit
                                                rule = PRef((epsRef.Item(System.Convert.ToInt32(fst t) - 1).ToString(), (0, 0)), None)
                                                binding = elem.binding
                                                checker = elem.checker
                                            }]
                                        |x -> [elem]
                                    )
                                ,
                                (match numberRule.body with PSeq(e, a, l) -> a | x -> None), 
                                (match numberRule.body with PSeq(e, a, l) -> l | x -> None))
                }]
            let numberRule = 
                {
                    name=rule.name
                    args=rule.args
                    _public=rule._public
                    metaArgs=rule.metaArgs
                    body=numberBody
                }
            numberEpsRef |> List.collect
                (fun eps -> addRule numberRule eps)
        else []
            
            
    //--Добавляем новые правила--------------------------------------------------------------------
    
    ruleList |> List.collect
        (fun rule -> 
            match rule.body with
            |PSeq(elements, actionCode, lbl) when not elements.IsEmpty -> 
                newRule rule (epsInRule elements) @ [rule]
            |x -> []
        )

//--Функция для удаления цепных правил-------------------------------------------------------------
                
let deleteChainRule (ruleList: Rule.t<_, _> list) = 
    
    let rec newRule (mainRule: Rule.t<_, _>) name = 
        ruleList |> List.collect
            (fun rule ->

                let isOneRule rule = 
                    match rule.body with
                    |PSeq(elements, actionCode, lbl) 
                        when elements.Length = 1
                        && (match elements.Head.rule with PRef(t, _) -> true | x -> false) -> true
                    |x -> false
                let label (rl: Rule.t<_, _>) = match rl.body with PSeq(_, _, l) -> l | x -> None
                let bodyChange (mR: Rule.t<_, _>) (r: Rule.t<_, _>) = 
                    if label mR = None then r.body
                    else
                        if label r = None then PSeq(
                                                (match r.body with PSeq(e, _, _) -> e | x -> []),
                                                (match r.body with PSeq(_, a, _) -> a | x -> None),
                                                label mR)
                        else 
                            printfn "label1 and label2 confict"
                            PSeq((match r.body with PSeq(e, _, _) -> e | x -> []),
                                 (match r.body with PSeq(_, a, _) -> a | x -> None),
                                 label mR)
                if rule.name = name then 
                    if isOneRule rule then
                        newRule mainRule 
                            (match rule.body with
                             |PSeq(elements, actionCode, lbl) -> (match elements.Head.rule with PRef(t, _) -> fst t | x -> "")
                             |x -> ""
                            )
                    else
                        [{
                            name=mainRule.name
                            args=mainRule.args
                            _public=mainRule._public
                            metaArgs=mainRule.metaArgs
                            body=(bodyChange mainRule rule)
                        }] 
                else
                    []
            )
        

    ruleList |> List.collect
        (fun rule -> 
            match rule.body with
            |PSeq(elements, actionCode, lbl) 
                when elements.Length = 1
                && (match elements.Head.rule with PRef(t, _) -> true | x -> false) -> 
                newRule rule (match elements.Head.rule with PRef(t, _) -> (fst t) | x -> "")
            |x -> [rule]
        )

//--Переименование терминалов в нетерминалы в неподходящих правилах---------------------------

let renameTerm ruleList = 
    
    let isToken (elem: elem<_,_>) = match elem.rule with PToken(_,_) -> true | x -> false
    let isRef (elem: elem<_,_>) = match elem.rule with PRef(_,_) -> true | x -> false
    let isCNF (rule: Rule.t<_,_>) = 
        match rule.body with
        |PSeq(elements,_,_) when elements.Length = 1 && isToken elements.Head -> true
        |PSeq(elements,_,_) when elements.Length = 2 && isRef elements.[0] && isRef elements.[1] -> true
        |x -> false

    let list1 = ref []
    let renameRule (rule: Rule.t<_, _>) = 
        let rename (elem: elem<_, _>) = 
            if isToken elem then 
                let newName = "new_" + (match elem.rule with PToken(t, _) -> t | x -> "")
                if not (!list1 |> List.exists (fun rl -> rl.name = newName)) then
                    list1 :=
                        [{
                            name = newName
                            args = rule.args
                            _public = false
                            metaArgs = rule.metaArgs 
                            body = PSeq([elem], None, None)
                        }] @ !list1
                {
                    omit = elem.omit
                    binding=elem.binding
                    checker=elem.checker
                    rule=PRef((newName, (0, 0)), None)
                }
            else elem
        let elements = match rule.body with PSeq(e, a, l) -> e | x -> []
        [{
            name = rule.name
            args = rule.args
            _public = rule._public
            metaArgs = rule.metaArgs 
            body = PSeq([rename elements.[0]; rename elements.[1]], 
                                (match rule.body with PSeq(e, a, l) -> a | x -> None),
                                (match rule.body with PSeq(e, a, l) -> l | x -> None))
            
        }]
    
    (ruleList |> List.collect 
        (fun rule -> if isCNF rule then [rule] else renameRule rule)) @ !list1

//--Преобразование в КНФ---------------------------------------------------------------------------

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
                        name = "newCnfRule" + (!i).ToString()
                        args = rule.args
                        _public = false
                        metaArgs = rule.metaArgs 
                        body = PSeq([elem1; elem2], None, None)
                    }] @ !list2
                "newCnfRule" + (!i).ToString()
            let cutRule = 
                if elements.Length > 2 then
                    ((elements |> List.rev).Tail.Tail |> List.rev) @ 
                        [{
                            omit = false
                            rule = PRef((addRule elements.[elements.Length - 2] elements.[elements.Length - 1], (0, 0)), None)
                            binding = None
                            checker = None
                        }]
                else []       
            if elements.Length > 2 then
                newRule 
                    {
                        name = rule.name
                        args = rule.args
                        _public = rule._public
                        metaArgs = rule.metaArgs
                        body = PSeq(cutRule, 
                                (match rule.body with PSeq(e, a, l) -> a | x -> None),
                                (match rule.body with PSeq(e, a, l) -> l | x -> None))
                    }
            else 
                [rule]

        ( rules |> List.collect (fun rule -> newRule rule) ) @ !list2

    ruleList
    |> deleteEpsRule
    |> deleteChainRule
    |> cnf
    |> renameTerm

type ToCNF() = 
    inherit Convertion()
        override this.Name = "ToCNF"
        override this.ConvertList (ruleList,_) = toCNF ruleList
        override this.EliminatedProductionTypes = [""]

type DeleteEpsRule() = 
    inherit Convertion()
        override this.Name = "DeleteEpsRule"
        override this.ConvertList (ruleList,_) = deleteEpsRule ruleList
        override this.EliminatedProductionTypes = [""]

type DeleteChainRule() = 
    inherit Convertion()
        override this.Name = "DeleteChainRule"
        override this.ConvertList (ruleList,_) = deleteChainRule ruleList
        override this.EliminatedProductionTypes = [""]

