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

module Yard.Core.Conversions.ToCNF

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule


//--Разделение длинных правил на правила длины 2 и 1 ------------------------------------------------------------------------

let splitLongRule rules =    
    let newRuleList = ref [] 
    let rec cutRule (rule: Rule.t<_, _>) = 
        let elements = match rule.body with PSeq(e, a, l) -> e | _ -> [] 
        if elements.Length > 2 then

            let revEls = elements |> List.rev
            let ruleBody = PSeq([revEls.Head; revEls.Tail.Head], None, None)
            let newRule = TransformAux.createRule (Namer.newSource(rule.name)) rule.args ruleBody false rule.metaArgs         
            let newelem = TransformAux.createDefaultElem (PRef(newRule.name, None)) 
            newRuleList := !newRuleList @ [newRule]
            let changedRule = ((revEls).Tail.Tail |> List.rev) @ [newelem] 
  
            cutRule // рекурсивный заход текущего правила уже с новым нетерминалом в правой части 
                {   rule with body = PSeq(changedRule, (match rule.body with PSeq(e, a, l) -> a | _ -> None),
                                                       (match rule.body with PSeq(e, a, l) -> l | _ -> None))
                }

         else [rule]

    (rules |> List.collect (fun rule -> cutRule rule)) @ !newRuleList 

let deleteTrashRule rulesList = 
     let trashFilter rule =  
        let elements = match rule.body with PSeq(e, _, _) -> e | _ -> []
        if elements.Length = 1 then 
                match elements.Head.rule with
                | PRef (e,_) -> rule.name.text.Equals(e.text) |> not
                | _ -> true
            else true
     rulesList |> List.filter(fun rule -> trashFilter rule)


//--Функция для удаления эпсилон-правил------------------------------------------------------------

let deleteEpsRule (ruleList: Rule.t<_,_> list) =
    // Все подмножества [1..N]
    let genSubsets N =
        [1 .. (1 <<< N) - 1]
        |> List.map (fun num ->
            [1..N] |> List.filter (fun i ->
                (num &&& (1 <<< (i-1))) <> 0
            )
        )  

    // Список всех правил
    let epsList = 
        ruleList |> List.collect
            (fun rule -> 
                match rule.body with
                | PSeq(elements, actionCode, lbl) when elements.IsEmpty -> [rule.name.text]
                | _ -> []
            )

    // Проверка вхождения эпсилон-правила
    let isEps s = epsList |> List.filter ((=) s)
    
    //Список эпсилон-правил входящих в данное правило  
    let rec epsInRule elements = 
            elements |> List.collect
                        (fun elem ->
                            match elem.rule with
                            | PSeq(e, a, l) -> epsInRule e
                            | PRef(t, _) -> isEps t.text
                            | _ -> []
                        )
    
    //Функция для добавления нового правила
    let newRule (rule: Rule.t<_, _>) (epsRef: list<_>) =         
        if not epsRef.IsEmpty then
            let numberEpsRef = genSubsets epsRef.Length
            let ac,lbl = match rule.body with PSeq(e, a, l) -> a,l | x -> None,None
            let i = ref 0
            let newBody elements =
                elements 
                |> List.collect
                    (fun elem ->
                        match elem.rule with
                        | PRef(t, _) when t.text |> isEps |> List.isEmpty |> not ->
                                    incr i
                                    [TransformAux.createSimpleElem (PRef(Source.t(string !i), None)) elem.binding]
                        | _ -> [elem]
                    )
            let numberBody =
                match rule.body with
                |PSeq(elements, _, _) -> 
                    PSeq(newBody elements, ac, lbl)
                |_ -> rule.body
            let rulename = rule.name

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
                            | PRef(t,_) when System.Int32.TryParse t.text |> fst && not <| epsWithNameExists t.text -> 
                                      [TransformAux.createSimpleElem (PRef(Source.t(epsRef.[int t.text - 1]), None)) elem.binding]
                            | _ -> [elem])
                [{numberRule with body=PSeq(newBody, ac, lbl)}]
            let numberRule = {rule with body=numberBody}
            numberEpsRef |> List.collect (addRule numberRule)
        else []     
            
    //Добавляем новые правила
    ruleList |> List.collect
        (fun rule ->
            match rule.body with
            | PSeq(elements, actionCode, lbl) when not elements.IsEmpty ->
                newRule rule (epsInRule elements) @ [rule]
            | _ -> []
        )
    |> List.filter (fun r -> not <| match r.body with PSeq([],_,_) -> true | _ -> false) |> deleteTrashRule

//--Функция для удаления цепных правил--------------------------------------------------------------------------------------------
                
let deleteChainRule (ruleList: Rule.t<_, _> list) = 
    
    let rec newRule (mainRule: Rule.t<_, _>) name =
        ruleList |> List.collect
            (fun rule ->
                let isOneRule rule =
                    match rule.body with
                    | PSeq(elements, actionCode, lbl) when elements.Length = 1 &&
                                 (match elements.Head.rule with PRef(t,_) -> true | _ -> false) -> true
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
            | PSeq(elements, actionCode, lbl) when elements.Length = 1 && (match elements.Head.rule with PRef(_, _) -> true | _ -> false) -> 
                newRule rule (match elements.Head.rule with PRef(t, _) -> t.text | _ -> "")
            | _ -> [rule]
        )

//--Переименование терминалов в нетерминалы в неподходящих правилах (вида s -> AB, s -> Ab, s -> bA)-------------------

let renameTerm ruleList = 
    let isToken (elem: elem<_,_>) = match elem.rule with PToken _ -> true | _ -> false
    let isRef (elem: elem<_,_>) = match elem.rule with PRef(_,_) -> true | _ -> false
    let isCNF (rule: Rule.t<_,_>) = 
        match rule.body with
        | PSeq(elements,_,_) when elements.Length = 1 -> true 
        | PSeq(elements,_,_) when elements.Length = 2 && isRef elements.[0] && isRef elements.[1] -> true 
        | PSeq([],_, _) when rule.isStart -> true 
        | _ -> false
       // | _ -> failwith "Error"

    let newRuleList = ref []
    let renameRule (rule: Rule.t<_, _>) = 
        let rename (elem: elem<_, _>) = 
            if isToken elem then 
                let newRuleName = Source.t("new_" + (match elem.rule with PToken t -> t.text | _ -> ""))
                if not (!newRuleList |> List.exists (fun rl -> rl.name = newRuleName)) then 
                    let newRule = TransformAux.createRule newRuleName rule.args 
                                                          (PSeq([elem], None, None)) false rule.metaArgs
                    newRuleList := !newRuleList @ [newRule]

                TransformAux.createSimpleElem (PRef(newRuleName, None)) elem.binding //новый элемент

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
        (fun rule -> if isCNF rule then [rule] else renameRule rule)) @ !newRuleList
   
//--Получить НФХ--------------------------------------------------------------------------------------------------------

let toCNF (ruleList: Rule.t<_, _> list) = 
    ruleList
    |> splitLongRule
    |> deleteEpsRule 
    |> deleteChainRule
    |> renameTerm

//------------------------------------------------------------------------------------------------------------------------



do()


type SplitLongRule() = 
    inherit Conversion()
        override this.Name = "SplitLongRule"
        override this.ConvertGrammar (ruleList,_) = mapGrammar splitLongRule ruleList


type DeleteEpsRule() = 
    inherit Conversion()
        override this.Name = "DeleteEpsRule"
        override this.ConvertGrammar (grammar,_) = mapGrammar deleteEpsRule grammar


type DeleteChainRule() = 
    inherit Conversion()
        override this.Name = "DeleteChainRule"
        override this.ConvertGrammar (grammar,_) = mapGrammar deleteChainRule grammar


type RenameTerm() = 
    inherit Conversion()
        override this.Name = "RenameTerm"
        override this.ConvertGrammar (grammar,_) = mapGrammar renameTerm grammar


type ToCNF() = 
    inherit Conversion()
        override this.Name = "ToCNF"
        override this.ConvertGrammar (grammar,_) = mapGrammar toCNF grammar