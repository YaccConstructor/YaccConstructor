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

module Yard.Core.Checkers

open Yard.Core.IL
open System.Collections.Generic
open System.Linq
open Yard.Core.Helpers

let private startRulesCount (def:Yard.Core.IL.Definition<_,_>) =
    def.grammar |> List.sumBy (fun module' -> module'.rules.Count (fun r -> r.isStart))

let IsStartRuleExists def =
    startRulesCount def > 0

let IsSingleStartRule def =
    startRulesCount def = 1

let GetIncorrectMetaArgsCount (def:Yard.Core.IL.Definition<_,_>) =
    let rules = metaRulesTbl def.grammar
    let checkBody module' =
        let map = rules.[module']
        let check acc (name : Source) cnt =
            let expected = 
                if not <| map.ContainsKey name.text 
                then 0
                else (snd map.[name.text]).metaArgs.Length
            if cnt = expected then acc
            else (name, cnt, expected)::acc
        let rec checkBody acc = function
            | PRef (name,_) -> check acc name 0
            | PMetaRef (name, _, metas) -> check acc name metas.Length
            | PAlt (x, y) -> checkBody acc x |> fun acc' -> checkBody acc' y
            | PConj (x, y) -> checkBody acc x |> fun acc' -> checkBody acc' y
            | PSeq (list,_,_) -> list |> List.fold (fun acc elem -> checkBody acc elem.rule) acc
            | PSome x
            | PMany x
            | POpt  x
            | PRepet (x,_,_)
                -> checkBody acc x
            | PLiteral _
            | PToken _
                -> acc
            | PPerm list -> list |> List.fold (fun acc elem -> checkBody acc elem) acc
        checkBody

    def.grammar
    |> List.fold (fun badModules module' ->
        let check = checkBody (getModuleName module')
        let badRules = 
            module'.rules
            |> List.fold (fun acc r -> check acc r.body) []
        match badRules with
        | [] -> badModules
        | list -> (module', List.rev list)::badModules
    ) []

let IsChomskyNormalForm (def:Yard.Core.IL.Definition<_,_>) =
    def.grammar
    |> List.forall (fun module' ->
        module'.rules
        |> List.forall (fun r ->
                match r.body with
                | PSeq([elem],_,_) ->
                    match elem.rule with PToken _ -> true | _ -> false
                | PSeq([e1; e2],_,_) ->
                    match e1.rule, e2.rule with PRef _, PRef _ -> true | _ -> false
                | _ -> false
            )
        )

let private getAllModuleNames (grammar : Grammar<_,_>) =
    grammar
    |> List.map (fun m -> getModuleName m)
    |> List.sort

let GetCoincideModuleNames (def : Yard.Core.IL.Definition<Source, Source>) =
    getAllModuleNames def.grammar
    |> (function
        | [] -> []
        | (x :: xs) -> xs |> List.fold
                              (fun (prev,acc) cur -> (cur, if prev = cur then cur::acc else acc))
                              (x,[])
                       |> snd
       )

let GetInvalidOpenings (def : Yard.Core.IL.Definition<Source, Source>) =
    let existsModule searched =
        def.grammar
        |> List.exists (fun m -> getModuleName m = searched)
    def.grammar
    |> List.choose
        (fun m ->
            let invalidOpenings =
                m.openings
                |> List.filter (fun op -> op.text = getModuleName m || not (existsModule op.text))
            match invalidOpenings with
            | [] -> None
            | _ -> Some (m, invalidOpenings)
        )

let checkModuleRules (publicRules : IDictionary<_,_>) (module' : Module<Source, Source>) = 
    let declaredInnerRules =
        module'.rules |> List.map (fun r -> r.name.text)
    let declaredRules = new HashSet<_>(declaredInnerRules)
    let declaredExportRules =
        module'.openings
        |> List.map (fun op ->
            let rules : Rule<_,_> list =
                if publicRules.ContainsKey op.text then publicRules.[op.text]
                else
                    eprintf "Undeclared module %s (%s:%d) " op.text op.file op.startPos.line
                    []
            op.text, rules
        )
    declaredExportRules
    |> List.iter (snd >> List.iter (fun r -> declaredRules.Add r.name.text |> ignore))

    let repeatedInnerRules =
        let rules = new HashSet<_>()
        let repeated = new HashSet<_>()
        module'.rules
        |> List.iter (fun r -> if not <| rules.Add r.name.text then
                                    ignore <| repeated.Add r.name.text)
        repeated |> List.ofSeq

    let repeatedExportRules =
        let ruleToModule = new Dictionary<_,HashSet<_>>()
        let repeated = new HashSet<_>()
        ((getModuleName module', module'.rules) :: declaredExportRules)
        |> List.iter (fun (mName, rules) ->
                rules |> List.iter (fun r ->
                    let rName = r.name.text
                    if ruleToModule.ContainsKey rName then
                        if not <| ruleToModule.[rName].Add mName then
                            repeated.Add rName |> ignore
                    else
                        ruleToModule.[rName] <- new HashSet<_>([mName])
                )
            )
        //module'.rules
        //|> List.iter (fun r -> if not <| rules.Add r.name.text then
        //                            ignore <| repeated.Add r.name.text)
        repeated |> List.ofSeq |> List.map (fun r -> r, List.ofSeq ruleToModule.[r])

    let undeclaredRules = new HashSet<_>()
    let addUndeclaredRule (name : Source) additionRules = 
        if not (declaredRules.Contains name.text
                || Seq.exists ((=) name.text) additionRules) && name.text <> errorToken
        then
            undeclaredRules.Add name |> ignore

    let rec getUndeclaredRules additionRules body =
        let getUndeclaredRulesCurried body = getUndeclaredRules additionRules body
        match body with
        | PRef (name,_) -> addUndeclaredRule name additionRules
        | PMetaRef (name,_,exprList) ->  
            addUndeclaredRule name additionRules
            exprList |> List.iter getUndeclaredRulesCurried 
        | PSeq (exprList,_,_) -> exprList |> List.iter (fun r -> getUndeclaredRulesCurried r.rule)
        | PPerm exprList    -> exprList |> List.iter getUndeclaredRulesCurried 
        | PRepet (expr,_,_)
        | PMany expr
        | PSome expr
        | POpt  expr -> getUndeclaredRulesCurried expr
        | PAlt (lExpr,rExpr) -> 
            getUndeclaredRulesCurried lExpr
            getUndeclaredRulesCurried rExpr
        | PConj (lExpr,rExpr) -> 
            getUndeclaredRulesCurried lExpr
            getUndeclaredRulesCurried rExpr
        | PLiteral _ 
        | PToken _  -> ()

    module'.rules
    |> List.iter
        (fun r -> 
            let additionRules = new HashSet<_>()
            r.metaArgs |> List.iter (fun i -> additionRules.Add i.text |> ignore)
            getUndeclaredRules additionRules r.body)

    repeatedInnerRules, repeatedExportRules, List.ofSeq undeclaredRules

let GetUndeclaredNonterminalsList (def : Yard.Core.IL.Definition<Source, Source>) =
    let grammar = def.grammar
    let publicRules = getPublicRules grammar
    let filterEmpty (x : ('a * 'b list)  list) =
        x |> List.filter
            (function
             | (_,[]) -> false
             | _ -> true)
    grammar
    |> List.map (fun m -> m, checkModuleRules publicRules m)
    |> List.map (fun (m,(l1,l2,l3)) ->  (m,l1), (m,l2), (m,l3))
    |> List.unzip3
    |> (fun (x,y,z) -> filterEmpty x, filterEmpty y, filterEmpty z)

// returns a list of rule's names which are reachead from start rule in the grammar
let reachableRulesInfo_of_grammar (grammar: Grammar<_,_>) =
    let rulesMap = getRulesMap grammar
    let reachedRules = new HashSet<_>()
    
    let getAdditionRules (rule : Rule<Source,Source>) =
        rule.metaArgs |> List.map (fun i -> i.text)
        |> fun x -> new HashSet<_>(x)

    let rec getReachableRules module' additionRules body : unit =
        let inline getReachableRulesCurried body = getReachableRules module' additionRules body
        match body with
        | PRef (name,_) -> addReachedRule module' name.text additionRules
        | PMetaRef (name,_,exprList) ->  
            addReachedRule module' name.text additionRules
            exprList |> List.iter getReachableRulesCurried 
        | PSeq (exprList,_,_) -> exprList |> List.iter (fun r -> getReachableRulesCurried r.rule)
        | PPerm exprList    -> exprList |> List.iter getReachableRulesCurried
        | PRepet (expr,_,_)
        | PMany expr
        | PSome expr
        | POpt expr -> getReachableRulesCurried expr
        | PAlt (lExpr,rExpr) -> 
            getReachableRulesCurried lExpr
            getReachableRulesCurried rExpr
        | PConj (lExpr,rExpr) -> 
            getReachableRulesCurried lExpr
            getReachableRulesCurried rExpr
        | PLiteral _ 
        | PToken _  -> ()

    and addReachedRule (module' : string) (name : string) (additionRules : HashSet<_>) : unit =  
        let key = module', name
        if not (additionRules.Contains name || reachedRules.Contains key)
        then
            reachedRules.Add key |> ignore
            let newModule = rulesMap.[module'].[name]
            grammar
            |> List.pick (fun m -> if getModuleName m = newModule then Some m.rules else None)
            |> List.find (fun r -> r.name.text = name)
            |> fun rule -> getReachableRules newModule (getAdditionRules rule) rule.body

    let startModule, startRule =
        grammar |> List.pick (fun m ->
            m.rules
            |> List.tryFind (fun r -> r.isStart)
            |> Option.map (fun r -> getModuleName m, r)
        )
    (startModule, startRule.name.text)
    |> reachedRules.Add |> ignore

    getReachableRules startModule (getAdditionRules startRule) startRule.body
    reachedRules

let reachableRulesInfo (def: Definition<_,_>) =
  reachableRulesInfo_of_grammar def.grammar

let IsUnusedRulesExists(def:Yard.Core.IL.Definition<_,_>) =
  let reachedRules = reachableRulesInfo def
  def.grammar |> List.exists (fun m ->
        m.rules |> List.exists (fun r -> let v = (getModuleName m, r.name.text) in not <| reachedRules.Contains v)
  )

/// Usage example: check after conversion, that we didn't lose any binding to source (e.g. position)
let sourcesWithoutFileNames (def:Yard.Core.IL.Definition<Source,Source>) =
    let inline check (src : Source) = src.file = ""
    let collectName (src : Source) = if check src then [src] else []
    let collectOpt (src : Source option) =
        match src with
        | Some src when check src -> [src]
        | _ -> []
    let rec processBody = function
        | PRef (name,args) -> collectName name @ collectOpt args
        | PMetaRef (name,args,metas) -> collectName name @ collectOpt args @ List.collect processBody metas
        | PSeq (s,ac,lab) -> collectOpt ac @ (s |> List.collect (fun e -> processBody e.rule))
        | PToken tok | PLiteral tok -> collectName tok
        | PAlt (l,r) -> processBody l @ processBody r
        | PConj (l,r) -> processBody l @ processBody r
        | PMany e | PSome e | POpt e -> processBody e
        | PPerm p -> List.collect processBody p
        | PRepet (p,_,_) -> processBody p
        

    def.grammar |> List.collect (fun m ->
        m.rules |> List.collect (fun r ->
            List.filter check r.args
            @ collectName r.name
            @ List.filter check r.metaArgs
            @ processBody r.body
        )
    )