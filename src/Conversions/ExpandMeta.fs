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

module Yard.Core.Conversions.ExpandMeta 

open Yard.Core
open Yard.Core.IL
open Production
open Yard.Core.Namer
open TransformAux
open Mono.Addins

open System.Collections.Generic

/// find metarule with given name in hash map of collected metarules
let findMetaRule (tbl : IDictionary<string,Dictionary<string,string * Rule.t<Source.t,Source.t>>>) module' metaName =
    try tbl.[module'].[metaName] with
    | :?System.Collections.Generic.KeyNotFoundException ->
        failwith <| sprintf "unable to find metarule %s in module %s" metaName module'

let getKey module' key = 
    let argsToString = function
        | None -> ""
        | Some x -> "[" + x.ToString() + "]"

                    
    let rec metaArgsToString metaArgs =
        if ((metaArgs : list<_>).IsEmpty) then ""
        else
            metaArgs
            |> List.map getProdKey
            |> String.concat " "
            |> fun res -> "<" + res + ">"
                    
    and getProdKey this =
        match this with
        |PAlt (x, y) -> getProdKey x + "|" + getProdKey y
        |PConj (x, y) -> getProdKey x + "&" + getProdKey y
        |PSeq (ruleSeq, attrs, l) ->
            let strAttrs =
                match attrs with
                | None -> ""
                | Some x -> "{" + (x : Source.t).text + "}"

            let elemToString (x : elem<Source.t, Source.t>) =
                let check =
                    match x.checker with
                    | None -> ""
                    | Some c -> "=>{" + c.ToString() + "}=>"
                let omit = if x.omit then "-" else ""
                let bind =
                    match x.binding with
                    | None -> ""
                    | Some var -> var.text + "="
                check + omit + bind + getProdKey x.rule
            "<" + String.concat " " (List.map (fun x -> "(" + elemToString x + ")") ruleSeq) + ">" + strAttrs
        |PToken src -> src.text
        |PRef (name, args) ->
            name.text + argsToString args
        |PMany x -> "(" + getProdKey x + ")*"
        |PSome x -> "(" + getProdKey x + ")+"
        |POpt x ->  "(" + getProdKey x + ")?"
        |PMetaRef (name, args, metaArgs) ->
            name.text + metaArgsToString metaArgs + argsToString args
        |PLiteral src -> src.text
        |PRepet (body,x,y) -> //failwith "Repetition was not realized yet"
                              match x,y with
                              |Some lower, Some upper -> sprintf "(%s)*[%i..%i]" (getProdKey body) lower upper
                              |None, Some upper ->  sprintf "(%s)*[..%i]" (getProdKey body) upper
                              |Some lower, None ->  sprintf "(%s)*[%i..]" (getProdKey body) lower
                              |None, None ->  sprintf "(%s)*[..]" (getProdKey body)
        |PPerm src ->
            src
            |> List.map getProdKey
            |> String.concat ";"
            |> fun res -> "[|" + res + "|]"
    module' + ":" + getProdKey key

/// <summary>
/// <para> Replace rule with new one, replacing references to metarules, </para>
/// <para> and generate new rules for every such reference </para>
/// <para> body: t - production which can contain metareferences </para>
/// <para> metaRules: TO_DO Dictionary&lt;string,Rule.t&lt;Source.t,Source.t&gt;&gt; - table, which contains expanding metareferences rules </para>
/// <para> expanded: Dictionary&lt;string,Production.t&gt; - map of metareference with actual params to generated rule name </para>
/// <para> res: Rule.t list - currently generated rules </para>
/// <para> returns (new body, generated rules + old rules) </para>
/// </summary>
let expandRule =
    /// Replace formal parameter with its actual value or (if it is not to be replaced) 
    let tryReplaceActual (formalToAct : (string * Production.t<_,_>) list) formal prev = 
        match List.tryFind (fun x -> fst x = formal) formalToAct with
        | None -> prev
        | Some res -> snd res

    /// Applies function to the first element of result
    let applyToRes f (a,b) = (f a, b)

    let rec expandMetaRef (name : Source.t) attrs metaArgs key module' metaRules expanded resRuleList =
        let (newMetaArgs, newRes) =
            metaArgs
            |> List.fold (fun (accMeta, accRes) body ->
                //printfn "%A" body
                expandBody body module' metaRules expanded accRes
                |> fun (newBody, newAccRes) -> (newBody::accMeta, newAccRes)
            ) ([], resRuleList)
            |> applyToRes List.rev
        // TODO catch exception
        let declModule, metaRule = findMetaRule metaRules module' name.text
        let newRuleName = new Source.t(newName ("rule_" + name.text), metaRule.name)
        let formalArgs = metaRule.args
        let substitution = PRef(new Source.t(newRuleName.text, name), attrs)
        let newKey = getKey module' <| PMetaRef(name, attrs, newMetaArgs)
        if not <| expanded.ContainsKey key then
            expanded.Add(key, substitution)
        if not <| expanded.ContainsKey newKey then
            expanded.Add(newKey, substitution)
        let newFormalToAct =
            let expected = metaRule.metaArgs.Length
            let actual = newMetaArgs.Length
            if expected = actual then
                List.zip (metaRule.metaArgs |> List.map Source.toString) newMetaArgs
            else 
                let pos (p:Source.Position) = string p.line + ":"  + string p.column
                failwithf "%s - %s. Incorrect number of args. Metarule \"%s\" expected %i parameter(s) but get %i." 
                    (pos name.startPos) (pos name.endPos) metaRule.name.text expected actual
        // TODO There can be a bug
        //let newGlobalAttrs = getRuleBindings metaRule globalAttrs
                
        let metaExp = expandBody (replaceMetasInBody newFormalToAct metaRule.body) declModule metaRules expanded newRes
        let newRule = {Rule.defaultRule newRuleName (fst metaExp) with args = formalArgs}
        (substitution, newRule::snd metaExp)

    /// <summary>
    /// <para> Replace rule body, expanding references to metarules, </para>
    /// <para> and generate new rules for every such reference </para>
    /// <para> body: t - production which can contain metareferences </para>
    /// <para> metaRules: TO_DO Dictionary&lt;string,Rule.t&lt;Source.t,Source.t&gt;&gt; - table, which contains expanding metareferences rules </para>
    /// <para> expanded: Dictionary&lt;string,Production.t&gt; - map of metareference with actual params to generated rule name </para>
    /// <para> resRuleList: Rule.t list - currently generated rules </para>
    /// <para> returns (new body, generated rules + old rules) </para>
    /// </summary>
    and expandBody body (module' : string) metaRules (expanded : Dictionary<_, Production.t<_,_>>) resRuleList =
        //printfn "b: %A" body
        /// Returns key for table of expanded rules.
        /// It's better to use hash.
        let key = getKey module' body
        if expanded.ContainsKey key then 
            // printfn "ok %A\n: \t%A\n\n:\t%A\n=========================\n" body (expanded.Item key) resRuleList
            expanded.[key], resRuleList
        else
            let newRule, newResRuleList =
                /// Expand rule with the previous parameters
                let simpleExpand body = expandBody body module' metaRules expanded resRuleList

                match body with
                | PSome body -> applyToRes PSome <| simpleExpand body
                | POpt body  -> applyToRes POpt  <| simpleExpand body
                | PMany body -> applyToRes PMany <| simpleExpand body
                | PRef(name, attrs) as x -> (x, resRuleList)
                | PAlt (l, r) ->
                    let x,y = expandBody l module' metaRules expanded [], simpleExpand r
                    (PAlt (fst x, fst y), snd x @ snd y)
                | PConj (l, r) ->
                    let x,y = expandBody l module' metaRules expanded [], simpleExpand r
                    (PConj (fst x, fst y), snd x @ snd y)
                | PSeq (ruleList, actionCode, l) ->
                    ruleList
                    |> List.fold (fun (curSeq, accRes) elem' ->
                        let bodyExp = expandBody elem'.rule module' metaRules expanded accRes
                        bodyExp |> applyToRes (fun h -> {elem' with rule = h}::curSeq)
                    ) ([], resRuleList)
                    |> applyToRes (fun x -> PSeq (List.rev x, actionCode, l))
                | PLiteral _ as literal -> (literal, resRuleList)
                | PToken _ as token -> (token, resRuleList)
                | PMetaRef (name, attrs, metaArgs) as x -> 
                    if metaArgs.IsEmpty then
                        (PRef(name, attrs), resRuleList)
                    else expandMetaRef name attrs metaArgs key module' metaRules expanded resRuleList
                | PPerm _ -> failwith "Unrealised meta-expanding of permutation"
                | PRepet (body,x,y) -> //failwith "Unrealised meta-expanding of repetition"
                                       let a, b = simpleExpand body
                                       applyToRes PRepet <| ((a, x, y),b)
            if not <| expanded.ContainsKey key then
                expanded.Add(key, newRule)
//            printfn "%A\n: \t%A\n\n:\t%A\n=========================\n" body rule resRuleList
            (newRule, newResRuleList)

    /// <para> Replace all rules to be replaced (meta-arguments substitution) in production. </para>
    /// <para> formalToAct - list of (metaArg name, its actual value) </para>
    /// <para> body - production to replace.  </para>
    and replaceMetasInBody formalToAct body = 
        let replace = replaceMetasInBody formalToAct
        match body with
        | PSome body -> PSome <| replace body
        | POpt body  -> POpt  <| replace body
        | PMany body -> PMany <| replace body
        | PAlt (l, r) -> PAlt(replace l, replace r)
        | PConj (l, r) -> PConj(replace l, replace r)
        | PLiteral _ as literal -> literal
        | PToken _ as token -> token
        | PRef(name, attrs) as prev ->
            prev |> tryReplaceActual formalToAct (Source.toString name)
        | PSeq (ruleList, actionCode, l) ->
            PSeq (ruleList |> List.map (fun x ->  {x with rule = replace x.rule})
                    , actionCode, l)
        | PMetaRef (name, attrs, metaArgs) -> 
            if metaArgs.IsEmpty then
                PRef(name, attrs) |> tryReplaceActual formalToAct (Source.toString name)
            else PMetaRef(name, attrs, List.map replace metaArgs)
        | PPerm _ -> failwith "Unrealised meta-expanding of permutation"
        | PRepet (body,x,y) -> //failwith "Unrealised meta-expanding of repetition"
                               PRepet  <| (replace body, x, y)

    fun body (module' : string) metaRules expanded ->
        expandBody body module' metaRules expanded []


(** grammar processing:
 *  - collect metarules
 *  - expand metarules
 *  - call metarules expanding 
 *)

(** main function 
 *  - create hash tables
 *  - call process grammar to expand metarules
 *)
let private expandMetaRules grammar =
    /// hash table for references to expanded metarules
    let refsTbl = new Dictionary<_, Production.t<_,_> >(200)
    /// Replace existing meta-rules. Suppose that all high-level meta-rules are in metaRulesTbl
    let rec replaceMetasInGrammar (grammar : Grammar.t<_,_>) metaRulesTbl refsTbl = 
        grammar
        |> List.map (fun m ->
            m.rules
            |> List.collect (fun rule ->
                if isMetaRule rule then []
                else 
                    let b, rules = expandRule rule.body (getModuleName m) metaRulesTbl refsTbl
                    let r = { rule with Rule.body = b }
                    r::rules
            )
            |> (fun rules -> {m with rules = rules})
        )
    replaceMetasInGrammar grammar (metaRulesTbl grammar) refsTbl


[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type ExpandMeta() = 
    inherit Conversion()
        override this.Name = "ExpandMeta"
        override this.ConvertGrammar (grammar,_) = expandMetaRules grammar
