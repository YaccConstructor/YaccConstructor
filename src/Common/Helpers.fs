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

[<AutoOpen>]
module Yard.Core.Helpers

open System.Collections.Generic
open Yard.Core.IL
open Production

let errorToken = "error"

let getModuleName (m : Module<_,_>) = 
    match m.name with
    | Some n -> n.text
    | None -> ""

let defaultModules rules : Module<_,_> list =
    [{
        rules = rules
        openings = []
        name = None
        allPublic = false
    }]

let defaultDefinition rules = {emptyGrammarDefinition with grammar = defaultModules rules}

let simpleRules nonTerm body : Rule.t<_,_> list =
    [{
        name = Source.t nonTerm
        args = []
        body = body
        isStart = true
        isPublic = false
        isInline = false
        metaArgs = []
    }]

let metaRules nonTerm body args : Rule.t<_,_> list = 
    [{
        name = Source.t nonTerm
        args = []
        body = body
        isStart = false
        isPublic = false
        isInline = false
        metaArgs = args
    }]

let simpleNotStartRules nonTerm body : Rule.t<_,_> list =
    metaRules nonTerm body []

let verySimpleRules nonTerm seq : Rule.t<_,_> list =
    simpleRules nonTerm <| PSeq(seq, None, None)

let verySimpleNotStartRules nonTerm seq : Rule.t<_,_> list =
    simpleNotStartRules nonTerm <| PSeq(seq, None, None)

let inline mapModule mapF (m : Module<_,_>) =
    {m with rules = mapF m.rules}

let inline mapGrammar mapF grammar =
    grammar
    |> List.map (mapModule mapF)

/// Map: module -> (list of public rules, declared in it)
let getPublicRules (grammar : Grammar<_,_>) =
    grammar
    |> List.map (fun module' ->
            let publics = module'.rules |> List.filter (fun r -> r.isPublic)
            getModuleName module', publics
        )
    |> dict

/// For each module creates map: rule -> (module, in which the rule is declared)
let getRulesMap (grammar : Grammar<_,_>) =
    let publicRules = getPublicRules grammar
    grammar
    |> List.map (fun module' ->
        let rMap = new System.Collections.Generic.Dictionary<_,_>()
        module'.openings
        |> List.iter (fun op ->
            try 
                publicRules.[op.text] |> List.iter (fun r ->
                    try 
                        rMap.[r.name.text] <- op.text
                    with
                    | e -> printfn "Get rules error: rule name: %A;" r.name.text

                        )
            with
            | e -> printfn "Get rules error: open: %A;" op.text  
            
        )
        module'.rules |> List.iter (fun r -> rMap.[r.name.text] <- getModuleName module')
        getModuleName module', rMap
    )
    |> dict

/// if rule has metaArgs then it is a metarule
let isMetaRule (r:Rule.t<Source.t,Source.t>) = r.metaArgs <> []

/// hash table for metarules. 
/// Map: using_module -> (rule_name -> (decl_module, rule_decl));
let metaRulesTbl grammar =
    let rulesMap = getRulesMap grammar
    let publicRules = new Dictionary<_,_>(getPublicRules grammar)
    /// Only public meta-rules present here
    let publicMeta =
        let map = new Dictionary<string,Rule.t<Source.t, Source.t> list>()
        publicRules |> Seq.iter (fun item ->
            map.[item.Key] <- List.filter isMetaRule item.Value
        )
        map

    grammar
    |> List.map (fun m ->
        let res = new Dictionary<_,_>()
        m.openings
        |> List.iter (fun op ->
            publicMeta.[op.text]
            |> List.iter (fun rule ->
                res.[rule.name.text] <- (op.text, rule)
            )
        )
        m.rules
        |> List.filter isMetaRule
        |> List.iter (fun rule ->
            res.[rule.name.text] <- (getModuleName m, rule)
        )
        getModuleName m, res
    )
    |> dict