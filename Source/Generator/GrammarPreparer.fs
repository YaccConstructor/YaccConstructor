// GrammarPreparer.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

module Yard.Core.GrammarPreparer 

open IL
open IL.Production
open IL.Rule

let prepare (definition:IL.Definition.t<'a,'b>) = definition.head,definition.grammar,definition.foot

let get_start_nterms grammar = 
    List.fold (fun buf (x:IL.Rule.t<'a,'b>)-> if x._public then x.name::buf else buf) [] grammar

let rec get_all_t grammar =     
    let rec get_tok production = 
        match production with
            | PSeq (seq,attr)   -> Set.unionMany (List.map (fun elem -> get_tok elem.rule)seq)                                                                
            | PAlt (l,r)        -> get_tok l + get_tok r                          
            | PMany (expr)                     
            | PSome (expr)      ->  get_tok expr
            | PToken(ch)
            | PRef(ch,_)
            | PLiteral(ch) as t -> Set.singleton (Source.toString ch)
            
    List.fold (fun lst (production:IL.Rule.t<_,_>) -> (get_tok production.body)+lst) Set.empty grammar
    
let createStartRule ruleName productionName = 
    {name=ruleName;
     args = [];
     body =  PSeq([{omit=false;
                   rule= PRef((productionName,(0,0)),None);
                   binding = None;
                   checker = None};
                  ],None); _public=true; metaArgs =[]}
                  
let replace_Public rules = 
    List.map (fun rule -> {name=rule.name;
                           args=rule.args;
                           body=rule.body;
                           _public=false;
                           metaArgs=rule.metaArgs}) 
              rules