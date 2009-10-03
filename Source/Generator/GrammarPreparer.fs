// GrammarPreparer.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

module GrammarPreparer 

open IL
open IL.Production

let prepare (definition:IL.Definition.t<'a,'b>) = definition.head,definition.grammar,definition.foot

let get_start_nterm (grammar:IL.Grammar.t<'a,'b>) = 
    List.filter (fun (x:IL.Rule.t<'a,'b>)-> x._public) grammar

let rec get_all_t (grammar:IL.Grammar.t<'a,'b>) =     
    let rec get_tok production = 
        match production with
            | PSeq (seq,attr) -> List.concat (List.map (fun (elem:(IL.Production.elem<_,_>)) -> get_tok elem.rule)seq)
                                                                
            | PAlt (l,r)      -> get_tok l @ get_tok r
                          
            | PMany (expr)                     
            | PSome (expr)    ->  get_tok expr
            | PToken(ch)
            | PRef(ch,_)
            | PLiteral(ch) as t -> [t]
            
    List.fold (fun lst (production:IL.Rule.t<_,_>) -> (get_tok production.body)@lst) [] grammar