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

let prepare (definition:IL.Definition.t<'a,'b>) = definition.head,definition.grammar,definition.foot

let get_start_nterm (grammar:IL.Grammar.t<'a,'b>) = 
    List.filter (fun (x:IL.Rule.t<'a,'b>)-> x._public) grammar

//let get_rules (grammar:IL.Grammar.t<'a,'b>) =  
 