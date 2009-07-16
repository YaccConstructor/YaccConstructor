// FinitAutomata.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light 

module FinitAutomata

open IL.Production
open IL.Source
open Set
  
let state = 
    let i = ref 0
    let next () = incr i;!i
    next  
  
let rec create_NFA = function 
    | PSeq (seq,attr) -> let new_autom = List.map (fun t -> create_NFA t.rule) seq                            
                         let aut_concat (lrules,ls,lf) (rrules,rs,rf) = ([lf,None,rs]@lrules@rrules,ls,rf)                                                        
                         List.fold_left aut_concat new_autom.Head new_autom.Tail
                                                        
    | PAlt (l,r)      -> match (create_NFA l,create_NFA r)with
                         (lrules,ls,lf),(rrules,rs,rf) -> (let s,f = state(),state()                                                                 
                                                           [s,None,ls]@[s,None,rs]@
                                                           [lf,None,f]@[rf,None,f]@
                                                           lrules@rrules,s,f)
                          
    | PSome (expr)    ->  (function (rules,s,f) ->([f,None,s]@[s,None,f]@rules,s,f)) (create_NFA expr)                          
    | PToken(ch)
    | PLiteral(ch) as t -> (let s,f = state(),state() in ([s,Some(t),f],s,f))
    
let states rules = List.fold_left (fun buf (a,b,c) -> buf+(of_list[a;c])) empty rules      
     
let e_closure (rules,s,f) =    
    let exists_e_elt:(int Set ref) = ref empty   
    let rec closure stt =
        if exists ((=)stt) (!exists_e_elt)
        then (!exists_e_elt)
        else (exists_e_elt:=add stt !exists_e_elt;
              let lst = List.filter (fun(state,symbol,next) -> state = stt && symbol = None) rules
              if lst = [] 
              then !exists_e_elt 
              else fold_left (fun buf (state,symbol,next) -> buf + closure next) empty (of_list lst))
     in       
     let get_rpart stt = set [for state,symbol,next in  rules do if state=stt && symbol<>None then yield symbol,next]
                                          
     let closure_set = map (fun x -> exists_e_elt:=empty;(x,closure x)) (states rules)
     let is_subset sttset (_,elt) = 
         if exists (fun x -> (subset elt x)&&(not(equal elt x))) sttset 
         then remove elt sttset 
         else sttset
     let new_states = fold_left is_subset (of_list(snd(List.unzip (to_list closure_set)))) closure_set
     let new_automata = 
         List.concat [for stt in new_states ->
                        List.concat[for (x,y,z) in rules do
                                      if (exists ((=)x) stt)&&(Option.is_some y)
                                      then yield [for q in new_states do 
                                                    if exists ((=)z) q then yield (stt,y,q)]]]                                                   
     //generte dictionary for numerating statest of new automaton
     let alter_name = dict (List.zip (to_list new_states) [0..new_states.Count-1]) 
     //replaceing all old states with new
     let new_rule (state,symbol,next) = alter_name.[state],symbol,alter_name.[next]
     let clean_new_automata = map new_rule (of_list new_automata)
     let set_alter_name = map (fun stt -> alter_name.[stt])
     //getting new start and finale states
     let find_state stt = set_alter_name(filter (fun x -> exists ((=)stt) x) new_states)
     let new_finale_state =  find_state f
     //it is really only one start state
     let new_start_state = choose (find_state s)
     in
#if DEBUG          
     Log.print_autonaton new_states clean_new_automata new_start_state new_finale_state closure_set (states rules);
#endif
     (clean_new_automata,new_start_state,new_finale_state)
     
let FA_rules rule =
    let fa_rule = create_NFA rule in 
#if DEBUG 
    (print_any "Fa_rule!!!!:"; print_any (fa_rule));
#endif
    e_closure(fa_rule)
