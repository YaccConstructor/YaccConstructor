// FinitAutomata.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light 

module Yard.Core.FinitAutomata

open IL.Production
open IL.Source
open Utils
  
let rec create_NFA = function 
    | PSeq (seq,attr) -> let new_autom = List.map (fun t -> create_NFA t.rule) seq                            
                         let aut_concat (lrules,ls,lf) (rrules,rs,rf) = ([lf,None,rs]@lrules@rrules,ls,rf)                                                        
                         List.fold aut_concat new_autom.Head new_autom.Tail
                                                        
    | PAlt (l,r)      -> match (create_NFA l,create_NFA r)with
                         (lrules,ls,lf),(rrules,rs,rf) -> (let s,f = next(),next()                                                                 
                                                           [s,None,ls]@[s,None,rs]@
                                                           [lf,None,f]@[rf,None,f]@
                                                           lrules@rrules,s,f)
                  
    (*it is dirty hack. IT MUST BE FIXED*)              
    | PMany (expr)    (*->  (let (rules1,s1,f1) = (create_NFA expr)
                           let (rules2,s2,f2) = (create_NFA (PSome expr))
                           //let ns,nf = next(),next()
                           ([f1,None,s2]@rules1@rules2,s1,f2)) *)
    | PSome (expr)    ->  (function (rules,s,f) ->([f,None,s]@[s,None,f]@rules,s,f)) (create_NFA expr)                          
    | PToken(ch)
    | PRef(ch,_)
    | PLiteral(ch) as t -> (let s,f = next(),next() in ([s,Some(t),f],s,f))
    
    
let states rules = List.fold (fun buf (a,b,c) -> buf+(Set.of_list[a;c])) Set.Empty rules      
     
let e_closure (rules,s,f) =    
    let exists_e_elt = ref Set.Empty   
    (*let rec closure stt =
        if Set.exists ((=)stt) (!exists_e_elt)
        then (!exists_e_elt)
        else (exists_e_elt:=Set.add stt !exists_e_elt;
              let lst = List.filter (fun(state,symbol,next) -> state = stt && symbol = None) rules
              if lst = [] 
              then !exists_e_elt 
              else Set.fold (fun buf (state,symbol,next) -> buf + closure next) Set.Empty (Set.of_list lst))
      *)        
     in         
     let closure q = 
         let q' = ref (Set.singleton q)
         let l = ref 0
         while (!l < Set.count !q') do
            l:= Set.count !q';
            for s1 in !q' 
                do for (s2,ch2,f2)as state' in rules 
                       do if s2=s1 && ch2=None
                          then q':= (Set.add f2) !q'
            
         !q'              
             
     in
     let get_rpart stt = set [for state,symbol,next in  rules do if state=stt && symbol<>None then yield symbol,next]

     let closure_set = Set.map (fun x -> exists_e_elt:=Set.Empty;(x,closure x)) (states rules)
     let is_subset sttset (_,elt:Set<'a>) = 
         if Set.exists (fun x -> (elt.IsSubsetOf x)&&(not(elt.Equals x))) sttset 
         then Set.remove elt sttset 
         else sttset    
     let new_states = Set.fold is_subset (Set.of_list(snd(List.unzip (Set.to_list closure_set)))) closure_set
     let new_automata = 
         List.concat [for stt in new_states ->
                        List.concat[for (x,y,z) in rules do
                                      if (Set.exists ((=)x) stt)&&(Option.isSome y)
                                      then yield [for q in new_states do
                                                    if Set.exists ((=)z) q then yield (stt,y,q)]]]     
     let alter_name = dict (List.zip (Set.to_list new_states) [0..new_states.Count-1])      
     let new_rule (state,symbol,next) = alter_name.[state],symbol,alter_name.[next]
     let clean_new_automata = Set.map new_rule (Set.of_list new_automata)
     let set_alter_name = Set.map (fun stt -> alter_name.[stt])     
     let find_state stt = set_alter_name (Set.filter (fun x -> Set.exists ((=)stt) x) new_states)
     let new_finale_state = find_state f
     //it is really only one start state
     let new_start_state =(find_state s).MinimumElement
     in
#if DEBUG          
     Log.print_autonaton new_states clean_new_automata new_start_state new_finale_state closure_set (states rules);
#endif
     (clean_new_automata,new_start_state,new_finale_state)
     
let FA_rules rule =
    let fa_rule = create_NFA rule in 
#if DEBUG 
    (printf "\n Fa_rule : \n %A " (fa_rule));
#endif
    e_closure(fa_rule)