#light 

module FinitAutomata

open IL.Production
open IL.Source
open Set

open System
  
let state = 
    let i = ref 0
    let next () = incr i;!i
    next  
  
let rec create_NFA = function 
    | PSeq (seq,attr) -> ( let new_autom = List.map (fun t -> create_NFA t.rule) seq                            
                           let f (lrules,ls,lf) (rrules,rs,rf) = (List.concat [[lf,None,rs];lrules;rrules],ls,rf)                                                        
                           List.fold_left f new_autom.Head new_autom.Tail
                          )                               
    | PAlt (l,r)      -> (match (create_NFA l,create_NFA r)with
                          ((lrules,ls,lf),(rrules,rs,rf)) -> (let s,f = state(),state()                                                                 
                                                              List.concat [[(s,None,ls);
                                                                            (s,None,rs);
                                                                            (lf,None,f);
                                                                            (rf,None,f)];lrules;rrules],s,f)
                            ) 
    | PMany (expr)    ->  (function (rules,s,f) ->(List.concat [[f,None,s];[s,None,f];rules],s,f)) (create_NFA expr)                          
    | PToken(ch)
    | PLiteral(ch) as t -> (let s,f = state(),state() in ([s,Some(t),f],s,f))
    
let states rules = List.fold_left (fun set (a,b,c) -> Set.union set (of_list[a;c])) empty rules      
     
let e_closure (rules,s,f) =    
    let exists_e_elt:(int Set ref) = ref empty   
    let rec closure stt =
        if exists ((=)stt) (!exists_e_elt)
        then (!exists_e_elt)
        else (exists_e_elt:=add stt !exists_e_elt;
              let lst = (List.filter (fun(a,b,c)-> a = stt && b = None) rules)
              if lst = [] 
              then !exists_e_elt 
              else union_all (map (fun (a,b,c)-> closure c)(of_list lst)))
     in        
     let get_rpart stt = of_list(List.map (fun (a,b,c)-> (b,c))(List.filter(fun (a,b,c)-> a=stt && b<>None)rules))
     let closure_set = map (fun x -> exists_e_elt:=empty;(x,closure x)) (states rules)
     let is_subset sttset (_,elt) = 
         if exists (fun x -> (subset elt x)&&(not(equal elt x))) sttset 
         then remove elt sttset 
         else sttset
     let new_states = fold_left is_subset (of_list(snd(List.unzip ( to_list closure_set)))) closure_set
     let new_automata = List.concat(List.concat(
                        List.map (fun stt -> 
                                      List.map (fun (x,y,z) -> 
                                                     List.map (fun lstt-> (stt,y,lstt))
                                                              (to_list(filter (fun q -> exists ((=)z) q ) new_states)))
                                               (List.filter (fun (x,y,z) -> (exists ((=)x) stt)&&(Option.is_some y)) rules))
                                      (to_list new_states)))
     in
     let alter_name = List.zip [0..new_states.Count-1] (to_list new_states) 
     let get_alter_name x = fst (List.find (fun (a,b) -> equal x b)alter_name)
     let new_rule (x,y,z) = get_alter_name x,y,get_alter_name z
     let clean_new_automata = map new_rule (of_list new_automata)
     let new_finale_state = map (fun x -> fst (List.find (fun (a,b) -> equal x b) alter_name ))(filter (fun x -> exists ((=)f) x) new_states)
     let new_start_state = (to_list (map (fun x -> fst (List.find (fun (a,b) -> equal x b) alter_name ))
                                         (filter (fun x -> exists ((=)s) x) new_states))).Head
     in
#if DEBUG     
     (
     Console.WriteLine("new_states:");
     iter print_any new_states;
     Console.WriteLine("new_automata:");
     iter print_any clean_new_automata;
     Console.WriteLine("new_start_state:");
     print_any new_start_state;
     Console.WriteLine("new_finale_state:");
     iter print_any new_finale_state;
     Console.WriteLine("Closure_set:");
     print_any (closure_set);
     Console.WriteLine("States:");
     print_any (states rules);
     Console.WriteLine()
     );
#endif
     (clean_new_automata,new_start_state,new_finale_state)
     
let FA_rules rule =
    let fa_rule = create_NFA rule in 
#if DEBUG 
    (print_any "Fa_rule!!!!:"; print_any (fa_rule));
#endif
    e_closure(fa_rule)
