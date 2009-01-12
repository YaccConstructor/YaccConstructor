#light "off"

module FinitAutomata

open IL.Production
open IL.Source
open Set
  
let rec create_NFA regexpr =
    let state = 
        let i =ref 0 in
        let next () = i:=!i+1;!i in
        next
    in   
    match regexpr
    with
    | PSeq (seq,attr) -> ( let new_autom = List.map (fun t -> create_NFA t.rule) seq
                           in 
                           let f (lrules,ls,lf) (rrules,rs,rf) = (List.concat [[lf,None,rs];lrules;rrules],ls,rf)                              
                           in
                           List.fold_left f new_autom.Head new_autom.Tail
                          ) 
                              
    | PAlt (l,r)      -> (match (create_NFA l,create_NFA r)
                          with ((lrules,ls,lf),(rrules,rs,rf))-> (let s,f = state(),state() in                                                                
                                                                  List.concat [[(s,None,ls);
                                                                              (s,None,rs);
                                                                              (lf,None,f);
                                                                              (rf,None,f)];lrules;rrules],s,f)
                            ) 
    | PMany (expr)    ->  (match (create_NFA expr)
                           with (rules,s,f) ->(List.concat [[f,None,s];[s,None,f];rules],s,f)
                          )
    | PToken(ch)
    | PLiteral(ch) as t -> (let s,f = state(),state() in ([s,Some(t),f],s,f))
    
let states rules = List.fold_left (fun set (a,b,c) -> Set.union set (of_list[a;c])) empty rules 
     
let exists_e_elt:(int list ref) = ref []

let e_closure (rules,s,f) =    
    let rec closure stt = 
        if List.exists ((=)stt) (!exists_e_elt)
        then (!exists_e_elt)
        else (exists_e_elt:=stt::!exists_e_elt;
              let lst = (List.filter (fun(a,b,c)-> a = stt && b = None) rules)
              in
              if lst = [] then !exists_e_elt else
              List.concat (List.map (fun (a,b,c)-> closure c)
              lst))
     in   
     let get_rpart stt = of_list(List.map (fun (a,b,c)-> (b,c))(List.filter(fun (a,b,c)-> a=stt && b<>None)rules))
     in 
     let unfiltr_rules = union_all(Set.map (fun x -> Set.map (fun (b,c)-> (x,b,c))(union_all(Set.map get_rpart (of_list(closure x))))) (states rules))
     in
     let rsymbols = Set.map (fun (a,b,c)->c) unfiltr_rules
     in
     (Set.filter (fun (a,b,c)->a=s||Set.exists ((=)c) rsymbols)unfiltr_rules,s,f)
     
let FA_rules rule = (e_closure(create_NFA rule))
    
    