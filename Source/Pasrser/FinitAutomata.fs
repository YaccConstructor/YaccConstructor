#light "off"

module FinitAutomaton

open IL.Production
open IL.Source

let state = 
    let i =ref 0 in
    let next () = i:=!i+1;!i in
    next
  
let rec create_NFA regexpr =       
    match regexpr
    with
    | PSeq (seq,attr) -> ( let new_autom = List.map (fun t -> create_NFA t.rule) seq
                           in 
                           let f (lrules,ls,lf) (rrules,rs,rf) = (List.concat [[lf,None,rs];lrules;rrules],ls,rf)                              
                           in
                           List.fold_left f new_autom.Head new_autom.Tail
                          ) 
                              
    | PAlt (l,r)      -> (match (create_NFA l,create_NFA r)
                          with
                              |((lrules,ls,lf),(rrules,rs,rf))-> (let s,f = state(),state() in                                                                
                                                                List.concat [[(s,None,ls);
                                                                              (s,None,rs);
                                                                              (lf,None,f);
                                                                              (rf,None,f)];lrules;rrules],s,f)
                            ) 
    | PMany (expr)    ->  (match (create_NFA expr)
                             with
                             |(rules,s,f) ->(let ns,nf = state(),state() in                                             
                                             (List.concat [[ns,None,nf];[nf,None,ns];[ns,None,s];[f,None,nf];rules],ns,nf)
                                             )
                                             //(List.concat [[s,'`',f];[f,'`',s];rules],s,f)
                             )
    | PToken(ch)
    | PLiteral(ch) as t -> (let s,f = state(),state() in ([s,Some(toString ch),f],s,f))