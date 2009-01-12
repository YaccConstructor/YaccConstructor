#light "off"

module PreCalculation

open FinitAutomata
open IL.Production
open IL.Rule
open IL.Source
open Set
open Log

type item<'a> = Item of int*string*(int*'a option*int)*int*int 

let lex_list = []//['E';'a';'+';'*';' ';'S';')';'(']
//(prod_num,(item_num,rec_symb,next_num),s,f)
let rules = []
let items =
    let rules_map  = List.zip ([0..(List.length rules)])rules
    in
    union_all(List.map (fun (i,rl) -> let (itm,s,f) = (FA_rules(rl.body)) in  Set.map (fun x -> Item(i,rl.name,x,s,f)) itm)rules_map)

let getText a = 
    match a 
    with 
    |PToken(x) -> toString x  
    | _       -> "" 

let closure q= 
    ( 
    let ex = new System.Collections.Generic.KeyNotFoundException()
    in
    let rec cl i q = 
        if i = Set.count q 
        then q
        else
         let next_cl f = cl (i+1) (union_all [q;filter f items])
         in 
         next_cl (fun x -> 
                      let el_for_cl = (List.nth (Set.to_list q) i)
                      in  
                      match x 
                      with 
                      | Item(prod_num,rl_name,(item_num,rec_symb,next_num),s,f) -> 
                                                       (fun (Item(prod_num2,rl_name2,(item_num2,Some(t),next_num2),s2,f2)) -> 
                                                        try(rl_name = (getText t) && 
                                                            item_num=s)
                                                        with ex -> false)                                                           
                                                        el_for_cl                                           
                  )
    in
    cl 0 q)

let nextItem (Item(prod_num,rl_name,(item_num,rec_symb,next_num),s,f)) = 
    try Set.filter (fun (Item(a,b,(c,d,e),s,f))-> next_num=c&&a=prod_num)items with _-> empty
let prevItem (Item(prod_num,rl_name,(item_num,rec_symb,next_num),s,f)) = 
    try Set.filter (fun (Item(a,b,(c,d,e),s,f))-> item_num=e&&a=prod_num)items with _-> empty
        
let closure_set = 
    let t = System.Collections.Generic.Dictionary<(item<'a>),Set<(item<'a>)>>()
    in
    Set.iter (fun x -> t.Add(x,closure (Set.add  x empty)))items;
    t

let goto_set = 
    let make_goto q x =  
        let cl = union_all (Set.map (fun x -> closure_set.[x]) q)
        in 
        union_all(Set.map (nextItem) (Set.filter (fun (Item(a,b,(c,d,e),s,f)) -> (x = d)) cl))
    in
    let t = System.Collections.Generic.Dictionary<(item<'a>*char),Set<item<'a>>>() 
    in
    List.iter (fun x-> (Set.iter (fun y-> t.Add((y,x),(make_goto (add y empty)) x)))items) lex_list;
    t  
     