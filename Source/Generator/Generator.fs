#light

module Generator

open IL.Production
open IL.Rule
open IL
open Grammar.Item
open Grammar.Symbol

let lex_list = Test.test_lexem

let rules =Test.test_grammar

let items =
    let rules_map  = List.zip ([0..(List.length rules)-1])rules
    List.map (fun (i,rl) -> let (itm,s,f) = (FinitAutomata.FA_rules(rl.body)) in 
                                         
#if DEBUG
                                      Log.print_item itm s f;
#endif
                                      Set.of_list(List.concat(Set.map (fun (a,b,c) ->
                                                             ( {prod_num = i;
                                                               prod_name = rl.name;
                                                               item_num = a;
                                                               symb = (match b with 
                                                                        Some(PLiteral(s)|PToken(s)|PRef(s,_)) -> Some(Terminal(s))                                                                                  
                                                                       | _ -> failwith "error!!!");                                                                           
                                                               next_num = Some c;
                                                               s =s;
                                                               f=f                                                                                          
                                                              }::
                                                              (if (Set.exists ((=)c) f)
                                                               then [{prod_num = i;
                                                               prod_name = rl.name;
                                                               item_num = c;
                                                               symb = (match b with 
                                                                        Some(PLiteral(s)|PToken(s)| PRef(s,_)) -> Some(Terminal(s))                                                                      
                                                                       | _ -> failwith "error!!!");                                                                    
                                                               next_num = None;
                                                               s =s;
                                                               f=f}]  
                                                               else [] )))itm)))rules_map
    |> Set.union_all

let getText = function
    |Some( Terminal x )     ->  Source.toString x  
    | _                     -> "" 

let closure q = 
    let rec cl i q = 
      if i = Set.count q 
      then q
      else
        let next_cl f = cl (i+1) (Set.union_all [q; Set.filter f items])
        let closure_one elt = 
            let el_for_cl = List.nth (Set.to_list q) i 
            elt.prod_name = getText el_for_cl.symb && elt.item_num = elt.s 
        next_cl closure_one                                                                                 
    in
    cl 0 q

let nextItem item = 
    let isNext x = item.next_num = Some x.item_num && item.prod_num=x.prod_num
    Set.filter isNext items    
        
let closure_set = 
#if DEBUG
  Log.print_items items
#endif
  dict <| Set.map (fun x -> x, closure (Set.singleton x) ) items
  
let goto_set =     
    let eql = function 
        | (PToken x |PLiteral x), Some(Terminal y | Nonterminal y ) -> x=y
        | _ -> false
    in 
    let make_goto q x =  
        let cl = Set.union_all (Set.map (fun x -> closure_set.[x]) q)         
        Set.union_all(Set.map (nextItem) (Set.filter (fun item -> (eql (x ,item.symb))) cl))    
    let t = new System.Collections.Generic.Dictionary<(Grammar.Item.t<Source.t>*string),Set<Grammar.Item.t<Source.t>>>()    
    let toString = function | PToken y |PLiteral y | PRef (y,_) -> Source.toString y 
                            | _ -> ""   
    List.iter (fun x -> (Set.iter (fun y -> let gt = make_goto (Set.singleton y) x in
#if DEBUG
                                                Log.print_goto_c gt y x;
#endif
                                                t.Add((y, toString x),gt)))items) lex_list;
                       
    t

let generate = 
    IO.writeValue "goto.dta" goto_set;
    IO.writeValue "items.dta" items;