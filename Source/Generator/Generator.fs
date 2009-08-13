// Generator.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

module Generator

open IL.Production
open IL.Rule
open IL
open Grammar.Item
open Grammar.Symbol

let start_time = ref System.DateTime.Now
let end_time   = ref System.DateTime.Now      

let lex_list = Test.test_lexem

let rules = Test.test_grammar

//let start_nterm = GrammarPreparer.get_start_nterm grammar

let items =
    let rules_map  = List.zip ([0..(List.length rules)-1])rules
    List.map (fun (i,rl) -> 
                let (itm,s,f) = FinitAutomata.FA_rules(rl.body) 
                let get_symb =  function 
                                Some(PLiteral(s)|PToken(s)|PRef(s,_)) -> Some(Terminal(s))                                                                                  
                                | _ -> failwith "error!!!"                                          
#if DEBUG
                Log.print_item itm s f;
#endif
                Set.fold_left (fun buf (a,b,c) ->                                                    
                                   let new_item  item_num next_num =
                                      {prod_num = i;
                                       prod_name = rl.name;
                                       item_num = item_num;
                                       symb = get_symb b;                                                                           
                                       next_num = next_num;
                                       s=s;
                                       f=f                                                                                          
                                      }
                                   buf + Set.singleton(new_item a (Some(c)))+
                                    if Set.exists ((=)c) f
                                    then Set.singleton(new_item c None)
                                    else Set.empty 
                                   )Set.empty itm)rules_map
    |> Set.union_all

let closure q = 
    let rec inner_closure i q = 
      if i = Set.count q 
      then q
      else
        let next_cl f = inner_closure (i+1) (q + Set.filter f items)
        let closure_one elt = 
            let el_for_cl = List.nth (Set.to_list q) i 
            elt.prod_name = Utils.getText el_for_cl.symb && elt.item_num = elt.s 
        next_cl closure_one                                                                                 
    in
    inner_closure 0 q
        
let closure_set = 
#if DEBUG
  Log.print_items items
#endif
  dict <| Set.map (fun x -> x, closure (Set.singleton x)) items                

let goto_set =     
    let eql = function 
        | (PToken x |PLiteral x), Some(Terminal y | Nonterminal y ) -> x=y
        | _ -> false
    in 
    let make_goto q x =  
        let closure = Set.fold_left (fun y x -> y + closure_set.[x]) Set.empty q
        Set.union_all [for item in closure do if eql(x, item.symb) then yield Utils.nextItem item items]
    let toString = function | PToken y |PLiteral y | PRef (y,_) -> Source.toString y 
                            | _ -> ""
    let goto_data symbol item = 
        let gt = make_goto (Set.singleton item) symbol
        hash(item, toString symbol),gt
    dict <| List.fold_left (fun buf symbol -> buf@[for item in items -> goto_data symbol item]) [] lex_list
                       
let generate = 
    IO.writeValue "goto.dta" goto_set;
    IO.writeValue "items.dta" items;
    PrettyPrinter.out := IO.text_writer "test1.fs";
    PrettyPrinter.print_header "test" ["IL"];
    (!PrettyPrinter.out).Close();
    printfn "End working time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time));