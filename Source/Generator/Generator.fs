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

let rules = Test.test_grammar_2

let items =
    let rules_map  = List.zip ([0..(List.length rules)-1])rules
    List.map (fun (i,rl) -> let (itm,s,f) = (FinitAutomata.FA_rules(rl.body)) 
                            let get_symb =  function 
                                            Some(PLiteral(s)|PToken(s)|PRef(s,_)) -> Some(Terminal(s))                                                                                  
                                            | _ -> failwith "error!!!" 
                                         
#if DEBUG
                            Log.print_item itm s f;
#endif
                            Set.of_list(List.concat(Set.map (fun (a,b,c) ->                                                    
                                                  (let new_item  item_num next_num =
                                                       {prod_num = i;
                                                        prod_name = rl.name;
                                                        item_num = item_num;
                                                        symb = get_symb b;                                                                           
                                                        next_num = next_num;
                                                        s=s;
                                                        f=f                                                                                          
                                                       }
                                                   (new_item a (Some(c)))
                                                    ::
                                                    (if (Set.exists ((=)c) f)
                                                     then [new_item c None]  
                                                     else [] 
                                                     )))itm)))rules_map
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
        let closure = Set.union_all (Set.map (fun x -> closure_set.[x]) q)         
        Set.union_all(Set.map nextItem (Set.filter (fun item -> eql(x, item.symb)) closure))
    let toString = function | PToken y |PLiteral y | PRef (y,_) -> Source.toString y 
                            | _ -> ""
    let goto_data symbol item = 
        let gt = make_goto (Set.singleton item) symbol
        hash(item, toString symbol),gt
    dict <| List.concat(List.map (fun symbol -> (List.map (goto_data symbol) (Set.to_list items))) lex_list)
                       
let generate = 
    IO.writeValue "goto.dta" goto_set;
    IO.writeValue "items.dta" items;
    printfn "End working time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time));