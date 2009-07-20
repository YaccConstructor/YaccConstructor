// Parser.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


#light 
#nowarn "40"
open IL
open Production
open Grammar.Item
open Tree
open Set
open Data
open Utils

open System.Threading

let m_end,m_start = (PLiteral("$",(1,1)),PToken("S",(1,1)))

let start_time = ref System.DateTime.Now
let end_time   = ref System.DateTime.Now                     

let memoize (f: ('a*'c) ->'b) =
   let t = new System.Collections.Generic.Dictionary<Set<'x>*'c,'b>()   
   fun (x,y) ->        
       let id = hash(x);
       let key = x,y
       if t.ContainsKey(key)       
       then 
         (
          let res = t.[key]
          res)
       else 
         (
          let res = f (x,y) 
          t.Add(key,res);          
          res )   
                      
do start_time := System.DateTime.Now;
   printfn "Parsing.\nStart time: %A" System.DateTime.Now    

let goto (states,symbol) = union_all [for y,tree in states -> set[for z in (goto_set.[hash (y,symbol)]) -> z,tree]]   
   
let rec climb =
    memoize (fun (states,(symbol,i)) -> 
    if Set.is_empty states
    then Set.empty
    else         
    let new_states = parse (goto (states,symbol),i)
#if DEBUG
    let gt =  goto (states,symbol)
    Log.print_climb_info i symbol states gt new_states;        
#endif             
    if Set.exists (fun ((item,tree),i) -> item.prod_name="S" && item.next_num=None && i=1) new_states     
    then set [for state in states do if (fst state).next_num = None then yield state,1] 
    else     
      union_all         
        [for (item,tree),i in new_states do
             let prev_itm = prevItem item                    
             if exists (fun itm -> getText itm.symb = symbol && itm.item_num=item.s)prev_itm
                && item.prod_name <> "S"
             then 
                let create_new_tree (state,_tree) = state, [Node(_tree@tree,item.prod_name,[],1)]
                yield climb(map create_new_tree states,(item.prod_name,i))
             else
                if exists (fun (itm,_) -> exists ((=)item) (nextItem itm) && itm.item_num <> itm.s) states
                then yield map (fun itm -> (itm, snd (choose states)@tree), i) prev_itm ])                

and parse =           
    memoize (fun (states,i) -> 
#if DEBUG 
    Log.print_parse states i;
#endif
    let text = mgetText(get_next_ch i)        
    let leaf_tree = [Leaf(text,[],1)]
    let new_states = Set.filter (fun (item,tree) -> item.next_num=None)states
    let result_states states create_tree = set[for (item,tree) in states -> item,create_tree]    
    map (fun x -> x,i)(result_states new_states [])
    + if (get_next_ch i = m_end) then empty else climb(result_states states leaf_tree,(text,i-1))
    )
                 
let res _ = 
    let parse_res =parse (of_list (List.map (fun x -> x,[])
                                            (List.filter (fun x -> x.prod_name ="S")
                                                         (Set.to_list items))),
                                   input_length())
    end_time := System.DateTime.Now;    
    let trees = of_list(List.concat(map(fun ((a,b),i)-> b) parse_res));
    iter(fun b -> print_tree b) trees;
    printfn "Parser get %A dirivation tree" trees.Count;
    not(parse_res=empty)
do                    
   let r = res ()
   printfn "Result : %A" r;
   printfn "End parsing time: %A Total: %A" !end_time (!end_time - (!start_time));
   printfn "End working time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time))   