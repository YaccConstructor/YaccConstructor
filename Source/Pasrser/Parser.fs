// Parser.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light 
namespace Yard.Core
open IL
open Production
open Grammar.Item
open Tree
open Utils
open Lexeme.Lexeme
open Lexeme

type Parser(tables: Tables) = class
  let m_end = {name = "$";value = 1}
  let start_time = ref System.DateTime.Now
  let end_time   = ref System.DateTime.Now               
  let (get_next_ch:int->t<int>),input_length =       
      let lex_list = //[{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
                     //[{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
                     [{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "LEFT";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "RIGHT";value = 1};{name = "$";value = 1}]
      let l = List.length lex_list 
      let get i =  List.nth (lex_list) (l-i)        
      let input_length () = l 
      get,input_length           



  let is_start symbol_name = List.exists ((=) symbol_name) tables.StartNterms

  let memoize f =
     let t = new System.Collections.Generic.Dictionary<Set<'x>*'c,'b>()   
     fun (x,y) ->        
         let id = hash(x)
         let key = x,y
         if t.ContainsKey(key)       
         then t.[key] 
         else 
            let res = f(x,y) 
            t.Add(key,res)
            res
                        
  do start_time := System.DateTime.Now;
     printfn "Parsing.\nStart time: %A" System.DateTime.Now    

  let goto (states,symbol) = 
      Set.unionMany 
        <| seq { for y,tree in states 
                 -> set <| seq {for z in (tables.GotoSet.[hash (y,symbol)]) 
                                -> z,tree}}
     
  let rec climb =
      memoize (fun (states,(symbol,i)) -> 
      if Set.isEmpty states
      then Set.empty
      else         
      let new_states = parse (goto (states,symbol),i)
  #if DEBUG
      let gt = goto (states,symbol)
      Log.print_climb_info i symbol states gt new_states;        
  #endif             
      if Set.exists (fun ((item,tree),i) ->is_start item.prod_name && item.next_num=None && i=1) new_states     
      then set [for state in states do if (fst state).next_num = None then yield state,1] 
      else
        [for (item,tree),i in new_states do
           let prev_itm = prevItem item tables.Items                   
           if Set.exists (fun itm -> getText itm.symb = symbol && itm.item_num=item.s) prev_itm 
              && not(is_start item.prod_name)
           then 
              let create_new_tree (state,_tree) = state, [Node(_tree@tree,item.prod_name,[],1)]
              yield climb(Set.map create_new_tree states,(item.prod_name,i))
           else
              if Set.exists (fun (itm,_) -> Set.exists ((=)item) (nextItem itm tables.Items) && itm.item_num <> itm.s)
                            states
              then yield Set.map (fun itm -> (itm, snd (states.MinimumElement)@tree), i) prev_itm ] |> Set.unionMany)                

  and parse =
      memoize (fun (states,i) -> 
  #if DEBUG 
      Log.print_parse states i;
  #endif
      let text = (get_next_ch i).name
      let leaf_tree = [Leaf(text,[],1)]
      let new_states = Set.filter (fun (item,tree) -> item.next_num=None)states
      let result_states states create_tree = set[for (item,tree) in states -> item,create_tree]    
      Set.map (fun x -> x,i)(result_states new_states [])
      + if (get_next_ch i = m_end) then Set.Empty else climb(result_states states leaf_tree,(text,i-1))
      )
        
  let res () =
      let parse_res =parse (Set.of_list (List.map (fun x -> x,[])
                                                  (List.filter (fun x ->is_start x.prod_name)
                                                               (Set.to_list tables.Items)))
                           ,input_length())
      end_time := System.DateTime.Now;    
      let trees = Set.of_list(List.concat(Set.map(fun ((a,b),i) -> b) parse_res));
      Seq.iter(fun b -> print_tree b) trees;
      printfn "Parser get %A dirivation tree" trees.Count;
      not(parse_res.IsEmpty)
  member self.Run() = Log.print_result !start_time !end_time (res())
end
    