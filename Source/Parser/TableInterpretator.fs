// TableInterpretator.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light 
namespace Yard.Core

open Grammar.Item
open AST
open Utils
open Lexeme.Lexeme


type TableInterpretator (tables: Tables) = class

  let m_end = {name = "$";value = "$"}
              
  let is_start symbol_name = List.exists ((=) symbol_name) tables.StartNterms

  let memoize f =
     let t = new System.Collections.Generic.Dictionary<_,_>()   
     fun (x,y,z) ->        
         let id = hash(x)
         let key = x,y
         if t.ContainsKey(key)       
         then t.[key] 
         else 
            let res = f(x,y,z) 
            t.Add(key,res)
            res                     

  let goto (states,symbol) = 
      Set.unionMany 
        <| seq { for y,tree in states 
                 -> set <| seq {for z in (tables.GotoSet.[hash (y,symbol)]) 
                                -> z,tree}}
                                     
  let rec climb =
      memoize (fun (states,(symbol,i),getLexeme) -> 
      if Set.isEmpty states
      then Set.empty
      else    
      let gt = goto (states,symbol)     
      let new_states = parse (gt,i,getLexeme)
  #if DEBUG      
      Log.print_climb_info i symbol states gt new_states;        
  #endif             
      if Set.exists (fun ((item,(tree,seqNum)),i) -> is_start item.prod_name && item.next_num=None && i=1) new_states     
      then set <|seq {for item,tree as state in states do if item.next_num = None then yield state,1}
      else
        seq {for (item,(tree,seqNum)),i in new_states do
             let prev_itms = prevItem item tables.Items                   
             if Set.exists (fun itm -> Option.get itm.symb = symbol && itm.item_num=item.s) prev_itms 
                && not(is_start item.prod_name)  
             then 
                let create_new_item (state,(_tree,_seqNum)) =
                   #if DEBUG
                      printf "\n\n current state:\n %A \n\n subtree_1 \n %A \n\n subtree_2\n %A \n tree:\n%A\n" item _tree tree [Node(_tree@tree,item.prod_name,{prodNum = item.prod_num;seqNum = _seqNum;varNum =1;value = Value.NodeV(null)})]
                   #endif 
                      state, ([Node(_tree@tree,item.prod_name,
                                    {prodNum = item.prod_num;
                                     seqNum = seqNum;
                                     varNum =1;
                                     value = Value.NodeV(null:obj)})]
                               ,item.seq_number)      
                yield Set.filter (fun ((item,_),_) -> item.item_num > 0)
                                 (climb(Set.map create_new_item states,(item.prod_name,i),getLexeme))
             if Set.exists (fun (itm,_) -> Set.exists ((=)item) (nextItem itm tables.Items))
                            states
             then yield Set.map (fun itm -> (itm, (fst(snd (states.MinimumElement))@tree,seqNum)), i) prev_itms 
              }  |> Set.unionMany
             )                

  and parse =
      memoize (
        fun (states,i,getLexeme) -> 
        #if DEBUG 
          Log.print_parse states i;
        #endif
          let value = (getLexeme i).value
          let text = (getLexeme i).name
          let leaf_tree item seqNum= 
              [Leaf(text,{prodNum = item.prod_num;
                          seqNum = item.seq_number;
                          varNum = 1;
                          value = Value.LeafV((getLexeme i))})]
              ,item.seq_number
          let new_states = Set.filter (fun (item,_) -> item.next_num=None)states
          let result_states states create_tree = set <| seq{for (item,(tree,seqNum)) in states -> item,(create_tree item seqNum)}
          Set.map (fun x -> x,i)(result_states new_states (fun x y -> ([],y)))
          + if (getLexeme i = m_end) then Set.Empty else climb(result_states states leaf_tree,(text,i-1),getLexeme)
      )
        
  let run getLexeme inputLength =      
      let startItems = Set.filter (fun item ->is_start item.prod_name) tables.Items
      parse (Set.map (fun item -> item,([],-1)) startItems,inputLength,getLexeme)
      
  member self.Run getLexeme inputLength = run getLexeme inputLength
end