// TableInterpretator.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.
 
namespace Yard.Core

open Grammar.Item
open AST
open Utils
open Lexeme 

type TableInterpretator (tables: TablesLoader,getLexeme) = class

  let m_end = {name = "$";value = "$"}
             
  let is_start symbol_name = List.exists ((=) symbol_name) tables.StartNterms

  let memoize f =
     let t = new System.Collections.Generic.Dictionary<_,_>()
     fun (parserState:ParserState<_,_,_>) ->        
         let id = hash(parserState)
         let key = parserState
         if t.ContainsKey(key)       
         then t.[key] 
         else 
            let res = f(parserState) 
            t.Add(key,res)
            res                     

  let goto (states,symbol) = 
      Set.unionMany 
        <| seq { for (state:State<_,_,_>) in states 
                 -> set <| seq {for z in (tables.GotoSet.[hash (state.item,symbol)]) 
                                -> State(z,state.trees)}}
                                    
  let rec climb = 
    let calculate states symbol position=    
      let gt = goto (states,symbol)     
      let newStates = parse (ParserState(gt,symbol,position))
      #if DEBUG      
      Log.print_climb_info position symbol states gt newStates        
      #endif     
      
      let checker (parserResult:ParserResult<_,_,_>) =
        let item = parserResult.state.item 
        is_start item.prod_name && item.next_num=None && parserResult.position=1
                
      if Set.exists checker newStates    
      then set <|seq {for state in states do
                        if state.item.next_num.IsNone 
                        then yield ParserResult(state,1)}
      else
        seq {for (parserResult:ParserResult<_,_,_>) in newStates do
               let item,trees,position = parserResult.state.item, parserResult.state.trees,parserResult.position                             
               let prevItms = prevItem item tables.Items
               let checker _item = Option.get _item.symb = symbol && _item.item_num=item.s
               if Set.exists checker prevItms && not(is_start item.prod_name)  
               then 
                  let createNewItem (state:State<_,_,_>) =
                    let newNode = 
                      Node(state.trees@trees
                           ,item.prod_name
                           ,{prodNum = item.prod_num;
                             seqNum = item.seq_number;                                                          
                             varNum = 1;
                             value = Value.NodeV(null:obj)})
                    State(state.item,[newNode])                   
                  let newStates = climb(ParserState(Set.map createNewItem states, item.prod_name, position))
                  let filter (parserResult:ParserResult<_,_,_>) = parserResult.state.item.item_num > 0
                  yield Set.filter filter newStates
               let checker (state:State<_,_,_>) = Set.exists ((=)item) (nextItem state.item tables.Items)
               if Set.exists checker states
               then 
                  let createResult item = 
                    ParserResult(State(item,states.MinimumElement.trees@trees), position)
                  yield Set.map createResult prevItms
              }  |> Set.unionMany  
               
    let climbFunction (parserState:ParserState<_,_,_>) =
      let states,symbol,position = parserState.states, parserState.symbol, parserState.position                      
      if Set.isEmpty states
      then Set.empty
      else calculate states symbol position

    memoize(fun parserState -> climbFunction parserState)


(*  let rec climb =
      memoize (fun (parserState1:ParserState<_,_,_>) -> 
      let states,symbol,position = parserState1.states, parserState1.symbol, parserState1.position   
      if Set.isEmpty states
      then Set.empty
      else    
      let gt = goto (states,symbol)     
      let new_states = parse (ParserState(gt,symbol,position))
  #if DEBUG      
      Log.print_climb_info position symbol states gt new_states;        
  #endif             
      if Set.exists (fun (parserResult:ParserResult<_,_,_>) -> is_start parserResult.state.item.prod_name && parserResult.state.item.next_num=None && parserResult.position=1) new_states     
      then set <|seq {for _state as state in states do if _state.item.next_num = None then yield ParserResult(state,1)}
      else
        seq {for (parserResult:ParserResult<_,_,_>) in new_states do
             let item,tree,i = parserResult.state.item, parserResult.state.trees,parserResult.position
             let prev_itms = prevItem item tables.Items                   
             if Set.exists (fun itm -> Option.get itm.symb = symbol && itm.item_num=item.s) prev_itms 
                && not(is_start item.prod_name)  
             then 
                let create_new_item (state:State<_,_,_>) =
                      let _tree = state.trees
                   #if DEBUG
                      printf "\n\n current state:\n %A \n\n subtree_1 \n %A \n\n subtree_2\n %A \n tree:\n%A\n" item _tree tree [Node(_tree@tree,item.prod_name,{prodNum = item.prod_num;seqNum = item.seq_number;varNum =1;value = Value.NodeV(null)})]
                   #endif 
                      State(state.item, [Node(_tree@tree,item.prod_name,{prodNum = item.prod_num;seqNum = item.seq_number;varNum =1;value = Value.NodeV(null:obj)})]
                      )
                yield Set.filter (fun (parserResult:ParserResult<_,_,_>) -> parserResult.state.item.item_num > 0)
                                 (climb(ParserState(Set.map create_new_item states,item.prod_name,i)))
             if Set.exists (fun (state:State<_,_,_>) -> Set.exists ((=)item) (nextItem state.item tables.Items))
                            states
             then yield Set.map (fun itm -> ParserResult(State(itm, (states.MinimumElement).trees@tree), i)) prev_itms 
              }  |> Set.unionMany
             )    
      *)       
  and parse =
      memoize         
       (fun (parserState) -> 
            let states = parserState.states
            let position = parserState.position
            #if DEBUG 
            Log.print_parse states position;
            #endif
            let value = (getLexeme position)
            let text = (getLexeme position).name
            let leaf_tree item = 
                  [Leaf(text,{prodNum = item.prod_num;
                              seqNum = item.seq_number;
                              varNum = 1;
                              value = Value.LeafV(value)})]
                  
            let new_states = Set.filter (fun (state:State<_,_,_>) -> state.item.next_num = None) states          
            let result_states states create_tree =
                  set <| seq{ for (state:State<_,_,_>) in states -> State(state.item,create_tree state.item)}                  
            Set.map (fun state -> ParserResult(state,position))(result_states new_states (fun _ -> []))
            + 
            if getLexeme position = m_end
            then Set.empty 
            else climb(ParserState(result_states states leaf_tree,text,position-1))
      )
        
  let run inputLength =      
      let startItems = Set.filter (fun item ->is_start item.prod_name) tables.Items
      parse (ParserState(Set.map (fun item -> State(item,[])) startItems, "", inputLength))
      

  (*let memoize f =
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
      if Set.exists (fun ((item,tree),i) -> is_start item.prod_name && item.next_num=None && i=1) new_states     
      then set <|seq {for item,tree as state in states do if item.next_num = None then yield state,1}
      else
        seq {for (item,tree),i in new_states do
             let prev_itms = prevItem item tables.Items                   
             if Set.exists (fun itm -> Option.get itm.symb = symbol && itm.item_num=item.s) prev_itms 
                && not(is_start item.prod_name)  
             then 
                let create_new_item (state,_tree) =
                   #if DEBUG
                      printf "\n\n current state:\n %A \n\n subtree_1 \n %A \n\n subtree_2\n %A \n tree:\n%A\n" item _tree tree [Node(_tree@tree,item.prod_name,{prodNum = item.prod_num;seqNum = item.seq_number;varNum =1;value = Value.NodeV(1)})]
                   #endif 
                      state, [Node(_tree@tree,item.prod_name,{prodNum = item.prod_num;seqNum = item.seq_number;varNum =1;value = Value.NodeV(1)})]
                yield Set.filter (fun ((item,_),_) -> item.item_num > 0)
                                 (climb(Set.map create_new_item states,(item.prod_name,i),getLexeme))
             if Set.exists (fun (itm,_) -> Set.exists ((=)item) (nextItem itm tables.Items))
                            states
             then yield Set.map (fun itm -> (itm, snd (states.MinimumElement)@tree), i) prev_itms 
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
          let leaf_tree item = 
              [Leaf(text,{prodNum = item.prod_num;
                          seqNum = item.seq_number;
                          varNum = 1;
                          value = Value.LeafV((getLexeme i))})]
          let new_states = Set.filter (fun (item,tree) -> item.next_num=None)states
          let result_states states create_tree = set <| seq{for (item,tree) in states -> item,(create_tree item)}
          Set.map (fun x -> x,i)(result_states new_states (fun x -> []))
          + if (getLexeme i = m_end) then Set.Empty else climb(result_states states leaf_tree,(text,i-1),getLexeme)
      )
        
  let run inputLength =      
      let startItems = Set.filter (fun item ->is_start item.prod_name) tables.Items
      parse (Set.map (fun item -> item,[]) startItems,inputLength,getLexeme)
          *)  
  member self.Run inputLength = run inputLength
end