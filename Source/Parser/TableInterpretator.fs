// TableInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
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
             
  let isStart symbolName = List.exists ((=) symbolName) tables.StartNterms

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
                 -> set <| seq {for item in (tables.GotoSet.[hash (state.item,symbol)]) 
                                -> State(item,state.trees)}}
                                    
  let rec climb = 
    let calculate states symbol position=    
      let gt = goto (states,symbol)     
      let newStates = parse (ParserState(gt,symbol,position))
      #if DEBUG      
      Log.print_climb_info position symbol states gt newStates        
      #endif     
      
      let checker (parserResult:ParserResult<_,_,_>) =
        let item = parserResult.state.item 
        isStart item.prod_name && item.next_num=None && parserResult.position=1
                
      if Set.exists checker newStates    
      then set <|seq {for state in states do
                        if state.item.next_num.IsNone 
                        then yield ParserResult(state,1)}
      else
        seq {for (parserResult:ParserResult<_,_,_>) in newStates do
               let item,trees,position = parserResult.state.item, parserResult.state.trees,parserResult.position                             
               let prevItms = prevItem item tables.Items
               let checker _item = Option.get _item.symb = symbol && _item.item_num=item.s
               if Set.exists checker prevItms && not(isStart item.prod_name)  
               then 
                  let createNewItem (state:State<_,_,_>) =
                    let newNode = 
                      Node(state.trees@trees
                           ,item.prod_name
                           ,{prodNum = item.prod_num;
                             seqNum = 1;//item.seq_number;                                                          
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
      let startItems = Set.filter (fun item ->isStart item.prod_name) tables.Items
      parse (ParserState(Set.map (fun item -> State(item,[])) startItems, "", inputLength))
      
  member self.Run inputLength = run inputLength
end