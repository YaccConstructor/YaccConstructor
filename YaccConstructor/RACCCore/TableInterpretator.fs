// TableInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.
 
namespace Yard.Generators.RecursiveAscent

open Grammar.Item
open AST
open Utils

type TableInterpretator (tables: TablesLoader,getLexeme) = class  
    
  let m_end = {name = "$";value = "$"}    
         
  let climbVisitsCount = ref 0           
  let parseVisitsCount = ref 0
  
  let isStart symbolName = List.exists ((=) symbolName) tables.StartNterms

  let memoize f =
     let t = new System.Collections.Generic.Dictionary<_,_>()
     fun (parserState:ParserState<_,_,_,_>) ->        
         let id = hash(parserState)
         let key = parserState
         if t.ContainsKey(key)       
         then
#if DEBUG         
              printfn "\n FOUND \n";
#endif              
              t.[key] 
         else 
#if DEBUG                  
            printfn "\n NOTFOUND \n";
#endif                        
            let res = f(parserState) 
            t.Add(key,res)
            res                     

  let goto states symbol =             
      seq { for (state:State<_,_,_,_>) in states                
             -> let  h = hash (state.item,symbol)
                let _item = state.item
                let h2 = hash (_item,symbol)
                seq {for item in (tables.GotoSet.[hash (state.item,symbol)]) -> 
                              let _trace =                                     
                                  List.filter (fun(k,v) -> not (List.isEmpty k)) item.fromStateTrace
                                  |> List.unzip
                                  |> snd
(*#if DEBUG                                                 
                              printf "---------------------------TRACE = %A \n" item.fromStateTrace                                    
                              printf "---------------------------Symbol = %A \n" _trace
                              printf "--------------------------------------------------------------------\n"
#endif*)                                                                                                            
                              State(  item
                                    , state.trees
                                    , (if item.prod_num = state.item.prod_num 
                                       then state.trace 
                                       else  List.filter (fun(k,v) -> (not (List.isEmpty k)) && (List.exists ((=)(Some(symbol)))) k)
                                                         (((Set.maxElement (prevItem item tables.Items)):Grammar.Item.t<_>).fromStateTrace)
                                             |> List.unzip
                                             |> snd ) 
                                      @ _trace
                                    )}
                  |>set} 
      |> Set.unionMany
                                      
  let rec climb =    
    let calculate states symbol position  = 
      incr climbVisitsCount; 
      let gt = goto states symbol
      let newStates = parse (ParserState(gt, symbol, position))
      (*if newStates.IsEmpty
      then 
        let possidleErrors = Set.map (fun (x:ParserResult<_,_,_,_>) -> {ePosition = x.position}) 
        let errorHandler = ErrorHandler.ErrorHandler()
        ()*)
#if DEBUG      
      Log.print_climb_info position symbol states gt newStates        
#endif     
      
      let inline checker (parserResult:ParserResult<_,_,_,_>) =
        let item = parserResult.state.item 
        isStart item.prod_name && item.next_num=None && parserResult.position=1
                
            
      if Set.exists checker newStates    
      then seq {for state in states do
                    if state.item.next_num.IsNone 
                    then yield ParserResult(state,1)}
           |>set          
      else
        seq {for (parserResult:ParserResult<_,_,_,_>) in newStates do
               let item,trees,position = parserResult.state.item, parserResult.state.trees, parserResult.position                             
               let prevItms = prevItem item tables.Items
               let checker _item = Option.get _item.symb = symbol && _item.item_num=item.s
               
               if Set.exists checker prevItms && not(isStart item.prod_name)  
               then 
                  let createNewItem (state:State<_,_,_,_>) =
                    let newNode () = 
#if DEBUG
                      printfn "\n %A reduce %A" (String.concat "" (List.map (fun x -> " ") [0..position])) item.prod_name 
#endif                     
                      Node(
                           state.trees@trees
                           ,item.prod_name
                           ,{
                             prodNum = item.prod_num                             
                             trace = parserResult.state.trace
                             value = Value.NodeV(null:obj)
                            }
                           )                      
                      
                    State(state.item,[newNode()], state.trace)                                                                           
                  let newStates = climb(ParserState(Set.map createNewItem states, item.prod_name, position))
                  let inline filter (parserResult:ParserResult<_,_,_,_>) = parserResult.state.item.item_num > 0
                  yield Set.filter filter newStates
                  
               let checker (state:State<_,_,_,_>) = Set.exists ((=)item) (nextItem state.item tables.Items)
               
               if Set.exists checker states
               then 
                  let createResult item = 
                    ParserResult(State( item
                                       ,states.MinimumElement.trees @ trees
                                       ,parserResult.state.trace)
                                       ,position)
                  yield Set.map createResult prevItms
              }  |> Set.unionMany               
                  
    let climbFunction (parserState:ParserState<_,_,_,_>)  =
      let states,symbol,position = 
          parserState.states, parserState.symbol, parserState.position  
                               
      if Set.isEmpty states
      then Set.empty
      else calculate states symbol position 

    memoize(fun parserState -> climbFunction parserState)

  and parse =      
      memoize                 
       (fun (parserState) -> 
            incr parseVisitsCount;
            let states = parserState.states
            let position = parserState.position
#if DEBUG 
            Log.print_parse states position
#endif
            let value = (getLexeme position)
            let text = (getLexeme position).name
            let leafTree item = 
#if DEBUG
                  printfn "\n %A shift %A" (String.concat "" (List.map (fun x -> " ") [0..position])) text 
#endif
                  [Leaf( text
                        ,{
                           prodNum = item.prod_num                              
                           trace = []
                           value = Value.LeafV(value)
                         }
                        )]
                  
            let newStates = Set.filter (fun (state:State<_,_,_,_>) -> state.item.next_num = None) states          
            let resultStates states create_tree i =
                Set.map (fun(_state:State<_,_,_,_>)  -> 
                          State(_state.item, create_tree _state.item, 
                                  
                                        if  i
                                        then _state.trace @ (List.filter (fun(k,v) -> not (List.isEmpty k)) _state.item.fromStateTrace
                                             |> List.unzip
                                             |> snd)                                             
                                        else _state.trace     
                                       )
                         )
                         states
                                                                   
            let res1 = Set.map (fun state -> ParserResult(state,position)) (resultStates newStates (fun _  -> []) false)
            let res2 = if getLexeme position = m_end
                       then Set.empty
                       else climb(ParserState(resultStates states leafTree true,text,position-1))
            if res1.IsEmpty && res2.IsEmpty 
            then 
                let possibleError = {ePosition = position}
                let errorHandler = ErrorHandler()
                errorHandler.Handle possibleError
                //printfn "Error is detected in position %A" position

            res1+res2
       )
      
        
  let run inputLength =      
      let startItems = Set.filter (fun item -> isStart item.prod_name) tables.Items
      parse (ParserState (Set.map (fun item -> State (item,[],[])) startItems, "", inputLength))
      
  member self.Run inputLength = run inputLength
  member self.ClimbVisitsCount with get () = !climbVisitsCount
  member self.ParseVisitsCount with get () = !parseVisitsCount
end