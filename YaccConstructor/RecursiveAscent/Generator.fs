// Generator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Generators.RecursiveAscent.Generator

open Yard.Core.IL.Production
open Yard.Core.IL.Rule
open Yard.Core.IL
open Grammar.Item
open GrammarPreparer


let start_time = ref System.DateTime.Now
let end_time   = ref System.DateTime.Now      

let items,_grammar,_generate, ruleToActionMap=
    let _items = ref Set.empty
    let _grammar = ref[]
    let _ruleToActonMap = ref[]
    let generate rules path =
       let codeGenerator = new CodeGenerator(path,path+".fs")
       
       codeGenerator.Write (codeGenerator.GenHeader())
       codeGenerator.Write (codeGenerator.GenDefaultFunctions)
       
       let finitAutomata = new FinitAutomataCreator(codeGenerator) 
       
       _grammar := rules
       
       let rulesMap  = List.zip ([0..(List.length rules)-1])rules
       
       let creatItem (i,rule) =
          let (itm,s:FAState,f:Set<FAState>),code,binding = finitAutomata.FA_rules rule.body
          let topLevelBindingName = rule.name+i.ToString()+"_action"
          _ruleToActonMap:=(i,topLevelBindingName)::(!_ruleToActonMap)
          
          if rule.name<>"_yard_start" 
          then codeGenerator.Write (codeGenerator.GenTopLEvelBinding topLevelBindingName code binding)
          
          let get_symb = 
              function 
              | Some((PLiteral(s)|PToken(s)|PRef(s,_)),_) -> Some(Source.toString s)                                                                                  
              | _ -> None
              
          let getSeqNum = 
              function 
              | Some(_,seqNum) -> seqNum                                                                                  
              | _ -> failwith "Generator error. Can not find seqNumber"                                                                          
              
#if DEBUG
          Log.print_item itm s f;
#endif
          let _createItem buf (fromS:FAState,symbol,tr,toS:FAState) =
              let _filter = 
                  fun (k,v) -> 
                      match k with
                      | []  -> [None],v
                      | lst -> if List.exists (fun x -> (get_symb x) = (get_symb symbol)) lst
                               then List.map get_symb k ,v  
                               else [],[]
                               
              let newItem  itemNum nextNum fromSTrace toSTrace=
                {
                 prod_num = i
                 prod_name = rule.name
                 item_num = itemNum
                 symb = get_symb symbol
                 next_num = nextNum
                 seq_number = getSeqNum symbol
                 s = s.num
                 f = Set.map (fun (_f:FAState) -> _f.num) f
                 fromStateTrace = List.map _filter tr
                 toStateTrace = List.map _filter tr
                }
                
              buf 
              + Set.singleton(newItem fromS.num (Some(toS.num)) tr tr)
              + if Set.exists ((=)toS.num) (Set.map (fun (_f:FAState) -> _f.num) f)
                then Set.singleton(newItem toS.num None tr tr)
                else Set.empty 
               
          Set.fold _createItem Set.empty itm
       _items := List.map creatItem rulesMap |> Set.unionMany;
       codeGenerator.CloseOutStream() 
    let items () = !_items
    let grammar () = !_grammar
    let ruleToActionMap ()=  !_ruleToActonMap
    items, grammar, generate, ruleToActionMap
    
let closure q = 
    let q' = ref (set q)
    let l = ref 0
    while (!l < Set.count !q') do
        l:= Set.count !q';
        for item in !q' 
            do for item' in (items()) 
                   do if Option.get item.symb = item'.prod_name && item'.item_num = 0
                      then q':= Set.add item' !q'
        
    !q'              
        
let getClosureSet, calcClosureSet = 
#if DEBUG
  Log.print_items (items())
#endif
  let _closureSet = ref( dict [])
  let calculateClousure_set () =
      _closureSet := dict <| Set.map (fun x -> x, closure (Set.singleton x)) (items())                
  let closureSet () = !_closureSet
  closureSet,calculateClousure_set

let goto_set ()=         
    let make_goto q x =  
        calcClosureSet()
        let closure = Set.fold (fun y x -> y + getClosureSet().[x]) Set.empty q
        Set.unionMany 
          <|seq {for item in closure do 
                  if x = Option.get item.symb 
                  then 
                     yield Set.map 
                           (fun itm -> 
                                {itm with fromStateTrace = (if item.fromStateTrace <> itm.fromStateTrace && item.prod_num = itm.prod_num
                                                            then ((*item.fromStateTrace @*) itm.fromStateTrace)
                                                            else itm.fromStateTrace )}
                           ) 
                           (Utils.nextItem item (items()))
                }
                
    let toString = 
        function | PToken y 
                 | PLiteral y 
                 | PRef (y,_) -> Source.toString y 
                 | _          -> ""
                 
    let goto_data symbol item = 
        let gt = make_goto (Set.singleton item) symbol
#if DEBUG        
        printf "\n GOTO \n:";
        Log.print_goto_c symbol item gt;
#endif        
        hash(item, symbol),gt
        
    Set.fold (fun buf symbol -> buf @ [for item in (items()) -> goto_data symbol item]) 
                     [] 
                     (GrammarPreparer.get_all_t(_grammar()))
                       
let generate input_grammar = 
    let head, rules, foot = GrammarPreparer.prepare input_grammar
    
    let addStartRule rules = 
        List.fold (fun rules rule_name -> (GrammarPreparer.createStartRule "_yard_start" rule_name)::rules)
                  (replacePublic rules)
                  (GrammarPreparer.get_start_nterms rules)
                  
    _generate((addStartRule rules))(input_grammar.info.fileName)
    
#if DEBUG    
    printf "Transformed grammar \n %A\n" <|_grammar()
    printf "\n Token list: \n  ";
    Set.iter (printf "%A;")(GrammarPreparer.get_all_t(_grammar()))
    printf "\n Start Nterms: \n %A " <|GrammarPreparer.get_start_nterms (_grammar())
#endif

    let gotoSet = goto_set()
    let items = items()
    let startNTerms = GrammarPreparer.get_start_nterms (_grammar())
    let ruleToActionMap = ruleToActionMap()
    let l = System.Linq.Enumerable.ToList(gotoSet)
    //IO.writeValue (input_grammar.info.fileName + ".goto.dta") (System.Linq.Enumerable.ToList(gotoSet))
    //IO.writeValue (input_grammar.info.fileName + ".items.dta") items
    //let printList lst printItem = "[" + String.concat ";" (List.map printItem lst) + "]"
    IO.writeTables input_grammar.info.fileName gotoSet items startNTerms ruleToActionMap    
    IO.writeValue (input_grammar.info.fileName + ".start_nterms.dta") startNTerms
    IO.writeValue (input_grammar.info.fileName + ".rule_to_action.dta") (System.Linq.Enumerable.ToList(ruleToActionMap))
    printfn "End working time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time))
    (gotoSet,items,startNTerms,ruleToActionMap)