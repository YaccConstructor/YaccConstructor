// FinitAutomataCreator.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

open IL.Production
open IL.Source
open Utils

type FinitAutomataCreator (codeGenerator:CodeGenerator) = class
  let varEnumerator = new Enumerator()
  let altEnumerator = new Enumerator()
  let stateEnumerator = new Enumerator()
  let rec create_NFA seq_num  = function 
      | PSeq (seq,attr) -> let new_autom = List.map (fun t -> create_NFA seq_num t.rule) seq 
                           let bindings = List.map (fun elem -> elem.binding) seq                                                  
                           let aut_concat (automata1:CreatorResult<_,_>) (automata2:CreatorResult<_,_>) b=
                              let fa1,code1,bindings1 = automata1.automata, automata1.actionCode, automata1.bindings                              
                              let fa2,code2,bindings2 = automata2.automata, automata2.actionCode, automata2.bindings 
                              CreatorResult( FinitAutomata( (fa1.finaleState,None,fa2.startState)::fa1.rules@fa2.rules
                                                           ,fa1.startState
                                                           ,fa2.finaleState)
                                            ,code1+(codeGenerator.GenBynding b bindings2 code2),bindings1@bindings2)
                           let rebuld (automata:CreatorResult<_,_>) b =                               
                              CreatorResult( automata.automata
                                            ,codeGenerator.GenBynding b automata.bindings automata.actionCode
                                            ,automata.bindings)
                           let action = if attr.IsNone then "" else (toString attr.Value)
                           let bindingValueMap = codeGenerator.GenBindingMap bindings
                           let _result=
                               List.fold2 aut_concat 
                                         (rebuld new_autom.Head (fst bindingValueMap.Head))
                                         new_autom.Tail 
                                         (List.map fst bindingValueMap.Tail)
                                         
                           CreatorResult( _result.automata
                                         ,codeGenerator.GenSeq _result.actionCode _result.bindings action
                                         ,_result.bindings)                                       
                                                          
      | PAlt (l,r)      -> let lAltNum = ref 0
                           let rAltNum = ref 0
                           match (create_NFA (lAltNum:=altEnumerator.Next();!lAltNum) l,
                                  (codeGenerator.ResetValueExtraction()
                                   create_NFA (rAltNum:=altEnumerator.Next()
                                               !rAltNum) r)) with
                           | atm1,atm2 ->                               
                               let code = codeGenerator.GenAlt atm1.actionCode atm2.actionCode atm1.bindings atm2.bindings
                               let s,f = stateEnumerator.Next(),stateEnumerator.Next()
                               let ls,lf,rs,rf = 
                                   let fa1 = atm1.automata
                                   let fa2 = atm2.automata
                                   fa1.startState,fa1.finaleState,fa2.startState,fa2.finaleState
                               CreatorResult( FinitAutomata(  (s,None,ls)::(s,None,rs)::(lf,None,f)::(rf,None,f)
                                                            ::atm1.automata.rules@atm2.automata.rules,s,f)
                                             ,code,atm1.bindings@atm2.bindings)
                    
      (*it is dirty hack. IT MUST BE FIXED*)              
      | PMany (expr)    (*->  (let (rules1,s1,f1) = (create_NFA expr)
                             let (rules2,s2,f2) = (create_NFA (PSome expr))
                             //let ns,nf = next(),next()
                             ([f1,None,s2]@rules1@rules2,s1,f2)) *)
      | PSome (expr)    ->  
          (function (atm:CreatorResult<_,_>) ->            
                    let code = codeGenerator.GenSome atm.actionCode atm.bindings
                    let a = atm.automata
                    CreatorResult(FinitAutomata(  (a.finaleState,None,a.startState)
                                                ::(a.startState,None,a.finaleState)
                                                ::a.rules,a.startState,a.finaleState),code,atm.bindings))
                   (create_NFA seq_num expr )                          
      | PToken(ch)
      | PRef(ch,_)
      | PLiteral(ch) as t -> 
         let s,f = stateEnumerator.Next(),stateEnumerator.Next()
         let code = "\n"
         let num =varEnumerator.Next()
         CreatorResult(FinitAutomata([(s,Some(t,num),f)],s,f),code,["x"+num.ToString()])
      | x -> failwith "You should support new elem" 
          
  let states rules = List.fold (fun buf (a,b,c) -> buf+(Set.ofList[a;c])) Set.empty rules      
       
  let e_closure (rules,s,f) =    
      let exists_e_elt = ref Set.empty               
      let closure q = 
          let q' = ref (Set.singleton q)
          let l = ref 0
          while (!l < Set.count !q') do
             l:= Set.count !q';
             for s1 in !q' 
                 do for (s2,ch2,f2)as state' in rules 
                        do if s2=s1 && ch2=None
                           then q':= (Set.add f2) !q'
              
          !q'          
       in
       let get_rpart stt = set [for state,symbol,next in  rules do if state=stt && symbol<>None then yield symbol,next]

       let closure_set = Set.map (fun x -> exists_e_elt:=Set.empty;(x,closure x)) (states rules)
       let is_subset sttset (_,elt:Set<'a>) = 
           if Set.exists (fun x -> (elt.IsSubsetOf x)&&(not(elt.Equals x))) sttset 
           then Set.remove elt sttset 
           else sttset    
       let new_states = Set.fold is_subset (Set.ofList(snd(List.unzip (Set.toList closure_set)))) closure_set
       let new_automata = 
           List.concat [for stt in new_states ->
                          List.concat[for (x,y,z) in rules do
                                        if (Set.exists ((=)x) stt)&&(Option.isSome y)
                                        then yield [for q in new_states do
                                                      if Set.exists ((=)z) q then yield (stt,y,q)]]]     
       let alter_name = dict (List.zip (Set.toList new_states) [0..new_states.Count-1])      
       let new_rule (state,symbol,next) = alter_name.[state],symbol,alter_name.[next]
       let clean_new_automata = Set.map new_rule (Set.ofList new_automata)
       let set_alter_name = Set.map (fun stt -> alter_name.[stt])     
       let find_state stt = set_alter_name (Set.filter (fun x -> Set.exists ((=)stt) x) new_states)
       let new_finale_state = find_state f
       //it is really only one start state
       let new_start_state =(find_state s).MinimumElement
       in
  #if DEBUG          
       Log.print_autonaton new_states clean_new_automata new_start_state new_finale_state closure_set (states rules);
  #endif
       (clean_new_automata,new_start_state,new_finale_state)
       
  let fa_rules rule =       
      codeGenerator.ResetValueEnumerator();    
      codeGenerator.ResetValueExtraction();    
      let _result = create_NFA 0 rule in 
  #if DEBUG 
      (printf "\n Fa_rule : \n %A " (_result.automata.rules));
  #endif
      let fa = _result.automata
      e_closure (fa.rules,fa.startState,fa.finaleState), _result.actionCode,_result.bindings
           
  member self.FA_rules rule = varEnumerator.Reset();fa_rules rule
end