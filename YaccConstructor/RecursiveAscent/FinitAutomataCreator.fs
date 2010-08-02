// FinitAutomataCreator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent

open Yard.Core.IL.Production
open Yard.Core.IL.Source
open Utils

type FinitAutomataCreator (codeGenerator:CodeGenerator) = class
  let varEnumerator = new Enumerator()
  let stateEnumerator = new Enumerator()
  let rec create_NFA   trace = function 
      | PSeq (seq,attr) -> let automataLst = List.map (fun t -> create_NFA  (trace) t.rule) seq 
                           let bindings = List.map (fun elem -> elem.binding) seq 
                           let codeBlocks,bds = List.unzip (List.map (fun (atm:CreatorResult<_,_>) -> atm.actionCode,atm.bindings ) automataLst)
                           let action = if attr.IsNone then "()" else (toString attr.Value)
                           let code = codeGenerator.GenSeq codeBlocks bindings (List.concat bds) action
                           let aut_concat (automata1:CreatorResult<_,_>) (automata2:CreatorResult<_,_>) =
                              let fa1 = automata1.automata
                              let fa2 = automata2.automata
                              let newFS = FAState((fa1.finaleState:FAState).num)
                              CreatorResult( FinitAutomata( (newFS ,None,[],fa2.startState)::fa1.rules @ fa2.rules
                                                           ,fa1.startState
                                                           ,fa2.finaleState)
                                            ,"",[])                                                      
                           let concatedAtm = List.fold aut_concat automataLst.Head automataLst.Tail                                          
                           let newAutomata =
                               let s,f = stateEnumerator.Next(),stateEnumerator.Next()
                               let ss,fs = concatedAtm.automata.startState,concatedAtm.automata.finaleState
                               FinitAutomata(  (FAState(s),None,[TSeqS(s)],ss)
                                             ::(fs,None,[TSeqE(s)],FAState(f))
                                             ::concatedAtm.automata.rules, FAState(s), FAState(f))      
                           CreatorResult (newAutomata, code, ["seq"])                                     
                                                          
      | PAlt (l,r)      -> match create_NFA trace l, create_NFA trace r with
                           | atm1,atm2 ->                               
                               let code = codeGenerator.GenAlt atm1.actionCode atm2.actionCode atm1.bindings atm2.bindings
                               let s,f = stateEnumerator.Next(),stateEnumerator.Next()
                               let fa1 = atm1.automata                               
                               let fa2 = atm2.automata
                               let ls,lf,rs,rf = 
                                   fa1.startState,fa1.finaleState,fa2.startState,fa2.finaleState                              
                               let newS = FAState(s)
                               let newF = FAState(f)
                               CreatorResult( FinitAutomata(  (newS,None,[TAlt1S(s)],ls)::(newS,None,[TAlt2S(s)],rs)::(lf,None,[TAlt1E(s)],newF)::(rf,None,[TAlt2E(s)],newF)
                                                            ::atm1.automata.rules@atm2.automata.rules,newS,newF)
                                             ,code,["alt"])
                    
      (*it is dirty hack. IT MUST BE FIXED*)              
      | PMany (expr)    (*->  (let (rules1,s1,f1) = (create_NFA expr)
                             let (rules2,s2,f2) = (create_NFA (PSome expr))
                             //let ns,nf = next(),next()
                             ([f1,None,s2]@rules1@rules2,s1,f2)) *)
      | PSome (expr)    ->  
          (function (atm:CreatorResult<_,_>) ->            
                    let code = codeGenerator.GenSome atm.actionCode atm.bindings
                    let a = atm.automata                    
                    let s,f = stateEnumerator.Next(),stateEnumerator.Next()
                    CreatorResult(FinitAutomata(  (FAState(s),None,[TClosureS(s)],a.startState)
                                                ::(a.startState,None,[TClosureE(s)],FAState(f))
                                                ::(a.finaleState,None,[],a.startState)
                                                ::(a.startState,None,[],a.finaleState)
                                                ::a.rules,FAState(s),FAState(f)),code,["cls"]))
                   (create_NFA  trace expr)                          
      | PToken(ch)
      | PRef(ch,_)
      | PLiteral(ch) as t -> 
         let s1,f1 = stateEnumerator.Next(),stateEnumerator.Next()
         let s2,f2 = stateEnumerator.Next(),stateEnumerator.Next()
         let num = varEnumerator.Next()
         let code = codeGenerator.GenLeaf()         
         let startState = FAState(s2)
         let finaleState = FAState(f2)
         let st1 = FAState(s1)
         let st2 = FAState(f1)         
         CreatorResult(FinitAutomata(  (st1,Some(t,num),[],st2)
                                     ::(startState,None,[TSmbS(s2)],st1)
                                     ::[(st2,None,[TSmbE(s2)],finaleState)]
                                     ,startState,finaleState)
                       ,code
                       ,["x"+num.ToString()])
      | x -> failwith "You should support new elem" 
            
  let states rules = List.fold (fun buf (a,b,t,c) -> buf+(Set.ofList[a;c])) Set.empty rules      
       
  let e_closure (rules,s,f) =   
      let findTraces q = 
          let ot = ref [] 
          let traces = ref [[q]]
          let visitedState = ref []
          let counter = ref 0
          while (!ot <> ! traces) do
              for trace in !traces
                  do let newEnds = List.map (fun (s2,ch2,tag,f2) -> s2) 
                                            (List.filter (fun (s2,ch2,tag,f2) -> f2 = List.head trace && Option.isNone ch2) rules)
                     let newTraces =  
                         if not newEnds.IsEmpty
                         then  (List.map (fun x -> x::trace) newEnds) 
                             @ (let rec mFilter lst =
                                    match lst with
                                    | hd::tl -> if hd = trace 
                                                then tl 
                                                else hd::(mFilter tl)
                                    | [] -> []
                                mFilter !traces)
                         else !traces      
                          
                     ot := !traces
                     counter:=0
                     if not newTraces.IsEmpty
                     then traces := newTraces 
          let key = List.map (fun (s,ch,t,f) -> ch)
                             (List.filter (fun (s,ch,t,f) -> s = q)
                                          rules)
          
          key                                           
          ,List.map (fun trace ->
                        let rec build trc = 
                            match trc with
                            | hd1::hd2::tl ->  
                                match List.find (fun (s,ch,tag,f) -> s = hd1 && f = hd2) rules with
                                | (_,_,tag,_) -> tag@(build (hd2::tl))
                            | _ -> []
                        build trace) !traces
                     
                     
      let closure q = 
          let q' = ref (Set.singleton q)
          let trace = ref []
          let l = ref 0
          while (!l < Set.count !q') do
             l:= Set.count !q';
             for s1 in !q' 
                 do for (s2,ch2,tag,f2)as state' in rules 
                        do if s2=s1 && ch2=None
                           then q':= (Set.add f2) !q'
                                trace := if (List.isEmpty tag || List.contains tag !trace) 
                                         then !trace 
                                         else (tag::!trace)
          let trace = (findTraces q)                       
#if DEBUG 
          printfn "--------------------------\n key = %A"(fst (findTraces q));
          List.iter (fun x -> printfn "trace = %A \n_________________________\n" x) (snd (findTraces q));
#endif
          !q'
          , if List.exists (fun x -> Option.isNone x) (fst trace) 
            then [],[] 
            else trace
       in       

       let closureSet = Set.map (fun x -> (x,closure x)) (states rules)
       let isSubset sttset (_,(elt:Set<'a>*'b)) = 
           if Set.exists (fun x -> ((fst elt).IsSubsetOf (fst x))&&(not(elt.Equals (x)))) sttset 
           then Set.remove elt sttset 
           else sttset
           
       let mFilter lst =
           let l = lst
           let rec _inner lst = 
               match lst with
               | hd::tl -> 
               if List.exists (fun elt -> Set.isSubset (Set.ofList (snd hd))(Set.ofList (snd elt)) 
                                          &&
                                          (Set.ofList (snd hd))<>(Set.ofList (snd elt))) l
               then _inner tl
               else hd::(_inner tl)
               | [] -> []
           _inner lst
               
       let newStates = 
           Set.map (fun (state,_) -> 
                        state
                        , Set.map (fun x ->
                                       Set.filter (fun (m,n) -> m = x) closureSet
                                       |> Set.map (fun(_,(_,y)) ->[y])
                                       |> List.concat
                                  ) state
                          |> List.concat
                          |> mFilter )
                                                    
                   (Set.fold isSubset (Set.ofList(snd(List.unzip (Set.toList closureSet)))) closureSet)
       let new_automata = 
           List.concat [for stt in newStates ->
                          List.concat[for (x,y,t,z) in rules do
                                        if (Set.exists ((=)x) (fst stt))&&(Option.isSome y)
                                        then yield [for q in newStates do
                                                      if Set.exists ((=)z) (fst q) 
                                                      then yield (stt,y,q)]]]     
       let alterName = 
           let newStatesLst = Set.toList newStates
           let _alterNames = List.map2 (fun state newNumber -> FAState(newNumber),snd state)
                                       newStatesLst [0..newStates.Count - 1]
           dict (List.zip  newStatesLst _alterNames)
           
       let new_rule (state,symbol,next) = 
             fst alterName.[state]
           , symbol
           , snd alterName.[state] 
             @(if Set.contains f (fst next) 
               then snd alterName.[next] 
               else [])
           , fst alterName.[next]
           
       let cleanNewAutomata = Set.map new_rule (Set.ofList new_automata)
       let setAlterName = Set.map (fun stt -> alterName.[stt])     
       let findState stt =  setAlterName (Set.filter (fun x -> Set.exists ((=)stt) (fst x)) newStates)
       let newFinaleState = Set.map fst (findState f)
       //it is really only one start state
       let newStartState = fst (findState s).MinimumElement
       in
  #if DEBUG          
       Log.print_autonaton newStates cleanNewAutomata newStartState newFinaleState closureSet (states rules);
  #endif
       (cleanNewAutomata,newStartState,newFinaleState)
       
  let fa_rules rule =       
      codeGenerator.ResetValueEnumerator();    
      codeGenerator.ResetValueExtraction();    
      let _result = create_NFA [] rule in 
  #if DEBUG 
      (printf "\n Fa_rule : \n %A " (_result.automata.rules));
  #endif
      let fa = _result.automata
      e_closure (fa.rules,fa.startState,fa.finaleState), _result.actionCode,_result.bindings
           
  member self.FA_rules rule = varEnumerator.Reset(); fa_rules rule
end