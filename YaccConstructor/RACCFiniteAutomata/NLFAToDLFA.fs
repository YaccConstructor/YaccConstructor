// NLFAToDLFA.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.
//
//Implementation of NLFA to DLFA convertion

module Yard.Generators.RACC.NLFAToDLFA


//see Dragon book p129
let NLFAToDLFA (nlfa:NLFA<_,_,_>) =
    let symbols = List.map (fun rule -> rule.Symbol) nlfa.Rules
    let move stateSet symbol = 
        Set.map 
            (fun state -> List.map 
                               (fun rule -> rule.ToStateID)
                               (List.filter 
                                     (fun rule -> rule.FromStateID = state && rule.Symbol = symbol) 
                                     nlfa.Rules))
            stateSet

    let eClosure statesSet = 
        let stack = ref (List.ofSeq statesSet)
        let eCls = ref statesSet
        while not (List.isEmpty !stack) do            
            let t = (!stack).Head
            stack := (!stack).Tail   
            List.iter 
                 (fun state -> 
                      if not (Set.exists ((=)state) !eCls)
                      then 
                        eCls := Set.add state !eCls
                        stack := state::!stack)
                 (List.map 
                       (fun rule -> rule.ToStateID)
                       (List.filter (fun rule -> rule.Symbol = Epsilon && rule.FromStateID = t) nlfa.Rules))
        done
        !eCls

    let newStates = ref [(false,eClosure nlfa.StartStates)]
    let newRules = 1
    1


