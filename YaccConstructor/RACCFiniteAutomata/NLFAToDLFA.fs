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
    let symbols = Set.filter ((<>) Epsilon) (Set.map (fun rule -> rule.Symbol) nlfa.Rules)
    let stateIDs = Set.ofSeq nlfa.IDToStateMap.Keys
    let eLinesSet = ref Set.empty
    
    let notVisitetStates = ref (List.ofSeq nlfa.StartStates)
    let visitedStates = ref []

    let rec eLines state =         
        visitedStates := state :: !visitedStates
        let eSteps = Set.fold 
                        (fun buf rule -> if rule.FromStateID = state && rule.Symbol = Epsilon
                                         then Set.add (rule.ToStateID, rule.Label) buf
                                         else buf)
                        Set.empty
                        nlfa.Rules
        Set.map 
            (fun step ->                
                if not (List.exists ((=) (fst step)) !visitedStates) 
                then
                    let eL = eLines (fst step)
                    if Set.isEmpty eL
                    then Set.singleton [step]
                    else Set.map (fun tl -> step::tl) eL
                else Set.singleton [step]) eSteps 
        |> Set.unionMany           

                     
    let buldELines = 
        Set.map 
            (fun stateID ->
                if not (List.exists ((=) stateID) !visitedStates) 
                then
                    let el = (eLines stateID)
                    eLinesSet := Set.union (!eLinesSet) (Set.map (fun x -> stateID,x)el))
            stateIDs

    let move stateSet symbol = 
        Set.map 
            (fun state -> Set.map 
                               (fun rule -> rule.ToStateID)
                               (Set.filter 
                                     (fun rule -> rule.FromStateID = state && rule.Symbol = symbol) 
                                     nlfa.Rules))
            stateSet
            |> Set.unionMany

    let eClosure statesSet = 
        let stack = ref (List.ofSeq statesSet)
        let eCls = ref statesSet
        while not (List.isEmpty !stack) do            
            let t = (!stack).Head
            stack := (!stack).Tail   
            Set.iter 
                 (fun state -> 
                      if not (Set.exists ((=)state) !eCls)
                      then 
                        eCls := Set.add state !eCls
                        stack := state::!stack)
                 (Set.map 
                       (fun rule -> rule.ToStateID)
                       (Set.filter (fun rule -> rule.Symbol = Epsilon && rule.FromStateID = t) nlfa.Rules))
        done
        !eCls

    let visitedNewStates = ref []
    let notVisitedNewStates = ref [eClosure nlfa.StartStates]
    let newRules = ref []

    while not (List.isEmpty !notVisitedNewStates) do
        let T = (!notVisitedNewStates).Head
        visitedNewStates :=  T :: !visitedNewStates
        notVisitedNewStates := (!notVisitedNewStates).Tail
        Set.iter
             (fun symbol ->
                let U = eClosure (move T symbol)
                if not (Set.isEmpty U)
                    then
                        if  not ((List.exists ((=)U) !notVisitedNewStates)  || (List.exists ((=)U) !visitedNewStates))
                        then 
                            notVisitedNewStates := U :: !notVisitedNewStates
                        newRules := (T,symbol,U)::!newRules)
             symbols
    done
    !newRules, !eLinesSet