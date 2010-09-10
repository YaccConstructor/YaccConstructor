// LFA.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.
//
//Implementation of LFA

namespace  Yard.Generators.RACC

type DFASymbol<'symbolVal> = 
    | DSymbol of 'symbolVal
    | Dummy

type NFASymbol<'symbolVal> = 
    | Epsilon
    | NSymbol of 'symbolVal

type DLFAState<'stateVal> =
    | State of 'stateVal
    | DummyState

type Rule<'symbol, 'label> =
    {
        FromStateID : int
        Symbol      : 'symbol
        Label       : 'label
        ToStateID   : int
    }    

type NLFA<'stateVal, 'symbolVal, 'label when 'symbolVal: comparison and 'label: comparison> =
    {        
        NIDToStateMap : System.Collections.Generic.IDictionary<int, 'stateVal>
        NStartState   : int
        NFinaleStates : Set<int>
        NRules        : Set<Rule<NFASymbol<'symbolVal>, 'label>>
    }

type DLFA<'stateVal, 'symbolVal, 'label when 'symbolVal: comparison and 'label: comparison> =
    {
        DIDToStateMap : System.Collections.Generic.IDictionary<int, DLFAState<'stateVal>>
        DStartState   : int
        DFinaleStates : Set<int>
        DRules        : Set<Rule<DFASymbol<'symbolVal>, 'label>>
    }