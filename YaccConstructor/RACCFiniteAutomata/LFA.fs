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
    | Symbol of 'symbolVal
    | Dummy

type NFASymbol<'symbolVal> = 
    | Epsilon
    | Symbol of 'symbolVal

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
        IDToStateMap : System.Collections.Generic.IDictionary<int, 'stateVal>
        StartStates  : Set<int>
        FinaleStates : Set<int>
        Rules        : Set<Rule<NFASymbol<'symbolVal>, 'label>>
    }

type DLFA<'stateVal, 'symbolVal, 'label when 'symbolVal: comparison and 'label: comparison> =
    {
        IDToStateMap : System.Collections.Generic.IDictionary<int, DLFAState<'stateVal>>
        StartState   : int
        FinaleStates : Set<int>
        Rules        : Set<Rule<DFASymbol<'symbolVal>, 'label>>
    }