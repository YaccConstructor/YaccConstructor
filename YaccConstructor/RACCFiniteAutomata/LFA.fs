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

type DFASymbol<'symbolVal> = Symbol of 'symbolVal

type NFASymbol<'symbolVal> = 
    | Epsilon
    | Symbol of 'symbolVal

type Rule<'symbol, 'label> =
    {
        FromStateID : int
        Symbol      : 'symbol
        Label       : 'label
        ToStateID   : int
    }    

type NLFA<'stateInfo, 'symbolVal, 'label> =
    {        
        IDToStateMap : System.Collections.Generic.IDictionary<int,'stateInfo>
        StartStates  : Set<int>
        FinaleStates : Set<int>
        Rules        : List<Rule<NFASymbol<'symbolVal>, 'label>>
    }

type DLFA<'stateInfo, 'symbolVal, 'label> =
    {
        IDToStateMap : System.Collections.Generic.IDictionary<int,'stateInfo>
        StartState   : int
        FinaleStates : Set<int>
        Rules        : List<Rule<DFASymbol<'symbolVal>, 'label>>
    }