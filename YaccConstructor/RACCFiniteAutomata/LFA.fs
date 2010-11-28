//  LFA.fs contains implementation of LFA
//
//  Copyright 2010 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.


namespace  Yard.Generators._RACCGenerator

open Yard.Generators._RACCGenerator

type DFASymbol<'symbolVal> = 
    | DSymbol of 'symbolVal
    | Dummy
    override self.ToString() = 
        match self with
        | Dummy       -> "Dummy"
        | DSymbol (s) -> "(DSymbol " + ToString.toString s + ")"

type NFASymbol<'symbolVal> = 
    | Epsilon
    | NSymbol of 'symbolVal
    override self.ToString() = 
        match self with
        | Epsilon     -> "Epsilon"
        | NSymbol (s) -> "(NSymbol " + s.ToString() + ")"

type DLFAState<'stateVal> =
    | State of 'stateVal
    | DummyState
    override self.ToString() = 
        match self with
        | DummyState -> "DummyState"
        | State (s)  -> "(State " + s.ToString() + ")"

type Rule<'symbol, 'label> =
    {
        FromStateID : int
        Symbol      : 'symbol
        Label       : 'label
        ToStateID   : int
    }    
    override self.ToString() = 
          "{ \n"
        + "   FromStateID = " + ToString.toString self.FromStateID + ";\n"
        + "   Symbol      = " + self.Symbol.ToString() + ";\n"
        + "   Label       = " + ToString.toString self.Label + ";\n"
        + "   ToStateID   = " + ToString.toString self.ToStateID + ";\n"
        + "}\n"    

type NLFA<'stateVal, 'symbolVal, 'label when 'symbolVal: comparison and 'label: comparison> =
    {        
        NIDToStateMap : System.Collections.Generic.IDictionary<int, 'stateVal>
        NStartState   : int
        NFinaleState  : int
        NRules        : Set<Rule<NFASymbol<'symbolVal>, 'label>>
    }
    override self.ToString() = 
          "{ \n"
        + "   NIDToStateMap = " + ToString.dictToString self.NIDToStateMap + ";\n"
        + "   NStartState   = " + self.NStartState.ToString() + ";\n"
        + "   NFinaleState  = " + self.NFinaleState.ToString() + ";\n"
        + "   NRules        = " + ToString.setToString self.NRules + ";\n"
        + "}\n"

type DLFA<'stateVal, 'symbolVal, 'label when 'symbolVal: comparison and 'label: comparison> =
    {
        DIDToStateMap : System.Collections.Generic.IDictionary<int, DLFAState<'stateVal>>
        DStartState   : int
        DFinaleStates : Set<int>
        DRules        : Set<Rule<DFASymbol<'symbolVal>, 'label>>
    }
    override self.ToString() = 
          "{ \n"
        + "   DIDToStateMap = " + ToString.dictToString self.DIDToStateMap + ";\n"
        + "   DStartState   = " + self.DStartState.ToString() + ";\n"
        + "   DFinaleStates = " + ToString.setToString self.DFinaleStates + ";\n"
        + "   DRules        = " + ToString.setToString self.DRules + ";\n"
        + "}\n"