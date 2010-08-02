// FinitAutomata.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent

open Yard.Core.CompareHelper


[<Struct;CustomEquality; CustomComparison>]
type FAState = 
   val num : int
   
   new (number) = {num = number}
   
   member self.GetValue (x:FAState) = x.num
   
   override self.Equals y = equalsOn self.GetValue self y
   override self.GetHashCode() = hashOn self.GetValue self 
   interface System.Collections.IStructuralComparable with      
      member self.CompareTo (y,c) = c.Compare(self.GetValue self ,self.GetValue (y :?> FAState))

[<Struct>]
type FinitAutomata<'a,'b> =
 val rules : List<FAState*Option<'a>*List<Trace>*FAState>
 val startState : FAState
 val finaleState : 'b
 new (rules,startState,finaleState) = {rules=rules; startState=startState; finaleState=finaleState}
 
[<Struct>]
type CreatorResult<'a,'b> =
  val automata : FinitAutomata<'a,'b>
  val actionCode : string
  val bindings : List<string>
  new (automata,actionCode,bindings) = {automata=automata; actionCode=actionCode; bindings=bindings}
 
