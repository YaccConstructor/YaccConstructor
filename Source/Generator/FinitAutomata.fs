// FinitAutomata.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

[<Struct>]
type FAState = 
   val num : int
   val trace : List<Trace>
   new (number,trace) = {num = number; trace = trace}

[<Struct>]
type FinitAutomata<'a,'b> =
 val rules : List<FAState*Option<'a>*FAState>
 val startState : FAState
 val finaleState : 'b
 new (rules,startState,finaleState) = {rules=rules; startState=startState; finaleState=finaleState}
 
[<Struct>]
type CreatorResult<'a,'b> =
  val automata : FinitAutomata<'a,'b>
  val actionCode : string
  val bindings : List<string>
  new (automata,actionCode,bindings) = {automata=automata; actionCode=actionCode; bindings=bindings}
 
