// FinitAutomata.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

[<Struct>]
type FinitAutomata<'a,'b> =
 val rules : List<int*Option<'a>*int>
 val startState : int
 val finaleState : 'b
 new (rules,startState,finaleState) = {rules=rules;startState=startState;finaleState=finaleState}
 
[<Struct>]
type CreatorResult<'a,'b> =
  val automata : FinitAutomata<'a,'b>
  val actionCode : string
  val bindings : List<string>
  new (automata,actionCode,bindings) = {automata=automata;actionCode=actionCode;bindings=bindings}
 
