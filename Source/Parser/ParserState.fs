// ParserState.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

open Yard.Core.CompareHelper

[<Struct>]
type State<'symb,'leafVal,'nodeVal  when 'leafVal : equality and 'leafVal : comparison > =
 val item : Grammar.Item.t<'symb> 
 val trees: AST.t<'leafVal,'nodeVal> list  
 new (item,trees) = {item=item;trees = trees}     

[<StructuralComparison; StructuralEquality;Struct>]
type ParserState<'symb,'leafVal,'nodeVal 
                  when 'leafVal : equality and 'leafVal : comparison 
                  and 'symb : equality and 'symb : comparison> = 
 val position : int;
 val symbol : string;
 val states : State<'symb,'leafVal,'nodeVal> Set;  
 new (states,symbol,position) = {states=states;symbol=symbol;position=position}
