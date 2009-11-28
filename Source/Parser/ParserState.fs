// ParserState.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

open Yard.Core.CompareHelper

[<CustomEquality; CustomComparison>]
type State<'symb,'leafVal,'nodeVal  when 'symb : equality and 'symb : comparison > = struct
 val item : Grammar.Item.t<'symb> 
 val trees: AST.t<'leafVal,'nodeVal> list  
 static member Item (x:State<_,_,_>) = x.item 
 override x.Equals y = equalsOn State<_,_,_>.Item x y
 override x.GetHashCode() = hashOn State<_,_,_>.Item x 
 interface System.IComparable with
      member x.CompareTo y = compareOn State<_,_,_>.Item x y
 new (item,trees) = {item=item;trees = trees}     
end

type public ParserState<'symb,'leafVal,'nodeVal when 'symb : equality and 'symb : comparison> = struct
 val position : int;
 val symbol : string;
 val states : State<'symb,'leafVal,'nodeVal> Set; 
 new (states,symbol,position) = {states=states;symbol=symbol;position=position}
end