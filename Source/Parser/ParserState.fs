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
 override self.ToString() = 
  String.concat "" [  "Item: "; self.item.ToString();"\n";"Trees"
                    ; String.concat "\n" (List.map (AST.dump_tree 0) self.trees);"\n"]  
 override self.Equals y = equalsOn State<_,_,_>.Item self y
 override self.GetHashCode() = hashOn State<_,_,_>.Item self 
 interface System.IComparable with
      member self.CompareTo y = compareOn State<_,_,_>.Item self y
 new (item,trees) = {item=item;trees = trees}     
end

type ParserState<'symb,'leafVal,'nodeVal when 'symb : equality and 'symb : comparison> = struct
 val position : int;
 val symbol : string;
 val states : State<'symb,'leafVal,'nodeVal> Set;  
 new (states,symbol,position) = {states=states;symbol=symbol;position=position}
end