// ParserState.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent

open Yard.Core.CompareHelper

[<Struct;CustomEquality; CustomComparison>]
type State<'symb,'leafVal,'nodeVal,'trace  when 'leafVal : equality and 'leafVal : comparison and 'symb : equality > =
 val item  : Grammar.Item.t<'symb> 
 val trace : List<'trace>
 val trees : AST.t<'leafVal,'nodeVal> list  
 new (item,trees,trace) = {item=item;trees = trees;trace=trace} 
 
 member self.GetValue (x:State<_,_,_,_>) = x.item,x.trees
 
 override self.Equals y = equalsOn self.GetValue self y
 override self.GetHashCode() = hashOn self.GetValue self 
 interface System.Collections.IStructuralComparable with      
    member self.CompareTo (y,c) = c.Compare(self.GetValue self ,self.GetValue (y :?> State<'symb,'leafVal,'nodeVal,'trace>))
 override self.ToString() = 
   //"item: \n" + (self.item.ToString()) + "\n"
   (*+*) "  trees: \n" + (String.concat "\n" (List.map (AST.dumpTree 2) self.trees))
 
[<Struct>]
type ParserState<'symb,'leafVal,'nodeVal, 'trace 
                  when 'leafVal : equality and 'leafVal : comparison 
                  and 'symb : equality and 'symb : comparison
                  and 'trace: comparison> = 
 val position : int;
 val symbol : string;
 val states : State<'symb,'leafVal,'nodeVal,'trace> Set;  
 new (states,symbol,position) = {states=states;symbol=symbol;position=position}
 override self.ToString() = 
   "position: " + (self.position.ToString()) + "\n"
   + "symbol: " + self.symbol + "\n"
   + "states: [" + (String.concat ";" (Set.map (fun x -> x.ToString()) self.states)) + "]\n"
 
