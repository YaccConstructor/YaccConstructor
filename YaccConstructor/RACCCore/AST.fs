//  AST.fs contains description of AST built with RACC
//
//  Copyright 2009,2010 Semen Grigorev <rsdpisuy@gmail.com>
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

module Yard.Generators.RACCGenerator.AST

open Yard.Core.CompareHelper


[<CustomEquality; CustomComparison>] 
type _value<'lexeme,'nodeVal when 'lexeme : equality and 'lexeme : comparison> = 
| LeafV of Lexeme<'lexeme> 
| NodeV of 'nodeVal    
    member self.GetValue x = 
        match x with 
        | LeafV(y) -> Some(y)
        | NodeV(y) -> None 
           
    override self.ToString() = 
        match self with 
        | NodeV(x) -> x.ToString()
        | LeafV(x) -> x.ToString()
      
    override self.Equals y = equalsOn self.GetValue self y
    override self.GetHashCode() = hashOn self.GetValue self 
    interface System.Collections.IStructuralComparable with      
        member self.CompareTo (y,c) = c.Compare(self.GetValue self ,self.GetValue (y :?> _value<'lexeme,'nodeVal>))
     
//[<CustomEquality; CustomComparison>]          
type value<'lexeme,'b, 'trace, 'id when 'lexeme : equality and 'lexeme : comparison and 'trace : equality> = 
    {
        id      : 'id;
        trace   : 'trace 
        value   : _value<'lexeme,'b>;    
    }

    member self.GetValue x = 
        x.trace,x.id 
    //override self.Equals y = equalsOn self.GetValue self y
    //override self.GetHashCode() = hashOn self.GetValue self 
    //interface System.Collections.IStructuralComparable with      
      //  member self.CompareTo (y,c) = c.Compare(self.GetValue self ,self.GetValue (y :?> value<'lexeme,'b, 'trace, 'id>))


type AST<'lexeme, 'nodeVal, 'trace, 'nodeId 
        when 'lexeme : equality 
        and 'lexeme : comparison
        and 'trace : equality> =
         
        | Node  of   List<AST<'lexeme, 'nodeVal, 'trace, 'nodeId>>
                   * int
                   * value<'lexeme, 'nodeVal, 'trace, 'nodeId>
        | Leaf  of int * value<'lexeme, 'nodeVal, 'trace, 'nodeId>
             
let rec dumpTree i item =
    let iter i = String.replicate i "    "
    match item with
    | Node (lst,name,value) -> 
        let trace = value.trace.ToString()                    
        let s = value.id.ToString() + ";" + value.value.ToString()
        [iter i;"<NODE name=\""; string name; "\" value=\""; s; "\" trace=\""; trace ; "\">\n"]
        @(List.map (dumpTree (i+1)) lst)
        @[iter i;"</NODE>\n"]
        |> String.concat "" 

    | Leaf (name,value)     ->
        String.concat "" [iter i;"<LEAF name=\""; string name; "\" value=\""; (value.value.GetValue (value.value)).Value.value.ToString(); "\"/>\n"]        

let PrintTree tree = System.Console.WriteLine (dumpTree 0 tree)
