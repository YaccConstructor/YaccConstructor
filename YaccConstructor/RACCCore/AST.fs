//  AST.fs contains description of AST built with RACC
//
//  Copyright 2009,2010,2011 Semen Grigorev <rsdpisuy@gmail.com>
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
type value<'lexeme,'nodeVal when 'lexeme : equality and 'lexeme : comparison> = 
| LeafV of ILexeme
| NodeV of 'nodeVal    
    member self.GetValue x = 
        match x with 
        | LeafV(y) -> Some(y.tag)
        | NodeV(y) -> None 
           
    override self.ToString() = 
        match self with 
        | NodeV(x) -> x.ToString()
        | LeafV(x) -> x.ToString()
      
    override self.Equals y = equalsOn self.GetValue self y
    override self.GetHashCode() = hashOn self.GetValue self 
    interface System.Collections.IStructuralComparable with      
        member self.CompareTo (y,c) = c.Compare(self.GetValue self ,self.GetValue (y :?> value<'lexeme,'nodeVal>))

type AST<'lexeme, 'nodeVal, 'trace 
        when 'lexeme : equality 
        and  'lexeme : comparison
        and  'trace  : equality> =
         
        | Node  of   List<AST<'lexeme, 'nodeVal, 'trace> ref>
                   * int
                   *'trace
                   * value<'lexeme, 'nodeVal>
        | Leaf  of int * value<'lexeme, 'nodeVal>
             
let rec dumpTree i item =
    let iter i = String.replicate i "    "
    match item with
    | Node (lst, tag, trace, value) -> 
        let trace = trace.ToString()                    
        let s = value.ToString()
        [iter i;"<NODE name=\""; string tag; "\" value=\""; s; "\" trace=\""; trace ; "\">\n"]
        @(List.map (fun x -> dumpTree (i+1) !x) lst)
        @[iter i;"</NODE>\n"]
        |> String.concat "" 

    | Leaf (tag, value)     ->
        String.concat "" [iter i;"<LEAF name=\""; string tag; "\"/>\n"]        

let PrintTree tree = System.Console.WriteLine (dumpTree 0 tree)