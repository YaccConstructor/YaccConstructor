// AST.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent

open Yard.Core.CompareHelper

module Value = 
 begin 
  [<CustomEquality; CustomComparison>] 
  type value<'a,'b when 'a : equality and 'a : comparison> = 
    | LeafV of Lexeme<'a> 
    | NodeV of 'b
    
    member self.GetValue x = 
      match x with 
      | LeafV(a) -> Some(a)
      | NodeV(a) -> None 
           
    override self.ToString() = 
      match self with 
      | NodeV(x) -> "null"
      | LeafV(x) -> x.value.ToString()
      
    override self.Equals y = equalsOn self.GetValue self y
    override self.GetHashCode() = hashOn self.GetValue self 
    interface System.Collections.IStructuralComparable with      
      member self.CompareTo (y,c) = c.Compare(self.GetValue self ,self.GetValue (y :?> value<'a,'b>))
         
  type t<'a,'b when 'a : equality and 'a : comparison> = {
    prodNum : int;
    trace   : List<List<List<Trace>>>
    value   : value<'a,'b>;    
  }
end

module AST =
  begin    
    type t<'a,'b when 'a : equality and 'a : comparison> = 
         | Node  of (t<'a,'b> list) * string * Value.t<'a,'b>
         | Leaf  of string * Value.t<'a,'b>
             
    let rec dumpTree i item =
        let rec iter i = (function 0 -> "" | x -> ("    "+(iter (x-1))))i
        match item with
          Node (lst,name,value) -> 
                String.concat "" 
                              ([iter i;"<NODE name=\"";name;(*"\" trace=\"";String.concat "; " (List.map (fun x -> x.ToString()) value.trace);*)(*"\" value=\"";value.value.ToString();*)"\">\n"]
                               @(List.map (dumpTree (i+1)) lst)
                               @[iter i;"</NODE>\n"])
        | Leaf (name,value)     -> 
               String.concat "" [iter i;"<LEAF name=\"";name;(*"\" trace=\"";value.trace.ToString();*)(*"\" value=\"";value.value.ToString();*)"\"/>\n"]        
        
    let PrintTree tree = System.Console.WriteLine (dumpTree 0 tree)
 end   