// AST.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

module Value = 
 begin 
  //type cobj =
   //with compare x y = 1
   
  type value<'a,'b> = 
    | LeafV of Lexeme.t<'a> 
    | NodeV of 'b
    override self.ToString() = match self with |NodeV(x) -> "null"|LeafV(x) -> x.value.ToString()
     
  type t<'a,'b> = {
    prodNum : int;
    seqNum  : int;
    varNum  : int;
    value   : value<'a,'b>;    
  }
end

module AST =
  begin
    type t<'a,'b> = 
         | Node  of (t<'a,'b> list)*string*Value.t<'a,'b>
         | Leaf  of string*Value.t<'a,'b>
             
    let rec dump_tree i item =
        let rec iter i = (function 0 -> "" | x -> ("    "+(iter (x-1))))i
        match item with
          Node (lst,name,value) -> 
                String.concat "" 
                              ([iter i;"<NODE name=\"";name;"\" seqNum=\"";value.seqNum.ToString();"\" value=\"";value.value.ToString();"\">\n"]
                               @(List.map (dump_tree (i+1)) lst)@[iter i;"</NODE>\n"])
        | Leaf (name,value)     -> 
               String.concat "" [iter i;"<LEAF name=\"";name;"\" seqNum=\"";value.seqNum.ToString();"\" value=\"";value.value.ToString();"\"/>\n"]        
        
    let print_tree tree = System.Console.WriteLine (dump_tree 0 tree)
 end   