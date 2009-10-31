// AST.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


#light

module AST

module Value =
 begin 
  type value<'a> = 
    | LeafV of Lexeme.Lexeme.t<'a> 
    | NodeV
     
  type t<'a> = {
    prodNum : int;
    seqNum  : int;
    varNum  : int;
    value   : value<'a>;   
  }
end

type AST <'a> = 
     | Node  of (AST<'a> list)*string*Value.t<'a>
     | Leaf  of string*Value.t<'a>
         
let rec dump_tree i item =
    let rec iter i = (function 0 -> "" | x -> ("    "+(iter (x-1))))i
    match item with
      Node (lst,name,value) -> String.concat "" ([iter i;"<NODE name=\"";name;"\">\n"]@(List.map (dump_tree (i+1)) lst)@[iter i;"</NODE>\n"])
    | Leaf (name,value)     -> String.concat "" [iter i;"<LEAF name=\"";name;"\" />\n"]        
    
let print_tree tree = System.Console.WriteLine (dump_tree 0 tree)