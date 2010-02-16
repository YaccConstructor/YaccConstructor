// RegExpAST.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Core.REAST

open Yard.Core.AST
open Yard.Core

type REAST<'a> = 
   | RESeq of List<REAST<'a>>
   | REClosure of List<REAST<'a>>
   | REAlt of Option<REAST<'a>>*Option<REAST<'a>>
   | RELeaf of 'a
   override self.ToString() = 
    match self with
    |RESeq(lst)     ->  "<Seq>\n"
                       + String.concat "\n    " (List.map (fun x -> (x.ToString())) lst)
                       + "</Seq>\n"
    |REClosure(lst) -> "<Closure>\n"+"</Closure>\n"
    |REAlt(a,b)     -> "<Alt>\n"+"</Alt>\n" 
    |RELeaf(_val)   -> "<val = " + _val.ToString() + ">\n"
   
let createREAST leafs =
   let groupFunction x =
     match x with 
     | Leaf(_,_val) | Node(_,_,_val) -> if _val.trace.IsEmpty then TSeq else _val.trace.Head     
      
   let group lst = System.Linq.Enumerable.GroupBy(lst, fun x -> groupFunction x) 
        
   let groups = group leafs
   let removeTraceHead node =
       match node with
       | Leaf (str,_val) -> Leaf(str,{_val with trace = _val.trace.Tail})
       | Node (x,y,_val) -> Node(x,y,{_val with trace = _val.trace.Tail})
       
   let g2l group = [for x in group -> x]    
   let rec createRegExpASTNode leafsGroup =
      match leafsGroup with
      | hd::tl -> match hd with
                  | Leaf(_,_val)| Node (_,_,_val) -> 
                                      let trace = _val.trace
                                      match trace with
                                      | hd::tl -> match hd with
                                                  | TSeq               -> RESeq [for x in (group (List.map removeTraceHead leafsGroup)) -> createRegExpASTNode (g2l x)]
                                                  | TClosure           -> REClosure[for x in (group (List.map removeTraceHead leafsGroup)) -> createRegExpASTNode (g2l x)]
                                                  | TAlt(position)     -> 
                                                         match position with
                                                         | First  -> REAlt(Some((createRegExpASTNode (List.map removeTraceHead leafsGroup))),None)
                                                         | Second -> REAlt(None,Some(createRegExpASTNode (List.map removeTraceHead leafsGroup)))
                                      | []     -> RELeaf _val.value
                  //       -> failwith "Value cannot be Node in REAST creating"

      | []     -> failwith "Value cannot be Node in REAST creating"
         
   createRegExpASTNode leafs
   