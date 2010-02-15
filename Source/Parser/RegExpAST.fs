// RegExpAST.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Core.AST

type REAST<'a> = 
   | RESeq of List<REAST<'a>>
   | REClosure of REAST<'a>
   | REAlt of Option<REAST<'a>>*Option<REAST<'a>>
   | RELeaf of 'a
   
let createAST leafs = 
    let TAlt2REAlt tAlt = 
      match tAlt with
      | TAlt (position) ->
         match position with
         | First  -> 2
         | Second -> 1
      | _               -> failwith "It is not altenative."
    1  