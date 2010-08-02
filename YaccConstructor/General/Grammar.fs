// Grammar.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Generators.RecursiveAscent.Grammar

open Yard.Core.CompareHelper

module Item = begin 
  
   [<CustomEquality; CustomComparison>] 
   type t<'a when 'a : equality> = 
       {prod_num:int;
        prod_name:string;
        item_num:int;
        symb: Option<'a>;
        next_num: Option<int>;
        seq_number:int;
        s:int;
        f: Set<int>;
        fromStateTrace: List<List<Option<'a>>*List<List<Yard.Generators.RecursiveAscent.Trace>>>
        toStateTrace: List<List<Option<'a>>*List<List<Yard.Generators.RecursiveAscent.Trace>>>}
         member self.GetValue x = x.prod_num,x.item_num,x.symb,x.next_num
         override self.Equals y = equalsOn self.GetValue self y
         override self.GetHashCode() = hashOn self.GetValue self 
         interface System.Collections.IStructuralComparable with      
           member self.CompareTo (y,c) = c.Compare(self.GetValue self , self.GetValue(y :?> t<'a>))
         override self.ToString() = 
              "    production number: " + (self.prod_num.ToString()) + "\n"
            + "    production name  : " + (self.prod_name) + "\n"
            + "    item number      : " + (self.item_num.ToString()) + "\n"
            + "    symbol           : " + (self.symb.ToString()) + "\n"
            + "    next number      : " + (if self.next_num.IsSome then  self.next_num.ToString() else "null") + "\n"
            + "    start state      : " + (self.s.ToString()) + "\n"
            + "    finale states    : " + (self.s.ToString()) + "\n"
            + "    trace for start state: " + (self.fromStateTrace.ToString()) + "\n"
            + "    trace for start state: " + (self.toStateTrace.ToString()) + "\n"
  end  

