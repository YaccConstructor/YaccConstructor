// Grammar.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Core.Grammar
module Item = begin   
   
   type t<'a> = 
       {prod_num:int;
        prod_name:string;
        item_num:int;
        symb: Option<'a>;
        next_num: Option<int>;
        seq_number:int;
        s:int;
        f: Set<int>;
        fromStateTrace: List<Yard.Core.Trace>;
        toStateTrace: List<Yard.Core.Trace>}
         override self.ToString() = 
              "production number: " + (self.prod_num.ToString()) + "\n"
            + "production name  : " + (self.prod_name) + "\n"
            + "item number      : " + (self.item_num.ToString()) + "\n"
            + "symbol           : " + (self.symb.ToString()) + "\n"
            + "next number      : " + (if self.next_num.IsSome then  self.next_num.ToString() else "null") + "\n"
            + "start state      : " + (self.s.ToString()) + "\n"
            + "finale states    : " + (self.s.ToString()) + "\n"
            + "trace for start state: " + (self.fromStateTrace.ToString()) + "\n"
            + "trace for start state: " + (self.toStateTrace.ToString()) + "\n"
  end  

