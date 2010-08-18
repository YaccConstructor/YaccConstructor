// Grammar.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Generators.RecursiveAscent.Grammar

open Yard.Core.CompareHelper
open System.Runtime.Serialization 
open System.Reflection
open Microsoft.FSharp.Reflection
open Yard.Generators.RecursiveAscent.IO

module Item = begin 
  
   [<CustomEquality; CustomComparison>] 
   type t<'a when 'a : equality> = 
       {
        prod_num       : int;
        prod_name      : string;
        item_num       : int;
        symb           : Option<'a>;
        next_num       : Option<int>;
        seq_number     : int;
        s              : int;
        f              : Set<int>;
        fromStateTrace : List<List<Option<'a>>*List<List<Yard.Generators.RecursiveAscent.Trace>>>
        toStateTrace   : List<List<Option<'a>>*List<List<Yard.Generators.RecursiveAscent.Trace>>>}
                 
         member self.GetValue x = x.prod_num,x.item_num,x.symb,x.next_num
         member self.printTrace trace = 
                printList 
                    trace 
                    (fun (x,y) -> 
                          "("
                        + printList x (function |Some(x) -> " Some(" + toString x + ")"
                                                     |None    -> "None")
                        + ","
                        + printList y (fun x -> printList x (fun y -> y.ToString()))
                        + ")"
                    )

         override self.Equals y = equalsOn self.GetValue self y                      
         override self.GetHashCode() = 
                  let h =  hashOn self.GetValue self 
                  h

         interface System.Collections.IStructuralComparable with      
           member self.CompareTo (y,c) = c.Compare(self.GetValue self , self.GetValue(y :?> t<'a>))

         override self.ToString() =
              "\n       {\n" 
            + "        prod_num       = " + (self.prod_num.ToString()) + ";\n"
            + "        prod_name      = \"" + (self.prod_name) + "\";\n"
            + "        item_num       = " + (self.item_num.ToString()) + ";\n"
            + "        symb           = " + (if self.symb.IsSome then  "Some(" + (IO.toString self.symb.Value) + ")" else "None") + ";\n"
            + "        next_num       = " + (if self.next_num.IsSome then  "Some("+self.next_num.Value.ToString()+")" else "None") + ";\n"
            + "        seq_number     = " + self.seq_number.ToString() + ";\n"
            + "        s              = " + (self.s.ToString()) + ";\n"
            + "        f              = " + printSet self.f (fun x -> x.ToString()) + ";\n"
            + "        fromStateTrace = " + (self.printTrace self.fromStateTrace) + ";\n"
            + "        toStateTrace   = " + (self.printTrace self.toStateTrace) + ";\n"
            + "       }"
  end  

