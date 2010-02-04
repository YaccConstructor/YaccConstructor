// Grammar.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Core.Grammar
module Item = begin   
   type t<'a> = {prod_num:int;
                 prod_name:string;
                 item_num:int;
                 symb:'a option;
                 next_num:int option;
                 seq_number:int;
                 s:int;
                 f:int Set}
  end  

