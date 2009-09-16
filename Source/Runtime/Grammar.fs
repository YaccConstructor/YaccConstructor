// Grammar.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light "off"

module Symbol = //Терминал или нетерминал 
  begin
   type t<'a> = 
    |Terminal    of 'a
    |Nonterminal of 'a 
  end  
  
module Item = 
  begin   
   type t<'a> = {prod_num:int;
                 prod_name:string;
                 item_num:int;
                 symb:Symbol.t<'a> option;
                 next_num:int option;
                 s:int;
                 f:int Set}
  end  

