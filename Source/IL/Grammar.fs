#light "off"

open Set

module Symbol = //Терминал или нетерминал 
  struct
   type t<'a> = 
    |Terminal    of 'a
    |Nonterminal of 'a 
  end  
  
module Item = 
  struct   
   type t<'a> = {prod_num:int;
                 prod_name:string;
                 item_num:int;
                 symb:Symbol.t<'a> option;
                 next_num:int option;
                 s:int;
                 f:int Set}
  end  

