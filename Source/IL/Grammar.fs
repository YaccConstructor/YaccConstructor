#light "off"

module Terminal = 
  struct
   type t<'a> = 'a
  end
  
module Nonterminal = 
  struct
   type t<'a> = 'a
  end

module Symbol = //Терминал или нетерминал 
  struct
   type t<'a> = 
    |Term    of Terminal.t<'a>
    |Nonterm of Nonterminal.t<'a> 
  end  

module Production = //нетерминал -> список Symbol 
  struct
   type t<'a,'b>  = Nonterminal.t<'a> * (Symbol.t<'b> list)
  end

