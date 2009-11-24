(** Module IL
 *
 *  Author: Jk
 *)

#light "off"
module Yard.Core.IL
module Source = 
  begin
   type t = string * (int * int) 
   
   let toString ((r,_):t):string = r

  end
  
module Production = 
  begin
    type elem<'patt,'expr> = 
    {
     omit:bool; //Вычитание
     rule:(t<'patt,'expr>);//Правило
     binding:'patt option; //замыкание :) f:F ну или f:=F...
     checker:'expr option //"почти" резольвер
    }   
    and
    t<'patt,'expr> =    
    |PAlt     of (t<'patt,'expr>) * (t<'patt,'expr>)//Альтернатива
    |PSeq     of (elem<'patt,'expr>) list * 'expr option //Последовательность * атрибут.(атрибут всегда применяется к последовательности) 
    |PToken   of Source.t //собственно токен
    |PRef     of Source.t * 'expr option // Vanilla rule reference with an optional args list.
    |PMany    of (t<'patt,'expr>) //expr*
    |PMetaRef of Source.t * 'expr option * 'expr list // Metarule reference like in "a: mr<x> y z"
    |PLiteral of Source.t //Литерал. Хочется в правилах явно писать ,например, .."if" expr "then" expr...
    |PRepet   of (t<'patt,'expr>) * int option * int option  //extended regexp repetition, "man egrep" for details
    |PPerm    of (t<'patt,'expr>) list //permutation (A || B || C)
    
/// The following are obsolete and reduction to PRepet should be discussed.
    |PSome    of (t<'patt,'expr>) //expr+
    |POpt     of (t<'patt,'expr>) //expr?

  end

module Rule = 
  begin
   type t<'patt,'expr> = 
   { 
    name    : string;
    args    : 'patt list;
    body    : (Production.t<'patt,'expr>);
    _public : bool; //Стартовый нетерминал (иногда их хочется иметь много...)
    metaArgs: 'patt list
   }
  end


module Grammar = 
  begin
    type t<'patt,'expr> = (Rule.t<'patt,'expr>) list //грамматика - список правил.
  end 

module Definition = 
  begin
    type info = { fileName: string};
    type ('patt,'expr) t = 
    { 
     info : info;
     head    :'expr option; //текст до грамматики, который потом просто копируеться(всякие open-ы)
     grammar : Grammar.t<'patt,'expr>;//грамматика
     foot    :'expr option //текст после грамматики
    }    
    
    let empty = { info = {fileName = ""}; head = None; foot = None; grammar = []}
end