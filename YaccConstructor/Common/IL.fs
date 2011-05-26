//  Copyright 2009-2011 Jake Kirilenko
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.



module Yard.Core.IL
module Source = 
  begin
   type t = string * (int * int) 
// TODO: make something with toString overriding of Source.t   
   let toString ((r,_):t):string = r

  end
  
module Production = begin
    type IRuleType = interface end
    type elem<'patt,'expr> = {
        omit:bool; //Вычитание - не включать в AST
        rule:(t<'patt,'expr>);//Правило
        binding:'patt option; //замыкание :) f:F ну или f:=F...
        checker:'expr option //"почти" резольвер
    }   
    and t<'patt,'expr> = 
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

module Rule = begin
   type t<'patt,'expr> = { 
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

module Definition = begin
    type info = { fileName: string}
    type t<'patt,'expr>  = { 
     info    : info;
     head    :'expr option; //текст до грамматики, который потом просто копируеться(всякие open-ы)
     grammar : Grammar.t<'patt,'expr>;//грамматика
     foot    :'expr option //текст после грамматики
    }    
    
    let empty = { info = {fileName = ""}; head = None; foot = None; grammar = []}
end