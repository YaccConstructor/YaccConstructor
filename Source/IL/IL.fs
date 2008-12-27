#light "off"

module Source = 
  struct
   type t = string * (int * int) 
   
   let toString ((r,_):t):string = r

  end

module Production = 
  struct
    type elem <'patt,'expr> = 
    {
     omit:bool; //Вычитание
     rule:(t<'patt,'expr>);//Правило
     binding:'patt option; //замыкание :) f:F ну или f:=F...
     checker:'expr option //"почти" резольвер
    }   
    and
    t <'patt,'expr> =    
    |PAlt     of (t <'patt,'expr>) * (t<'patt,'expr>)//Альтернатива
    |PSeq     of (elem <'patt,'expr>) list * 'expr option //Последовательность * атрибут.(атрибут всегда применяется к последовательности) 
    |PToken   of Source.t //собственно токен
    |PRef     of Source.t * 'expr option 
    |PMany    of (t <'patt,'expr>) //expr*
    |PMetaRef of Source.t * 'expr option * 'expr list
    |PSome    of (t <'patt,'expr>) //expr+
    |POpt     of (t <'patt,'expr>) //expr?
    |PLiteral of Source.t //Литерал. Хочется в правилах явно писать ,например, .."if" expr "then" expr...
    |POccur   of (t <'patt,'expr>)*int*int option  //я добавить расширенное рег. выражение (не A* а от скольки-то до стольки-то.)
    |PComb    of (t <'patt,'expr>) list //я добавил перестоновки (A|B|C) = перестановка A,B и C

  end

module Rule = 
  struct
    type t <'patt,'expr> = 
    { 
     name    : string;
     args    : 'patt list;
     body    : (Production.t <'patt,'expr>);
     _public : bool; //Стартовый нетерминал (иногда их хочется иметь много...)
     metaArgs: 'patt list
    }
  end


module Grammar = 
  struct
    type t <'patt,'expr> = (Rule.t<'patt,'expr>) list //грамматика - список правил.
  end 

module Definition = 
  struct
    type ('patt,'expr) t = 
    { 
     head    :'expr option; //текст до грамматики, который потом просто копируеться(всякие open-ы)
     grammar : Grammar.t<'patt,'expr>;//грамматика
     foot    :'expr option //текст после грамматики
    }
end