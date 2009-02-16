#light "off"

open IL
open Rule
open Production
open Source
open System
open Gen_fo_Transform

let rec mult n = 
    if n = 0 
    then "  "
    else String.concat "" ["  ";mult (n-1)]
    
let rec dumpElem n (elem:Production.elem<t,t>) = 
    Console.WriteLine(String.concat ""[mult n;"<Elem ommit = "; string (elem.omit) ; " binding = ";
                                       (match elem.binding 
                                        with
                                        |Some (x)-> getText x
                                        |None   -> "no attr");">\n"]);
    dumpProduction (n+1) (elem.rule);
    Console.WriteLine(String.concat ""[mult n;"<Elem\>\n"]);
    
    
and dumpProduction n (prod:Production.t<t,t>) =
    match prod
    with
    |PAlt (a,b)      -> Console.WriteLine(String.concat ""[mult n;"<Alt>"]);
                        dumpProduction (n+1) a;
                        dumpProduction (n+1) b;
                        Console.WriteLine(String.concat ""[mult n;"<\Alt>"])
                        
    |PSeq (a,b)      -> Console.WriteLine(String.concat ""[mult n;"<Seq attr = ";
                                                           (match b 
                                                            with
                                                            |Some (x)-> getText x
                                                            | None   -> "no attr");">"]);
                        List.iter (dumpElem(n+1))a;                                    
                        Console.WriteLine(String.concat ""[mult n;"<\Seq>"])
    |PToken(a)       -> Console.WriteLine(String.concat ""[mult n;"<Token name = "; getText a;"\>"])
    |PRef (a,b)      -> Console.WriteLine(String.concat ""[mult n;"<PRef name = "; getText a;" attr=";
                                                           (match b 
                                                            with
                                                            |Some (x)-> getText x
                                                            | None   -> "no attr");"\>"])
    |PMany (a)   -> Console.WriteLine(String.concat ""[mult n;"<Many>"]);
                        dumpProduction (n+1) a;
                        Console.WriteLine(String.concat ""[mult n;"<\Many>"])
    |PMetaRef (a,b,c) -> Console.WriteLine(String.concat ""[mult n;"<PMetaRef name = "; getText a;" attr=";
                                                           (match b 
                                                            with
                                                            |Some (x)-> getText x
                                                            | None   -> "no attr");"\>"]);
                         Console.WriteLine(String.concat ""[mult n;"<\PMetaRef>"])//of Source.t * 'expr option * 'expr list
    |PSome (a)   -> Console.WriteLine(String.concat ""[mult n;"<Some>"]);
                        dumpProduction (n+1) a;
                        Console.WriteLine(String.concat ""[mult n;"<\Some>"])
    //|POpt     of (t <'patt,'expr>) //expr?
    |PLiteral (a)-> Console.WriteLine(String.concat ""[mult n;"<Literal name = "; getText a;"\>"])
    //|POccur   of (t <'patt,'expr>)*int*int option  //я добавить расширенное рег. выражение (не A* а от скольки-то до стольки-то.)
    //|PComb 
    //| _  ->  Console.WriteLine(String.concat ""[mult n;"<Other\>"])
    
let dumpRule n (rule:Rule.t<Source.t,Source.t>) = 
    Console.WriteLine(String.concat ""[mult n;"<Rule name = "; rule.name ;">"]);
    dumpProduction (n+1) (rule.body);
    Console.WriteLine(String.concat ""[mult n;"<Rule\>"]);
    //Console.WriteLine()