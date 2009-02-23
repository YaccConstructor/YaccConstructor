#light "off"

open IL
open Rule
open Production

let  getText = TransformAux.getText
let  indent n = new string(' ', 2 * n)
let  src2text = function |Some (x)-> getText x |None   -> "*NONE*"


let rec elem2xml n (elem:Production.elem<Source.t,Source.t>) =
  let bindingText = src2text elem.binding in 
    sprintf "%s<Elem omit=%A binding=%s>\n%s%s</Elem>\n" 
    (indent n)  elem.omit bindingText 
       (production2xml (n+1) elem.rule) 
    (indent n) 
    
    
and production2xml n (*(prod:Production.t<t,t>)*) = function
    |PAlt (a,b)      -> indent n + "<Alt>" 
                        + production2xml (n+1) a
                        + production2xml (n+1) b
                        + indent n + "</Alt>"
                        
    |PSeq (a,b)      -> indent n + "<Seq attr = " + src2text b 
                        +  String.concat "" (List.map (elem2xml (n+1)) a)
                        +  indent n + "</Seq>"
    |PToken(a)       -> indent n + "<Token name = "  + getText a + "/>"
    |PRef (a,b)      -> indent n + "<PRef name = "  + getText a + " attr="  
                        +  src2text b + "/>"
    |PMany (a)       -> indent n + "<Many>" + production2xml (n+1) a 
                        + indent n + "</Many>"
    |PMetaRef (a,b,c)-> indent n  + "<PMetaRef name = "  + getText a 
                        + " attr=" + src2text b 
                        + indent n  + "</PMetaRef>"
    |PSome (a)       -> indent n + "<Some>" + production2xml (n+1) a 
                        + indent n + "</Some>"
    |PLiteral (a)    -> indent n + "<Literal name = " + getText a + "\>"
    | _other         -> invalid_arg "prodution2xml"
    
let rule2xml n (rule:Rule.t<Source.t,Source.t>) = 
    indent n + "<Rule name=" + rule.name + ">"
    + production2xml (n+1) rule.body
    + indent n + "</Rule>"