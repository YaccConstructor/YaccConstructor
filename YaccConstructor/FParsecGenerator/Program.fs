module Yard.Generators.FParsecGenerator.Program

open Yard.Core.IL.Production
open Yard.Core.IL.Rule
open Yard.Core.IL
open Yard.Generators.FParsecGenerator.WriteToFile

let repr = fst
let printArgs indent = List.map repr  >> String.concat " " >> (+) indent
let printBinding = function None -> "_" | Some patt -> repr patt
let printArg = function None -> "" | Some arg -> repr arg

    

let rec printBody indent body  =
    match body with
    |PAlt(a,b)  -> sprintf "(attempt (%s)) <|> (%s)" (printBody (indent) a) (printBody (indent) b) 
    |PSeq (elems,Some a) ->  
      match  List.rev elems with
        | [] -> sprintf "preturn %s" (repr a)
        | lastElem::otherElems -> 
            let lastRepr = sprintf "%s |>> fun (%s) -> (%s) " (printBody indent lastElem.rule) (printBinding lastElem.binding) (repr a)
            let list = List.fold (fun r e -> printElem indent e + ") -> (" + r + ")" ) (lastRepr  ) otherElems 
            sprintf "%s  " list 
    |PSeq(elems,None) -> 
      match List.rev elems with
        | [] -> "???" 
        | lastElem::otherElems ->
            let i =  ref elems.Length
            let lastRepr = sprintf "%s |>> fun (%s)  " (printBody indent lastElem.rule) 
                                                         (printBinding lastElem.binding + " as _" + (!i).ToString() ) 
                                                 
            let rec beg = List.fold (fun (l,r) e -> decr i; 
                                                    ((if e.omit then l else ("_" + (!i).ToString())::l),printElem indent e  + " as _" + (!i).ToString() + ") ->(" + r   ) )
                                                                                                  ((if lastElem.omit then [] else ["_" + (!i).ToString()]),lastRepr ) otherElems
       
            sprintf "%s -> (%s "  (snd beg) (String.concat "," <| fst beg) + String.replicate elems.Length ")"
    |PToken a ->  "Lexer.p" + Source.toString a
    |PRef (r,arg)->  sprintf "%s %s" (Source.toString r)  (printArg arg)
    |PMany a -> sprintf "many ( attempt(%s))" <| printBody (indent +  "") a
    |PMetaRef (a,b,c)->sprintf "%s %s %s" (Source.toString a) (printArgs " " c) ( printArg b)   
    |PLiteral a -> "Lexer.literal " +  Source.toString a  
//What about following items
    |PSome a -> sprintf "many1 ( attempt(%s))" <| printBody (indent +  "") a
    |POpt a -> sprintf "opt ( attempt(%s))" <| printBody (indent +  "") a
    | x -> failwith <| sprintf "Unsupported construct\n%A" x

and printElem indent e = sprintf "%s >>= fun (%s " (printBody indent e.rule) (printBinding e.binding )


let generate (input_grammar:Definition.t<Source.t,Source.t>) = 
    
    let header = printArg input_grammar.head 
    let functions = List.map (fun e -> (if e._public then "public " else "private " ) 
                                      + e.name + (printArgs " " e.metaArgs) + (printArgs " " e.args) + " = " 
                                      + printBody "" e.body ) 
                         input_grammar.grammar

    let res = "module test\n" + "\nopen FParsec.Primitives\n" + header + "let rec " + String.concat ( "\n\n and ") functions
  
    WriteFile  (System.IO.Path.ChangeExtension(input_grammar.info.fileName,".fs"),res)