// Learn more about F# at http://fsharp.net

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
            let lastRepr = sprintf "%s |>> fun (%s) -> (%s)" (printBody indent lastElem.rule) (printBinding lastElem.binding) (repr a)
            List.fold (fun r e -> printElem indent e + "(" + r + ")") lastRepr otherElems 
//    |PSeq(elems,None) -> 
//      match List.rev elems with
//        | [] -> "???" 
//        | lastElem::otherElems ->
//            let i =  ref elems.Length
//            let lastRepr = sprintf "%s |>> fun (%s)" (printBody indent lastElem.rule) 
//                                                         (printBinding lastElem.binding + " as f" + (!i).ToString() )
//            let beg = List.fold (fun (l,r) e -> decr i; 
//                                                (("f" + (!i).ToString() )::l,printElem indent e  + " as f" + (!i).ToString() + " ->(" + r   ) )
//                                                                                                  (["f" + (!i).ToString()],lastRepr) otherElems
//         //   let list = List.map (fun u ->"f" + (!u).ToString() ) (fst beg) 
//            sprintf "%s -> (%s)"  (snd beg) (String.concat "," <| fst beg)
    |PToken a ->  "lexer.p" + Source.toString a
    |PRef (r,arg)->  sprintf "%s %s" (Source.toString r)  (printArg arg)
    |PMany a -> sprintf "many ( attempt(%s))" <| printBody (indent +  "") a
    |PMetaRef (a,b,c)->sprintf "%s %s %s" (Source.toString a) (printArgs " " c) ( printArg b)   
    |PLiteral a -> "literal_" + Source.toString a   
    | x -> failwith <| sprintf "Unsupported construct\n%A" x

and printElem indent e = sprintf "%s >>= fun (%s) -> " (printBody indent e.rule) (printBinding e.binding )


let generate (input_grammar:Definition.t<Source.t,Source.t>) = 
    
    let header = printArg input_grammar.head 
    let functions = List.map (fun e -> (if e._public then "public " else "private " ) 
                                      + e.name + (printArgs " " e.metaArgs) + (printArgs " " e.args) + " = " 
                                      + printBody "" e.body ) 
                         input_grammar.grammar

    let res = "module test\n" + "\nopen FParsec.Primitives\n" + header + "let rec " + String.concat ( "\n\n and ") functions
  
    WriteFile  (System.IO.Path.ChangeExtension(input_grammar.info.fileName,".fs"),res)