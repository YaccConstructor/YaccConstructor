#light
module Yard.Core.CodeGenerator
open Utils

let genSome code bindings = 
    let param = 
        if bindings = []
        then "_"
        else "((" + (List.fold (fun prms prm -> prms + "),(" + prm + ")") "" bindings) + ")"
    "List.map (fun "+param+"->"+code+")"    

let genBindingMap bindings =
    List.map (fun (k:Option<_>) -> (k,if k.IsSome then "x"+(next()).ToString() else "_"))  bindings

let genBynding (bnd,var) code =
    match bnd with
    |Some(_bnd ) -> "let (" + _bnd + ") = /n(" + code + ")" + var + "/n in /n"
    |None        -> "(" + code + ")" + var + "; /n"
     
let genSeq codeLst bindingLst action= 
    let body = List.fold2 (fun buf bvm code -> buf + (genBynding bvm code)) "" bindingLst codeLst
    "fun TODO -> " + body + "/n in" + action
//let genAlt       