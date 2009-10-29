// CodeGenerator.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

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
    List.map (fun (k:Option<_>) -> ((if k.IsNone then None else Some(IL.Source.toString k.Value)) ,"x"+(next()).ToString()))  bindings

let genBynding (bnd,var) code =
    match bnd with
    |Some(_bnd) -> "let (" + _bnd + ") = "+ (if String.trim [' ';'\n'] code <> "" then "\n(" + code + ")" else "") + var + "\nin \n"
    |None        -> "(" + code + ")" + var + "; \n"
     
let genSeq code bindingLst action=     
    let bnpl =  bindingLst 
    let _params = "(" + List.fold (fun buf (_,name) -> buf + "," + name) (snd (List.hd bnpl)) (List.tl bnpl) + ")"
    "fun" + _params + " -> " + code + action
//let genAlt       