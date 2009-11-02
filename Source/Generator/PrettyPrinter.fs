// PrettyPrinter.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

module Yard.Core.PrettyPrinter

open IO
open IL.Production

let (out:(System.IO.StreamWriter) ref) = ref null

let print_str (str:string) = (!out).Write(str)

let print_str_n (str:string) = (!out).WriteLine(str)

let print_import import_name = print_str_n("open "+import_name)

let print_imports imports = List.iter print_import imports

let new_l _ = print_str_n ""

(*let x w = 
    let c = new System.Data.Linq.DBConvert.ChangeType<System.Object>()
    1
*)
let print_header file_name imports =     
    print_str_n "//Test comment";new_l();
    print_str_n "#light";new_l();
    print_str "module ";
    print_str_n file_name;new_l();       
    print_imports imports

let print_int (i:int) = (!out).Write(i) 

let print_source (str,(i1,i2)) = 
    print_str("(\""+str+"\",(");
    print_int i1;
    print_str",";
    print_int i2;
    print_str"))"

let generate_list list_name list_body iterator = 
    (!out).WriteLine("let " + list_name + " = [" + String.concat " ; " (iterator list_body) + "]")