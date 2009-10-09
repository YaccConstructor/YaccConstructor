// Runner.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


#light
open Yard.Core
open Lexeme.Lexeme
open Tree

let (get_next_ch:int->t<_>),input_length =       
      let lex_list = //[{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
                     //[{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
                     [{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "LEFT";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "RIGHT";value = 1};{name = "$";value = 1}]
      let l = List.length lex_list 
      let get i =  List.nth (lex_list) (l-i)        
      let input_length () = l 
      get,input_length           

let  runTest testFilePath = 
    let commandLineArgs = System.Environment.GetCommandLineArgs()        
    Generator.generate (Main.ParseFile testFilePath)
    let tables = new Tables(testFilePath)
    let parser = new TableInterpretator(tables)
    let result = parser.Run get_next_ch (input_length())
    let trees = Set.of_list(List.concat(Set.map(fun ((a,b),i) -> b) result));
    Seq.iter(fun b -> print_tree b) trees;
    printfn "Parser get %A dirivation tree" trees.Count
    
do runTest @"..\..\..\..\Tests\test010.yrd"    
