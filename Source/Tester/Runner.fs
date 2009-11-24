// Tester.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


#light
open Yard.Core

let  runTest testFilePath = 
    let commandLineArgs = System.Environment.GetCommandLineArgs()        
    Generator.generate (Main.ParseFile testFilePath)
    let tables = new Tables(testFilePath)
    let parser = new Parser(tables)
    let result = parser.Run Test.test3_w_a_2
    let astInterp = new ASTInterpretator(tables)    
    let trees = List.concat(List.map(fun (parserResult:ParserResult<_,_,_>) -> parserResult.state.trees) (List.ofSeq result))
    Seq.iter(fun b -> AST.print_tree b) trees    
    printfn "Parser get %A dirivation tree" trees.Length;
    let res_2 = List.map astInterp.Interp trees
    List.iter (printf "\nresult: %A") res_2
    
do runTest @"..\..\..\..\Tests\test003.yrd"