// Runner.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


#light
open Yard.Core
open Tree

let  runTest testFilePath = 
    let commandLineArgs = System.Environment.GetCommandLineArgs()        
    Generator.generate (Main.ParseFile testFilePath)
    let tables = new Tables(testFilePath)
    let parser = new Parser(tables)
    let result = parser.Run Tests.test1
    let trees = List.concat(Set.map(fun ((a,b),i) -> b) result)
    Seq.iter(fun b -> print_tree b) trees
    printfn "Parser get %A dirivation tree" trees.Length
    
do runTest @"..\..\..\..\Tests\test013.yrd"