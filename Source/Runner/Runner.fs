// Runner.fs
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
    parser.Run() 
    
do runTest @"..\..\..\..\Tests\test002.yrd"    