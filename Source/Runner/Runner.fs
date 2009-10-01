// Runner.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


#light

let generate = 
    let commandLineArgs = System.Environment.GetCommandLineArgs()
    //Main.main;
    Generator.generate (Main.ParseFile @"..\..\..\..\Tests\test002.yrd")
//let generate = Generator.generate