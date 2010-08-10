// Tester.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent

open Yard.Generators.RecursiveAscent
open Tables

type RACCTester() = class
    let  runTest testFilePath = 
        let commandLineArgs = System.Environment.GetCommandLineArgs()  
        //We have some problems with serialization/seserialization.      
        //let gotoSet,items,startNTerms,ruleToActionMap = Generator.generate (Main.ParseFile testFilePath)
        let tables = new TablesLoader(testFilePath, gotoSet,items,startNterms,ruleToActionMap)
        let parser = new Parser(tables)            
    
        let count = ref 0
    
        let testStreams = [Test.test024_b_0;Test.test024_b_1]//Test.testGen11 //[Test.test021_1]
                         //[Test.test018p_0;Test.test018p_1;Test.test018p_2;Test.test018p_3;Test.test018p_4;Test.test018p_5
                          // ;Test.test018p_6;Test.test018p_7;Test.test018p_8;Test.test018p_9;Test.test018p_10];
                          //[Test.test018pl_1;Test.test018pl_2;Test.test018pl_3;Test.test018pl_4;Test.test018pl_5
                           //;Test.test018pl_6;Test.test018pl_7;Test.test018pl_8;Test.test018pl_9;Test.test018pl_10;
                           //Test.test018pl_11;Test.test018pl_12;Test.test018pl_13;Test.test018pl_14;Test.test018pl_15
                           //;Test.test018pl_16;Test.test018pl_17;Test.test018pl_18;Test.test018pl_19;Test.test018pl_20]
                           //[Test.test021_0;Test.test021_1;Test.test021_2;Test.test021_3;Test.test021_4;Test.test021_5
                           // ;Test.test021_6;Test.test021_7;Test.test021_8;Test.test021_9;Test.test021_10]
        List.iter  
           (fun inputStream -> 
              let startTime = System.DateTime.Now
              //now we have not lexer. Lists from Test module are emulation of input stream
              //let inputStream = Test.test018p_0
              let trees = parser.Run inputStream 
    //#if DEBUG           
              Seq.iter(fun b -> AST.PrintTree b) trees
              if List.isEmpty trees then printfn "Input string is not correct.\n"
    //#endif    
              //printfn "Parser get %A dirivation tree" trees.Length    
              //let astInterp = new ASTInterpretator(tables)    
              //let res_2 = List.map astInterp.Interp trees
              //List.iter (printf "\nResult: %A") res_2
              //printfn "\n Total time = %A " (System.DateTime.Now.t - startTime)
              //printfn  "\n %A ; %A" (!count) (parser.ClimbVisitsCount + parser.ParseVisitsCount) //((System.DateTime.Now - startTime).TotalMilliseconds)
              incr count
              )
           testStreams
    
        
    //do runTest @"..\..\..\..\Tests\test024.yrd"
    member self.RunTest testFilePath = runTest testFilePath
end