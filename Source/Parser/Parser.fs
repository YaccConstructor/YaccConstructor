// Parser.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light 
namespace Yard.Core
open Lexeme.Lexeme
open Microsoft.FSharp.Compiler.CodeDom.Internal
open Microsoft.FSharp.Compiler.CodeDom

type Parser(tables: Tables) = class

    let cp = new FSharpCodeProvider()
    let expr = System.CodeDom.CodeExpression()
    //expr.UserData.Add("a","b")
    let cp2 = Microsoft.FSharp.Compiler.CodeDom.Internal.Generator.generateExpression(expr)
    
    let get_next_ch,input_length,init =       
      let lex_list = ref []
      let l = ref 0;
      let get i =  List.nth (!lex_list) (!l-i)        
      let input_length () = !l
      let init input =
          lex_list := input
          l:=List.length input
          
      get,input_length,init
          
    let run input =  
        init input
        let tableInterp = new TableInterpretator(tables)
        tableInterp.Run get_next_ch (input_length())

    member self.Run input = run input

end