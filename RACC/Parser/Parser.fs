// Parser.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core
open Lexeme
open Microsoft.FSharp.Compiler.CodeDom.Internal
open Microsoft.FSharp.Compiler.CodeDom

type Parser(tables: TablesLoader) = class
    
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
        let tableInterp = new TableInterpretator(tables,get_next_ch)
        let result = tableInterp.Run (input_length())
        let getForest (parserResult:ParserResult<_,_,_>) = parserResult.state.trees
        List.concat(List.map getForest (List.ofSeq result))

    member self.Run input = run input

end