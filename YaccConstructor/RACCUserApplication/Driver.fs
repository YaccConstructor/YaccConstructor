// Driver.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module m1

open Microsoft.FSharp.Text.Lexing
open Yard.Generators._RACCGenerator
open Yard.Generators._RACCGenerator.Tables
module Lexer = UserLexer

let run_common path = 
    let content = System.IO.File.ReadAllText(path)    
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

type t<'buf,'a,'b> = 
    {
        buf   : 'buf
        lexer : ILexer<'a>
    }

let run path =
    let buf = run_common path 
    let l = UserLexer.Lexer(buf)
    let tables =
        {
            gotoSet = gotoSet
            automataDict = autumataDict
            items = items
        }
    
    let trees = TableInterpreter.run l tables
    let r = 
        Seq.collect (fun e -> e.rItem.forest |> List.map (ASTInterpretator.interp RACC.Actions.ruleToAction)) trees
        
    printf "\nResult %A\n" r
    trees        
    
let main path = run path

