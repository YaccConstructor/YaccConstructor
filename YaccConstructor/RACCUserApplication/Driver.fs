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
        buf : 'buf
        lexer : ILexer<'a,'b>
    }

let run path =
    let buf = run_common path 
    let l = UserLexer.Lexer()
    let tables =
        {
            gotoSet = (*dict*) gotoSet
            automataDict = autumataDict
            items = items
        }
    let tt = {buf=buf; lexer = l}
    //let ti  TableInterpreter. //= TableInterpreter()
    TableInterpreter.run l buf tables
    

    //let driver = Yard.Generators._RACCGenerator.CoreDriver(tables)
    //let forest = driver.Parse l buf

    //printfn "%A" (tt.lexer.Next( tt.buf))
    //printfn "%A" (tt.lexer.Next( tt.buf))
do run //@"W:\Users\gsv2\Diploma\trunk\YaccConstructor\RACCUserApplication\test"
    @"..\..\test"
System.Console.ReadLine();

