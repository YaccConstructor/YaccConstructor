// TableInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

type TableInterpreter(tables) =
    class
        let goto = ()
        let memoize = ()
        let parse (lexer:ILexer<_,_>) lexbuf  = (lexer).Next lexbuf
        let climb   = ()

        member self.Parse lexer lexbuf = parse lexer lexbuf
    end