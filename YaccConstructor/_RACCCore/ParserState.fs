// ParserState.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

type ParserState<'state, 'value, 'stream, 'lb when 'state :comparison> =
    {
        statesSet : Set<'state>
        inpSymbol : Lexeme<'value>
        inpStream : 'stream
        lexer     : ILexer<'value,'lb>
    }

